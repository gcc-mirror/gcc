// expressions.cc -- Go frontend expression handling.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <algorithm>

#include "go-c.h"
#include "gogo.h"
#include "go-diagnostics.h"
#include "go-encode-id.h"
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

// A traversal used to set the location of subexpressions.

class Set_location : public Traverse
{
 public:
  Set_location(Location loc)
    : Traverse(traverse_expressions),
      loc_(loc)
  { }

  int
  expression(Expression** pexpr);

 private:
  Location loc_;
};

// Set the location of an expression.

int
Set_location::expression(Expression** pexpr)
{
  // Some expressions are shared or don't have an independent
  // location, so we shouldn't change their location.  This is the set
  // of expressions for which do_copy is just "return this" or
  // otherwise does not pass down the location.
  switch ((*pexpr)->classification())
    {
    case Expression::EXPRESSION_ERROR:
    case Expression::EXPRESSION_VAR_REFERENCE:
    case Expression::EXPRESSION_ENCLOSED_VAR_REFERENCE:
    case Expression::EXPRESSION_STRING:
    case Expression::EXPRESSION_FUNC_DESCRIPTOR:
    case Expression::EXPRESSION_TYPE:
    case Expression::EXPRESSION_BOOLEAN:
    case Expression::EXPRESSION_CONST_REFERENCE:
    case Expression::EXPRESSION_NIL:
    case Expression::EXPRESSION_TYPE_DESCRIPTOR:
    case Expression::EXPRESSION_GC_SYMBOL:
    case Expression::EXPRESSION_PTRMASK_SYMBOL:
    case Expression::EXPRESSION_TYPE_INFO:
    case Expression::EXPRESSION_STRUCT_FIELD_OFFSET:
      return TRAVERSE_CONTINUE;
    default:
      break;
    }

  (*pexpr)->location_ = this->loc_;
  return TRAVERSE_CONTINUE;
}

// Set the location of an expression and its subexpressions.

void
Expression::set_location(Location loc)
{
  this->location_ = loc;
  Set_location sl(loc);
  this->traverse_subexpressions(&sl);
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
Expression::do_export(Export_function_body*) const
{
  go_unreachable();
}

// Write a name to the export data.

void
Expression::export_name(Export_function_body* efb, const Named_object* no)
{
  if (no->package() != NULL)
    {
      char buf[50];
      snprintf(buf, sizeof buf, "<p%d>", efb->package_index(no->package()));
      efb->write_c_string(buf);
    }

  if (!Gogo::is_hidden_name(no->name()))
    efb->write_string(no->name());
  else
    {
      efb->write_c_string(".");
      efb->write_string(Gogo::unpack_hidden_name(no->name()));
    }
}

// Give an error saying that the value of the expression is not used.

void
Expression::unused_value_error()
{
  if (this->type()->is_error())
    {
      go_assert(saw_errors());
      this->set_is_error();
    }
  else
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
  go_error_at(this->location_, "%s", msg);
  this->set_is_error();
}

// A convenience function for handling a type in do_is_untyped.  If
// TYPE is not abstract, return false.  Otherwise set *PTYPE to TYPE
// and return true.

bool
Expression::is_untyped_type(Type* type, Type** ptype)
{
  if (!type->is_abstract())
    return false;
  *ptype = type;
  return true;
}

// Report whether this is a type expression.

bool
Expression::is_type_expression() const
{
  if (this->classification_ == EXPRESSION_TYPE)
    return true;
  if (this->unknown_expression() != NULL)
    {
      Named_object* no = this->unknown_expression()->named_object();
      if (no->is_unknown())
	{
	  no = no->unknown_value()->real_named_object();
	  if (no == NULL)
	    return false;
	}
      return no->is_type();
    }
  if (this->unary_expression() != NULL
      && this->unary_expression()->op() == OPERATOR_MULT
      && this->unary_expression()->operand()->is_type_expression())
    return true;
  return false;
}

// Set types of variables and constants.  This is implemented by the
// child class.

void
Expression::determine_type(Gogo* gogo, const Type_context* context)
{
  this->do_determine_type(gogo, context);
}

// Set types when there is no context.

void
Expression::determine_type_no_context(Gogo* gogo)
{
  Type_context context;
  this->do_determine_type(gogo, &context);
}

// Return true if two expressions refer to the same variable or struct
// field.  This can only be true when there are no side effects.

bool
Expression::is_same_variable(Expression* a, Expression* b)
{
  if (a->classification() != b->classification())
    return false;

  Var_expression* av = a->var_expression();
  if (av != NULL)
    return av->named_object() == b->var_expression()->named_object();

  Field_reference_expression* af = a->field_reference_expression();
  if (af != NULL)
    {
      Field_reference_expression* bf = b->field_reference_expression();
      return (af->field_index() == bf->field_index()
	      && Expression::is_same_variable(af->expr(), bf->expr()));
    }

  Unary_expression* au = a->unary_expression();
  if (au != NULL)
    {
      Unary_expression* bu = b->unary_expression();
      return (au->op() == OPERATOR_MULT
	      && bu->op() == OPERATOR_MULT
	      && Expression::is_same_variable(au->operand(),
					      bu->operand()));
    }

  Array_index_expression* aie = a->array_index_expression();
  if (aie != NULL)
    {
      Array_index_expression* bie = b->array_index_expression();
      return (aie->end() == NULL
	      && bie->end() == NULL
	      && Expression::is_same_variable(aie->array(), bie->array())
	      && Expression::is_same_variable(aie->start(), bie->start()));
    }

  Numeric_constant aval;
  if (a->numeric_constant_value(&aval))
    {
      Numeric_constant bval;
      if (b->numeric_constant_value(&bval))
	return aval.equals(bval);
    }

  return false;
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

  bool are_identical = Type::are_identical(lhs_type, rhs_type,
					   (Type::COMPARE_ERRORS
					    | Type::COMPARE_TAGS),
					   NULL);
  Expression* ret;
  if (!are_identical && lhs_type->interface_type() != NULL)
    {
      // Type to interface conversions have been made explicit early.
      go_assert(rhs_type->interface_type() != NULL);
      ret = Expression::convert_interface_to_interface(gogo, lhs_type, rhs,
						       false, location);
    }
  else if (!are_identical && rhs_type->interface_type() != NULL)
    ret = Expression::convert_interface_to_type(gogo, lhs_type, rhs, location);
  else if (lhs_type->is_slice_type() && rhs_type->is_nil_type())
    {
      // Assigning nil to a slice.
      Expression* nil = Expression::make_nil(location);
      Expression* zero = Expression::make_integer_ul(0, NULL, location);
      ret = Expression::make_slice_value(lhs_type, nil, zero, zero, location);
    }
  else if (rhs_type->is_nil_type())
    ret = Expression::make_nil(location);
  else if (are_identical)
    {
      if (lhs_type->forwarded() != rhs_type->forwarded())
	{
	  // Different but identical types require an explicit
	  // conversion.  This happens with type aliases.
	  return Expression::make_cast(lhs_type, rhs, location);
	}

      // No conversion is needed.
      return rhs;
    }
  else if (lhs_type->points_to() != NULL)
    ret = Expression::make_unsafe_cast(lhs_type, rhs, location);
  else if (lhs_type->is_numeric_type())
    ret = Expression::make_cast(lhs_type, rhs, location);
  else if ((lhs_type->struct_type() != NULL
            && rhs_type->struct_type() != NULL)
           || (lhs_type->array_type() != NULL
               && rhs_type->array_type() != NULL))
    {
      // This conversion must be permitted by Go, or we wouldn't have
      // gotten here.
      ret = Expression::make_unsafe_cast(lhs_type, rhs, location);
    }
  else
    return rhs;

  Type_context context(lhs_type, false);
  ret->determine_type(gogo, &context);
  return ret;
}

// Return an expression for a conversion from a non-interface type to an
// interface type.  If ON_STACK is true, it can allocate the storage on
// stack.

Expression*
Expression::convert_type_to_interface(Type* lhs_type, Expression* rhs,
                                      bool on_stack, Location location)
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
  if (!lhs_interface_type->implements_interface(rhs_type, NULL))
    {
      go_assert(saw_errors());
      return Expression::make_error(location);
    }

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
  if (rhs_type->is_direct_iface_type())
    {
      // We are assigning a pointer to the interface; the interface
      // holds the pointer itself.
      obj = unpack_direct_iface(rhs, location);
    }
  else
    {
      // We are assigning a non-pointer value to the interface; the
      // interface gets a copy of the value in the heap if it escapes.

      // An exception is &global if global is notinheap, which is a
      // pointer value but not a direct-iface type and we can't simply
      // take its address.
      bool is_address = (rhs->unary_expression() != NULL
                         && rhs->unary_expression()->op() == OPERATOR_AND);

      if (rhs->is_constant() && !is_address)
        obj = Expression::make_unary(OPERATOR_AND, rhs, location);
      else
        {
          obj = Expression::make_heap_expression(rhs, location);
          if (on_stack)
            obj->heap_expression()->set_allocate_on_stack();
        }
    }

  return Expression::make_interface_value(lhs_type, first_field, obj, location);
}

// Return an expression for the pointer-typed value of a direct interface
// type.  Specifically, for single field struct or array, get the single
// field, and do this recursively.  The reason for this is that we don't
// want to assign a struct or an array to a pointer-typed field.  The
// backend may not like that.

Expression*
Expression::unpack_direct_iface(Expression* rhs, Location loc)
{
  Struct_type* st = rhs->type()->struct_type();
  if (st != NULL)
    {
      go_assert(st->field_count() == 1);
      Expression* field = Expression::make_field_reference(rhs, 0, loc);
      return unpack_direct_iface(field, loc);
    }
  Array_type* at = rhs->type()->array_type();
  if (at != NULL)
    {
      int64_t len;
      bool ok = at->int_length(&len);
      go_assert(ok && len == 1);
      Type* int_type = Type::lookup_integer_type("int");
      Expression* index = Expression::make_integer_ul(0, int_type, loc);
      Expression* elem = Expression::make_array_index(rhs, index, NULL, NULL, loc);
      return unpack_direct_iface(elem, loc);
    }
  return rhs;
}

// The opposite of unpack_direct_iface.

Expression*
Expression::pack_direct_iface(Type* t, Expression* rhs, Location loc)
{
  if (rhs->type() == t)
    return rhs;
  Struct_type* st = t->struct_type();
  if (st != NULL)
    {
      Expression_list* vals = new Expression_list();
      vals->push_back(pack_direct_iface(st->field(0)->type(), rhs, loc));
      return Expression::make_struct_composite_literal(t, vals, loc);
    }
  Array_type* at = t->array_type();
  if (at != NULL)
    {
      Expression_list* vals = new Expression_list();
      vals->push_back(pack_direct_iface(at->element_type(), rhs, loc));
      return Expression::make_array_composite_literal(t, vals, loc);
    }
  return Expression::make_unsafe_cast(t, rhs, loc);
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
      Expression::make_dereference(mtable, NIL_CHECK_NOT_NEEDED, location);
  descriptor = Expression::make_field_reference(descriptor, 0, location);
  Expression* nil = Expression::make_nil(location);

  Expression* eq =
      Expression::make_binary(OPERATOR_EQEQ, mtable, nil, location);
  return Expression::make_conditional(eq, nil, descriptor, location);
}

// Return an expression for the conversion of an interface type to an
// interface type.

Expression*
Expression::convert_interface_to_interface(Gogo* gogo, Type *lhs_type,
					   Expression* rhs,
                                           bool for_type_guard,
                                           Location location)
{
  if (Type::are_identical(lhs_type, rhs->type(),
			  Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			  NULL))
    return rhs;

  Interface_type* lhs_interface_type = lhs_type->interface_type();
  bool lhs_is_empty = lhs_interface_type->is_empty();

  // In the general case this requires runtime examination of the type
  // method table to match it up with the interface methods.

  // FIXME: If all of the methods in the right hand side interface
  // also appear in the left hand side interface, then we don't need
  // to do a runtime check, although we still need to build a new
  // method table.

  // We are going to evaluate RHS multiple times.
  go_assert(rhs->is_multi_eval_safe());

  // Get the type descriptor for the right hand side.  This will be
  // NULL for a nil interface.
  Expression* rhs_type_expr = Expression::get_interface_type_descriptor(rhs);
  Expression* lhs_type_expr =
      Expression::make_type_descriptor(lhs_type, location);

  Expression* first_field;
  if (for_type_guard)
    {
      // A type assertion fails when converting a nil interface.
      first_field = Runtime::make_call(gogo, Runtime::ASSERTITAB, location, 2,
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
      first_field = Runtime::make_call(gogo, Runtime::REQUIREITAB, location, 2,
				       lhs_type_expr, rhs_type_expr);
    }

  // The second field is simply the object pointer.
  Expression* obj =
      Expression::make_interface_info(rhs, INTERFACE_INFO_OBJECT, location);
  Expression* ret = Expression::make_interface_value(lhs_type, first_field,
						     obj, location);
  Type_context context(lhs_type, false);
  ret->determine_type(gogo, &context);
  return ret;
}

// Return an expression for the conversion of an interface type to a
// non-interface type.

Expression*
Expression::convert_interface_to_type(Gogo* gogo, Type *lhs_type, Expression* rhs,
                                      Location location)
{
  // We are going to evaluate RHS multiple times.
  go_assert(rhs->is_multi_eval_safe());

  // Build an expression to check that the type is valid.  It will
  // panic with an appropriate runtime type error if the type is not
  // valid.
  // (lhs_type == rhs_type ? nil /*dummy*/ :
  //    panicdottype(lhs_type, rhs_type, inter_type))
  // For some Oses, we need to call runtime.eqtype instead of
  // lhs_type == rhs_type, as we may have unmerged type descriptors
  // from shared libraries.
  Expression* lhs_type_expr = Expression::make_type_descriptor(lhs_type,
                                                                location);
  Expression* rhs_descriptor =
      Expression::get_interface_type_descriptor(rhs);

  Type* rhs_type = rhs->type();
  Expression* rhs_inter_expr = Expression::make_type_descriptor(rhs_type,
                                                                location);

  Expression* cond;
  if (gogo->need_eqtype()) {
    cond = Runtime::make_call(gogo, Runtime::EQTYPE, location,
                              2, lhs_type_expr,
                              rhs_descriptor);
  } else {
    cond = Expression::make_binary(OPERATOR_EQEQ, lhs_type_expr,
                                   rhs_descriptor, location);
  }

  rhs_descriptor = Expression::get_interface_type_descriptor(rhs);
  Expression* panic = Runtime::make_call(gogo, Runtime::PANICDOTTYPE, location,
                                         3, lhs_type_expr->copy(),
                                         rhs_descriptor,
                                         rhs_inter_expr);
  Expression* nil = Expression::make_nil(location);
  Expression* check = Expression::make_conditional(cond, nil, panic,
                                                   location);

  // If the conversion succeeds, pull out the value.
  Expression* obj = Expression::make_interface_info(rhs, INTERFACE_INFO_OBJECT,
                                                    location);

  // If the value is a direct interface, then it is the value we want.
  // Otherwise it points to the value.
  if (lhs_type->is_direct_iface_type())
    obj = Expression::pack_direct_iface(lhs_type, obj, location);
  else
    {
      obj = Expression::make_unsafe_cast(Type::make_pointer_type(lhs_type), obj,
                                         location);
      obj = Expression::make_dereference(obj, NIL_CHECK_NOT_NEEDED,
                                         location);
    }
  return Expression::make_compound(check, obj, location);
}

// Convert an expression to its backend representation.  This is implemented by
// the child class.  Not that it is not in general safe to call this multiple
// times for a single expression, but that we don't catch such errors.

Bexpression*
Expression::get_backend(Translate_context* context)
{
  // The child may have marked this expression as having an error.
  if (this->classification_ == EXPRESSION_ERROR)
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

  return this->do_get_backend(context);
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
      mpc_t cval;
      if (!val->to_complex(&cval))
        {
          go_assert(saw_errors());
          return gogo->backend()->error_expression();
        }
      ret = gogo->backend()->complex_constant_expression(btype, cval);
      mpc_clear(cval);
    }
  else
    go_unreachable();

  return ret;
}

// Insert bounds checks for an index expression.  Check that that VAL
// >= 0 and that it fits in an int.  Then check that VAL OP BOUND is
// true.  If any condition is false, call one of the CODE runtime
// functions, which will panic.

void
Expression::check_bounds(Gogo* gogo, Expression* val, Operator op,
			 Expression* bound,
			 Runtime::Function code,
			 Runtime::Function code_u,
			 Runtime::Function code_extend,
			 Runtime::Function code_extend_u,
			 Statement_inserter* inserter,
			 Location loc)
{
  go_assert(val->is_multi_eval_safe());
  go_assert(bound->is_multi_eval_safe());

  Type* int_type = Type::lookup_integer_type("int");
  int int_type_size = int_type->integer_type()->bits();

  Type* val_type = val->type();
  if (val_type->integer_type() == NULL)
    {
      go_assert(saw_errors());
      return;
    }
  int val_type_size = val_type->integer_type()->bits();
  bool val_is_unsigned = val_type->integer_type()->is_unsigned();

  // Check that VAL >= 0.
  Expression* check = NULL;
  if (!val_is_unsigned)
    {
      Expression* zero = Expression::make_integer_ul(0, val_type, loc);
      check = Expression::make_binary(OPERATOR_GE, val->copy(), zero, loc);
    }

  // If VAL's type is larger than int, check that VAL fits in an int.
  if (val_type_size > int_type_size
      || (val_type_size == int_type_size
	  && val_is_unsigned))
    {
      mpz_t one;
      mpz_init_set_ui(one, 1UL);

      // maxval = 2^(int_type_size - 1) - 1
      mpz_t maxval;
      mpz_init(maxval);
      mpz_mul_2exp(maxval, one, int_type_size - 1);
      mpz_sub_ui(maxval, maxval, 1);
      Expression* max = Expression::make_integer_z(&maxval, val_type, loc);
      mpz_clear(one);
      mpz_clear(maxval);

      Expression* cmp = Expression::make_binary(OPERATOR_LE, val->copy(),
						max, loc);
      if (check == NULL)
	check = cmp;
      else
	check = Expression::make_binary(OPERATOR_ANDAND, check, cmp, loc);
    }

  // For the final check we can assume that VAL fits in an int.
  Expression* ival;
  if (val_type == int_type)
    ival = val->copy();
  else
    ival = Expression::make_cast(int_type, val->copy(), loc);

  // BOUND is assumed to fit in an int.  Either it comes from len or
  // cap, or it was checked by an earlier call.
  Expression* ibound;
  if (bound->type() == int_type)
    ibound = bound->copy();
  else
    ibound = Expression::make_cast(int_type, bound->copy(), loc);

  Expression* cmp = Expression::make_binary(op, ival, ibound, loc);
  if (check == NULL)
    check = cmp;
  else
    check = Expression::make_binary(OPERATOR_ANDAND, check, cmp, loc);

  Runtime::Function c;
  if (val_type_size > int_type_size)
    {
      if (val_is_unsigned)
	c = code_extend_u;
      else
	c = code_extend;
    }
  else
    {
      if (val_is_unsigned)
	c = code_u;
      else
	c = code;
    }

  Expression* ignore = Expression::make_boolean(true, loc);
  Expression* crash = Runtime::make_call(gogo, c, loc, 2,
					 val->copy(), bound->copy());
  Expression* cond = Expression::make_conditional(check, ignore, crash, loc);
  Statement* s = Statement::make_statement(cond, true);
  s->determine_types(gogo);
  inserter->insert(s);
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
  do_is_untyped(Type**) const
  { return false; }

  bool
  do_numeric_constant_value(Numeric_constant*)
  { return false; }

  bool
  do_discarding_value()
  { return true; }

  Type*
  do_type()
  { return Type::make_error_type(); }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  bool
  do_is_addressable() const
  { return true; }

  Bexpression*
  do_get_backend(Translate_context* context)
  { return context->backend()->error_expression(); }

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

class Type_expression : public Expression
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
  do_determine_type(Gogo*, const Type_context*)
  { }

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context*);

  void do_dump_expression(Ast_dump_context*) const;

 private:
  // The type which we are representing as an expression.
  Type* type_;
};

void
Type_expression::do_check_types(Gogo*)
{
  if (this->type_->is_error())
    {
      go_assert(saw_errors());
      this->set_is_error();
    }
}

Bexpression*
Type_expression::do_get_backend(Translate_context* context)
{
  if (!this->is_error_expression())
    this->report_error("invalid use of type");
  return context->backend()->error_expression();
}

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

// Class Var_expression.

// Lower a variable expression.  Here we just make sure that the
// initialization expression of the variable has been lowered.  This
// ensures that we will be able to determine the type of the variable
// if necessary.

Expression*
Var_expression::do_lower(Gogo* gogo, Named_object* function,
			 Statement_inserter* inserter)
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
Var_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  if (this->variable_->is_variable())
    this->variable_->var_value()->determine_type(gogo);
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

  if (this->variable_->is_variable()
      && this->variable_->var_value()->is_in_heap())
    {
      Node::make_node(this)->set_encoding(Node::ESCAPE_HEAP);
      Node::make_node(this->variable_)->set_encoding(Node::ESCAPE_HEAP);
    }
}

// Export a reference to a variable.

void
Var_expression::do_export(Export_function_body* efb) const
{
  Named_object* no = this->variable_;
  if (no->is_result_variable() || !no->var_value()->is_global())
    efb->write_string(Gogo::unpack_hidden_name(no->name()));
  else
    Expression::export_name(efb, no);
}

// Get the backend representation for a reference to a variable.

Bexpression*
Var_expression::do_get_backend(Translate_context* context)
{
  Bvariable* bvar = this->variable_->get_backend_variable(context->gogo(),
							  context->function());
  bool is_in_heap;
  Location loc = this->location();
  Btype* btype;
  Gogo* gogo = context->gogo();
  if (this->variable_->is_variable())
    {
      is_in_heap = this->variable_->var_value()->is_in_heap();
      btype = this->variable_->var_value()->type()->get_backend(gogo);
    }
  else if (this->variable_->is_result_variable())
    {
      is_in_heap = this->variable_->result_var_value()->is_in_heap();
      btype = this->variable_->result_var_value()->type()->get_backend(gogo);
    }
  else
    go_unreachable();

  Bexpression* ret =
      context->backend()->var_expression(bvar, loc);
  if (is_in_heap)
    ret = context->backend()->indirect_expression(btype, ret, true, loc);
  return ret;
}

// Ast dump for variable expression.

void
Var_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << this->variable_->message_name() ;
}

// Make a reference to a variable in an expression.

Expression*
Expression::make_var_reference(Named_object* var, Location location)
{
  if (var->is_sink())
    return Expression::make_sink(location);
  if (var->is_redefinition())
    return Expression::make_error(location);

  // FIXME: Creating a new object for each reference to a variable is
  // wasteful.
  return new Var_expression(var, location);
}

// Class Enclosed_var_expression.

int
Enclosed_var_expression::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Lower the reference to the enclosed variable.

Expression*
Enclosed_var_expression::do_lower(Gogo* gogo, Named_object* function,
				  Statement_inserter* inserter)
{
  gogo->lower_expression(function, inserter, &this->reference_);
  return this;
}

// Flatten the reference to the enclosed variable.

Expression*
Enclosed_var_expression::do_flatten(Gogo* gogo, Named_object* function,
				    Statement_inserter* inserter)
{
  gogo->flatten_expression(function, inserter, &this->reference_);
  return this;
}

void
Enclosed_var_expression::do_address_taken(bool escapes)
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

  if (this->variable_->is_variable()
      && this->variable_->var_value()->is_in_heap())
    Node::make_node(this->variable_)->set_encoding(Node::ESCAPE_HEAP);
}

// Ast dump for enclosed variable expression.

void
Enclosed_var_expression::do_dump_expression(Ast_dump_context* adc) const
{
  adc->ostream() << this->variable_->message_name();
}

// Make a reference to a variable within an enclosing function.

Expression*
Expression::make_enclosing_var_reference(Expression* reference,
					 Named_object* var, Location location)
{
  return new Enclosed_var_expression(reference, var, location);
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

// Export a reference to a temporary.

void
Temporary_reference_expression::do_export(Export_function_body* efb) const
{
  unsigned int idx = efb->temporary_index(this->statement_);
  char buf[50];
  snprintf(buf, sizeof buf, "$t%u", idx);
  efb->write_c_string(buf);
}

// Import a reference to a temporary.

Expression*
Temporary_reference_expression::do_import(Import_function_body* ifb,
					  Location loc)
{
  std::string id = ifb->read_identifier();
  go_assert(id[0] == '$' && id[1] == 't');
  const char *p = id.c_str();
  char *end;
  long idx = strtol(p + 2, &end, 10);
  if (*end != '\0' || idx > 0x7fffffff)
    {
      if (!ifb->saw_error())
	go_error_at(loc,
		    ("invalid export data for %qs: "
		     "invalid temporary reference index at %lu"),
		    ifb->name().c_str(),
		    static_cast<unsigned long>(ifb->off()));
      ifb->set_saw_error();
      return Expression::make_error(loc);
    }

  Temporary_statement* temp =
    ifb->temporary_statement(static_cast<unsigned int>(idx));
  if (temp == NULL)
    {
      if (!ifb->saw_error())
	go_error_at(loc,
		    ("invalid export data for %qs: "
		     "undefined temporary reference index at %lu"),
		    ifb->name().c_str(),
		    static_cast<unsigned long>(ifb->off()));
      ifb->set_saw_error();
      return Expression::make_error(loc);
    }

  return Expression::make_temporary_reference(temp, loc);
}

// Get a backend expression referring to the variable.

Bexpression*
Temporary_reference_expression::do_get_backend(Translate_context* context)
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
      && stype->points_to() != NULL
      && stype->points_to()->is_void_type())
    {
      Btype* btype = this->type()->base()->get_backend(gogo);
      ret = gogo->backend()->convert_expression(btype, ret, this->location());
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
  statement->add_use();
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
    Gogo* gogo,
    const Type_context* context)
{
  this->expr_->determine_type(gogo, context);
}

// Take the address.

void
Set_and_use_temporary_expression::do_address_taken(bool)
{
  this->statement_->set_is_address_taken();
}

// Return the backend representation.

Bexpression*
Set_and_use_temporary_expression::do_get_backend(Translate_context* context)
{
  Location loc = this->location();
  Gogo* gogo = context->gogo();
  Bvariable* bvar = this->statement_->get_backend_variable(context);
  Bexpression* lvar_ref = gogo->backend()->var_expression(bvar, loc);

  Named_object* fn = context->function();
  go_assert(fn != NULL);
  Bfunction* bfn = fn->func_value()->get_or_make_decl(gogo, fn);
  Bexpression* bexpr = this->expr_->get_backend(context);
  Bstatement* set = gogo->backend()->assignment_statement(bfn, lvar_ref,
                                                          bexpr, loc);
  Bexpression* var_ref = gogo->backend()->var_expression(bvar, loc);
  Bexpression* ret = gogo->backend()->compound_expression(set, var_ref, loc);
  return ret;
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
      type_(NULL), bvar_(NULL)
  { }

 protected:
  bool
  do_discarding_value()
  { return true; }

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*);

  Expression*
  do_copy()
  { return new Sink_expression(this->location()); }

  Bexpression*
  do_get_backend(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of this sink variable.
  Type* type_;
  // The temporary variable we generate.
  Bvariable* bvar_;
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
Sink_expression::do_determine_type(Gogo*, const Type_context* context)
{
  if (context->type != NULL)
    this->type_ = context->type;
}

// Return a temporary variable for a sink expression.  This will
// presumably be a write-only variable which the middle-end will drop.

Bexpression*
Sink_expression::do_get_backend(Translate_context* context)
{
  Location loc = this->location();
  Gogo* gogo = context->gogo();
  if (this->bvar_ == NULL)
    {
      if (this->type_ == NULL || this->type_->is_sink_type())
	{
	  go_assert(saw_errors());
	  return gogo->backend()->error_expression();
	}

      Named_object* fn = context->function();
      go_assert(fn != NULL);
      Bfunction* fn_ctx = fn->func_value()->get_or_make_decl(gogo, fn);
      Btype* bt = this->type_->get_backend(context->gogo());
      Bstatement* decl;
      this->bvar_ =
	gogo->backend()->temporary_variable(fn_ctx, context->bblock(), bt, NULL,
					    0, loc, &decl);
      Bexpression* var_ref =
          gogo->backend()->var_expression(this->bvar_, loc);
      var_ref = gogo->backend()->compound_expression(decl, var_ref, loc);
      return var_ref;
    }
  return gogo->backend()->var_expression(this->bvar_, loc);
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

// Get the backend representation for the code of a function expression.

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
      go_error_at(loc,
		  ("invalid use of special built-in function %qs; "
		   "must be called"),
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

// Get the backend representation for a function expression.  This is used when
// we take the address of a function rather than simply calling it.  A func
// value is represented as a pointer to a block of memory.  The first
// word of that memory is a pointer to the function code.  The
// remaining parts of that memory are the addresses of variables that
// the function closes over.

Bexpression*
Func_expression::do_get_backend(Translate_context* context)
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
	      go_error_at(this->location(),
			  ("invalid use of special built-in function %qs; "
			   "must be called"),
			  no->message_name().c_str());
	      return gogo->backend()->error_expression();
	    }
	  descriptor = no->func_declaration_value()->descriptor(gogo, no);
	}
      else
	go_unreachable();

      Bexpression* bdesc = descriptor->get_backend(context);
      return gogo->backend()->address_expression(bdesc, this->location());
    }

  go_assert(this->function_->func_value()->enclosing() != NULL);

  // If there is a closure, then the closure is itself the function
  // expression.  It is a pointer to a struct whose first field points
  // to the function code and whose remaining fields are the addresses
  // of the closed-over variables.
  Bexpression *bexpr = this->closure_->get_backend(context);

  // Introduce a backend type conversion, to account for any differences
  // between the argument type (function descriptor, struct with a
  // single field) and the closure (struct with multiple fields).
  Gogo* gogo = context->gogo();
  Btype *btype = this->type()->get_backend(gogo);
  return gogo->backend()->convert_expression(btype, bexpr, this->location());
}

// The cost of inlining a function reference.

int
Func_expression::do_inlining_cost() const
{
  // FIXME: We don't inline references to nested functions.
  if (this->closure_ != NULL)
    return 0x100000;
  if (this->function_->is_function()
      && this->function_->func_value()->enclosing() != NULL)
    return 0x100000;

  return 1;
}

// Export a reference to a function.

void
Func_expression::do_export(Export_function_body* efb) const
{
  Expression::export_name(efb, this->function_);
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
  Func_expression* fe = new Func_expression(function, closure, location);

  // Detect references to builtin functions and set the runtime code if
  // appropriate.
  if (function->is_function_declaration())
    fe->set_runtime_code(Runtime::name_to_code(function->name()));
  return fe;
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
  Type* struct_type = Type::make_builtin_struct_type(1, "fn", uintptr_type);
  Func_descriptor_expression::descriptor_type =
    Type::make_builtin_named_type("functionDescriptor", struct_type);
}

Type*
Func_descriptor_expression::do_type()
{
  Func_descriptor_expression::make_func_descriptor_type();
  return Func_descriptor_expression::descriptor_type;
}

// The backend representation for a function descriptor.

Bexpression*
Func_descriptor_expression::do_get_backend(Translate_context* context)
{
  Named_object* no = this->fn_;
  Location loc = no->location();
  if (this->dvar_ != NULL)
    return context->backend()->var_expression(this->dvar_, loc);

  Gogo* gogo = context->gogo();
  Backend_name bname;
  gogo->function_descriptor_backend_name(no, &bname);
  bool is_descriptor = false;
  if (no->is_function_declaration()
      && !no->func_declaration_value()->asm_name().empty()
      && Linemap::is_predeclared_location(no->location()))
    is_descriptor = true;

  // The runtime package implements some functions defined in the
  // syscall package.  Let the syscall package define the descriptor
  // in this case.
  if (gogo->compiling_runtime()
      && gogo->package_name() == "runtime"
      && no->is_function()
      && !no->func_value()->asm_name().empty()
      && no->func_value()->asm_name().compare(0, 8, "syscall.") == 0)
    is_descriptor = true;

  Btype* btype = this->type()->get_backend(gogo);

  Bvariable* bvar;
  if (no->package() != NULL || is_descriptor)
    bvar =
      context->backend()->immutable_struct_reference(bname.name(),
						     bname.optional_asm_name(),
						     btype, loc);
  else
    {
      Location bloc = Linemap::predeclared_location();

      // The runtime package has hash/equality functions that are
      // referenced by type descriptors outside of the runtime, so the
      // function descriptors must be visible even though they are not
      // exported.
      bool is_exported_runtime = false;
      if (gogo->compiling_runtime()
	  && gogo->package_name() == "runtime"
	  && (no->name().find("hash") != std::string::npos
	      || no->name().find("equal") != std::string::npos))
	is_exported_runtime = true;

      bool is_hidden = ((no->is_function()
			 && no->func_value()->enclosing() != NULL)
			|| (Gogo::is_hidden_name(no->name())
			    && !is_exported_runtime)
			|| Gogo::is_thunk(no));

      if (no->is_function() && no->func_value()->is_referenced_by_inline())
	is_hidden = false;

      unsigned int flags = 0;
      if (is_hidden)
	flags |= Backend::variable_is_hidden;
      bvar = context->backend()->immutable_struct(bname.name(),
						  bname.optional_asm_name(),
						  flags, btype, bloc);
      Expression_list* vals = new Expression_list();
      vals->push_back(Expression::make_func_code_reference(this->fn_, bloc));
      Expression* init =
	Expression::make_struct_composite_literal(this->type(), vals, bloc);
      Translate_context bcontext(gogo, NULL, NULL, NULL);
      bcontext.set_is_const();
      Bexpression* binit = init->get_backend(&bcontext);
      context->backend()->immutable_struct_set_init(bvar, bname.name(),
						    flags, btype, bloc, binit);
    }

  this->dvar_ = bvar;
  return gogo->backend()->var_expression(bvar, loc);
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
  do_is_static_initializer() const
  { return true; }

  Type*
  do_type()
  { return Type::make_pointer_type(Type::make_void_type()); }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return Expression::make_func_code_reference(this->function_,
						this->location());
  }

  Bexpression*
  do_get_backend(Translate_context*);

  void
  do_dump_expression(Ast_dump_context* context) const
  { context->ostream() << "[raw " << this->function_->name() << "]" ; }

 private:
  // The function.
  Named_object* function_;
};

// Get the backend representation for a reference to function code.

Bexpression*
Func_code_reference_expression::do_get_backend(Translate_context* context)
{
  return Func_expression::get_code_pointer(context->gogo(), this->function_,
					   this->location());
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

// Set the iota value if this could be a reference to iota.

void
Unknown_expression::set_iota_value(int iota_value)
{
  this->iota_value_ = iota_value;
  this->is_iota_ = true;
}

// Traversal.

int
Unknown_expression::do_traverse(Traverse* traverse)
{
  if (this->lowered_ != NULL)
    {
      if (Expression::traverse(&this->lowered_, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Determine the type of a reference to an unknown name.  At this
// point we have to figure out what the name refers to.

void
Unknown_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  if (this->is_error_expression())
    return;

  if (this->lowered_ != NULL)
    {
      this->lowered_->determine_type(gogo, context);
      return;
    }

  Location loc = this->location();

  Named_object* no = this->named_object_;
  if (no->is_unknown())
    {
      Named_object* real = no->unknown_value()->real_named_object();
      if (real == NULL)
	{
	  if (!this->no_error_message_)
	    go_error_at(loc, "reference to undefined name %qs",
			no->message_name().c_str());
	  this->set_is_error();
	  return;
	}
      no = real;
      this->named_object_ = real;
    }

  switch (no->classification())
    {
    case Named_object::NAMED_OBJECT_TYPE:
      this->lowered_ = Expression::make_type(no->type_value(), loc);
      break;
    case Named_object::NAMED_OBJECT_FUNC:
    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
      this->lowered_ = Expression::make_func_reference(no, NULL, loc);
      break;
    case Named_object::NAMED_OBJECT_CONST:
      this->lowered_ = Expression::make_const_reference(no, loc);
      this->lowered_->determine_type(gogo, context);
      if (this->is_iota_)
	this->lowered_->const_expression()->set_iota_value(this->iota_value_);
      break;
    case Named_object::NAMED_OBJECT_VAR:
      this->lowered_ = Expression::make_var_reference(no, loc);
      no->var_value()->set_is_used();
      this->lowered_->determine_type(gogo, context);
      break;
    case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
      if (!this->no_error_message_)
	go_error_at(this->location(), "reference to undefined type %qs",
		    no->message_name().c_str());
      this->set_is_error();
      break;
    case Named_object::NAMED_OBJECT_PACKAGE:
      if (!this->no_error_message_)
	this->report_error(_("unexpected reference to package"));
      this->set_is_error();
      break;
    default:
      go_unreachable();
    }
}

Type*
Unknown_expression::do_type()
{
  if (this->is_error_expression())
    return Type::make_error_type();
  go_assert(this->lowered_ != NULL);
  return this->lowered_->type();
}

bool
Unknown_expression::do_is_constant() const
{
  if (this->is_error_expression())
    return true;
  if (this->lowered_ != NULL)
    return this->lowered_->is_constant();

  // This can be called before do_determine_types by
  // Binary_expression::do_determine_type, which needs to know which
  // values are constant before it works out the appropriate
  // Type_context to pass down.
  Named_object* no = this->named_object_;
  if (no->is_unknown())
    {
      no = no->unknown_value()->real_named_object();
      if (no == NULL)
	return true;
    }
  return no->is_const();
}

bool
Unknown_expression::do_is_untyped(Type** ptype) const
{
  if (this->is_error_expression())
    return false;
  if (this->lowered_ != NULL)
    return this->lowered_->is_untyped(ptype);

  Named_object* no = this->named_object_;
  if (no->is_unknown())
    {
      no = no->unknown_value()->real_named_object();
      if (no == NULL)
	return false;
    }

  if (!no->is_const())
    return false;
  Type* t = no->const_value()->type();
  if (t != NULL)
    return Expression::is_untyped_type(t, ptype);
  return no->const_value()->expr()->is_untyped(ptype);
}

bool
Unknown_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->is_error_expression())
    return false;
  if (this->lowered_ != NULL)
    return this->lowered_->numeric_constant_value(nc);

  // This can be called before the determine_types pass.
  Named_object* no = this->named_object_;
  if (no->is_unknown())
    {
      no = no->unknown_value()->real_named_object();
      if (no == NULL)
	return false;
    }
  if (!no->is_const())
    return false;
  return no->const_value()->expr()->numeric_constant_value(nc);
}

bool
Unknown_expression::do_string_constant_value(std::string* val)
{
  if (this->is_error_expression())
    return false;
  go_assert(this->lowered_ != NULL);
  return this->lowered_->string_constant_value(val);
}

bool
Unknown_expression::do_boolean_constant_value(bool* val)
{
  if (this->is_error_expression())
    return false;
  go_assert(this->lowered_ != NULL);
  return this->lowered_->boolean_constant_value(val);
}

bool
Unknown_expression::do_is_addressable() const
{
  if (this->is_error_expression())
    return true;
  go_assert(this->lowered_ != NULL);
  return this->lowered_->is_addressable();
}

// Lower a reference to an unknown name.

Expression*
Unknown_expression::do_lower(Gogo*, Named_object*, Statement_inserter*)
{
  if (this->is_error_expression())
    return Expression::make_error(this->location());
  go_assert(this->lowered_ != NULL);
  return this->lowered_;
}

// Dump the ast representation for an unknown expression to a dump context.

void
Unknown_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  if (this->lowered_ != NULL)
    this->lowered_->dump_expression(ast_dump_context);
  else
    ast_dump_context->ostream() << "_Unknown_(" << this->named_object_->name()
				<< ")";
}

// Make a reference to an unknown name.

Unknown_expression*
Expression::make_unknown_reference(Named_object* no, Location location)
{
  return new Unknown_expression(no, location);
}

// Start exporting a type conversion for a constant, if needed.  This
// returns whether we need to export a closing parenthesis.

bool
Expression::export_constant_type(Export_function_body* efb, Type* type)
{
  if (type == NULL
      || type->is_abstract()
      || type == efb->type_context())
    return false;
  efb->write_c_string("$convert(");
  efb->write_type(type);
  efb->write_c_string(", ");
  return true;
}

// Finish a type conversion for a constant.

void
Expression::finish_export_constant_type(Export_function_body* efb, bool needed)
{
  if (needed)
    efb->write_c_string(")");
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
  do_import(Import_expression*, Location);

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_is_constant() const
  { return true; }

  bool
  do_is_untyped(Type**) const;

  bool
  do_is_zero_value() const
  { return this->val_ == false; }

  bool
  do_boolean_constant_value(bool* val)
  {
    *val = this->val_;
    return true;
  }

  bool
  do_is_static_initializer() const
  { return true; }

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*);

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context* context)
  { return context->backend()->boolean_constant_expression(this->val_); }

  int
  do_inlining_cost() const
  { return 1; }

  void
  do_export(Export_function_body* efb) const;

  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const
  { ast_dump_context->ostream() << (this->val_ ? "true" : "false"); }

 private:
  // The constant.
  bool val_;
  // The type as determined by context.
  Type* type_;
};

// Traverse a boolean expression.  We just need to traverse the type
// if there is one.

int
Boolean_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

bool
Boolean_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);
  *ptype = Type::make_boolean_type();
  return true;
}

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
Boolean_expression::do_determine_type(Gogo*, const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL && context->type->is_boolean_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_bool_type();
}

// Export a boolean constant.

void
Boolean_expression::do_export(Export_function_body* efb) const
{
  bool exported_type = Expression::export_constant_type(efb, this->type_);
  efb->write_c_string(this->val_ ? "$true" : "$false");
  Expression::finish_export_constant_type(efb, exported_type);
}

// Import a boolean constant.

Expression*
Boolean_expression::do_import(Import_expression* imp, Location loc)
{
  if (imp->version() >= EXPORT_FORMAT_V3)
    imp->require_c_string("$");
  if (imp->peek_char() == 't')
    {
      imp->require_c_string("true");
      return Expression::make_boolean(true, loc);
    }
  else
    {
      imp->require_c_string("false");
      return Expression::make_boolean(false, loc);
    }
}

// Make a boolean expression.

Expression*
Expression::make_boolean(bool val, Location location)
{
  return new Boolean_expression(val, location);
}

// Class String_expression.

// Traverse a string expression.  We just need to traverse the type
// if there is one.

int
String_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

bool
String_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);
  *ptype = Type::make_string_type();
  return true;
}

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
String_expression::do_determine_type(Gogo*, const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL && context->type->is_string_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_string_type();
}

// Build a string constant.

Bexpression*
String_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Btype* btype = Type::make_string_type()->get_backend(gogo);

  Location loc = this->location();
  std::vector<Bexpression*> init(2);

  if (this->val_.size() == 0)
    init[0] = gogo->backend()->nil_pointer_expression();
  else
    {
      Bexpression* str_cst =
	gogo->backend()->string_constant_expression(this->val_);
      init[0] = gogo->backend()->address_expression(str_cst, loc);
    }

  Btype* int_btype = Type::lookup_integer_type("int")->get_backend(gogo);
  mpz_t lenval;
  mpz_init_set_ui(lenval, this->val_.length());
  init[1] = gogo->backend()->integer_constant_expression(int_btype, lenval);
  mpz_clear(lenval);

  return gogo->backend()->constructor_expression(btype, init, loc);
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
String_expression::do_export(Export_function_body* efb) const
{
  bool exported_type = Expression::export_constant_type(efb, this->type_);
  String_expression::export_string(efb, this);
  Expression::finish_export_constant_type(efb, exported_type);
}

// Import a string expression.

Expression*
String_expression::do_import(Import_expression* imp, Location loc)
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
	      go_error_at(imp->location(), "bad string constant");
	      return Expression::make_error(loc);
	    }
	}
    }
  return Expression::make_string(val, loc);
}

// Ast dump for string expression.

void
String_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  String_expression::export_string(ast_dump_context, this);
}

// Make a string expression with abstract string type (common case).

Expression*
Expression::make_string(const std::string& val, Location location)
{
  return new String_expression(val, NULL, location);
}

// Make a string expression with a specific string type.

Expression*
Expression::make_string_typed(const std::string& val, Type* type, Location location)
{
  return new String_expression(val, type, location);
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
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return new String_info_expression(this->string_->copy(), this->string_info_,
				      this->location());
  }

  Bexpression*
  do_get_backend(Translate_context* context);

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

Bexpression*
String_info_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  Bexpression* bstring = this->string_->get_backend(context);
  switch (this->string_info_)
    {
    case STRING_INFO_DATA:
    case STRING_INFO_LENGTH:
      return gogo->backend()->struct_field_expression(bstring,
						      this->string_info_,
						      this->location());
      break;
    default:
      go_unreachable();
    }
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

// An expression that represents an string value: a struct with value pointer
// and length fields.

class String_value_expression : public Expression
{
 public:
  String_value_expression(Expression* valptr, Expression* len, Location location)
      : Expression(EXPRESSION_STRING_VALUE, location),
        valptr_(valptr), len_(len)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type()
  { return Type::make_string_type(); }

  void
  do_determine_type(Gogo*, const Type_context*)
  { go_unreachable(); }

  Expression*
  do_copy()
  {
    return new String_value_expression(this->valptr_->copy(),
                                       this->len_->copy(),
                                       this->location());
  }

  Bexpression*
  do_get_backend(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The value pointer.
  Expression* valptr_;
  // The length.
  Expression* len_;
};

int
String_value_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->valptr_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->len_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

Bexpression*
String_value_expression::do_get_backend(Translate_context* context)
{
  std::vector<Bexpression*> vals(2);
  vals[0] = this->valptr_->get_backend(context);
  vals[1] = this->len_->get_backend(context);

  Gogo* gogo = context->gogo();
  Btype* btype = Type::make_string_type()->get_backend(gogo);
  return gogo->backend()->constructor_expression(btype, vals, this->location());
}

void
String_value_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "stringvalue(";
  ast_dump_context->ostream() << "value: ";
  this->valptr_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ", length: ";
  this->len_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ")";
}

Expression*
Expression::make_string_value(Expression* valptr, Expression* len,
                              Location location)
{
  return new String_value_expression(valptr, len, location);
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
  do_import(Import_expression*, Location);

  // Write VAL to string dump.
  static void
  export_integer(String_dump* exp, const mpz_t val);

  // Write VAL to dump context.
  static void
  dump_integer(Ast_dump_context* ast_dump_context, const mpz_t val);

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_is_constant() const
  { return true; }

  bool
  do_is_untyped(Type**) const;

  bool
  do_is_zero_value() const
  { return mpz_sgn(this->val_) == 0; }

  bool
  do_is_static_initializer() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc);

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context* context);

  void
  do_check_types(Gogo*);

  Bexpression*
  do_get_backend(Translate_context*);

  Expression*
  do_copy()
  {
    if (this->is_character_constant_)
      return Expression::make_character(&this->val_,
					(this->type_ == NULL
					 ? NULL
					 : this->type_->copy_expressions()),
					this->location());
    else
      return Expression::make_integer_z(&this->val_,
					(this->type_ == NULL
					 ? NULL
					 : this->type_->copy_expressions()),
					this->location());
  }

  int
  do_inlining_cost() const
  { return 1; }

  void
  do_export(Export_function_body*) const;

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

// Traverse an integer expression.  We just need to traverse the type
// if there is one.

int
Integer_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

// Return a numeric constant for this expression.  We have to mark
// this as a character when appropriate.

bool
Integer_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->is_character_constant_)
    nc->set_rune(this->type_, this->val_);
  else
    nc->set_int(this->type_, this->val_);
  return true;
}

bool
Integer_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);
  if (this->is_character_constant_)
    *ptype = Type::make_abstract_character_type();
  else
    *ptype = Type::make_abstract_integer_type();
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
Integer_expression::do_determine_type(Gogo*, const Type_context* context)
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

// Get the backend representation for an integer constant.

Bexpression*
Integer_expression::do_get_backend(Translate_context* context)
{
  if (this->is_error_expression()
      || (this->type_ != NULL && this->type_->is_error_type()))
    {
      go_assert(saw_errors());
      return context->gogo()->backend()->error_expression();
    }

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
            go_error_at(this->location(), "integer constant overflow");
          return context->gogo()->backend()->error_expression();
        }
    }
  Numeric_constant nc;
  nc.set_int(resolved_type, this->val_);
  return Expression::backend_numeric_constant_expression(context, &nc);
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
Integer_expression::do_export(Export_function_body* efb) const
{
  bool exported_type = Expression::export_constant_type(efb, this->type_);

  Integer_expression::export_integer(efb, this->val_);
  if (this->is_character_constant_)
    efb->write_c_string("'");
  // A trailing space lets us reliably identify the end of the number.
  efb->write_c_string(" ");

  Expression::finish_export_constant_type(efb, exported_type);
}

// Import an integer, floating point, or complex value.  This handles
// all these types because they all start with digits.

Expression*
Integer_expression::do_import(Import_expression* imp, Location loc)
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
	  go_error_at(imp->location(), "bad number in import data: %qs",
		      num.c_str());
	  return Expression::make_error(loc);
	}
      if (pos == std::string::npos)
	mpfr_init_set_ui(real, 0, MPFR_RNDN);
      else
	{
	  std::string real_str = num.substr(0, pos);
	  if (mpfr_init_set_str(real, real_str.c_str(), 10, MPFR_RNDN) != 0)
	    {
	      go_error_at(imp->location(), "bad number in import data: %qs",
			  real_str.c_str());
	      return Expression::make_error(loc);
	    }
	}

      std::string imag_str;
      if (pos == std::string::npos)
	imag_str = num;
      else
	imag_str = num.substr(pos);
      imag_str = imag_str.substr(0, imag_str.size() - 1);
      mpfr_t imag;
      if (mpfr_init_set_str(imag, imag_str.c_str(), 10, MPFR_RNDN) != 0)
	{
	  go_error_at(imp->location(), "bad number in import data: %qs",
		      imag_str.c_str());
	  return Expression::make_error(loc);
	}
      mpc_t cval;
      mpc_init2(cval, mpc_precision);
      mpc_set_fr_fr(cval, real, imag, MPC_RNDNN);
      mpfr_clear(real);
      mpfr_clear(imag);
      Expression* ret = Expression::make_complex(&cval, NULL, loc);
      mpc_clear(cval);
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
	  go_error_at(imp->location(), "bad number in import data: %qs",
		      num.c_str());
	  return Expression::make_error(loc);
	}
      Expression* ret;
      if (is_character_constant)
	ret = Expression::make_character(&val, NULL, loc);
      else
	ret = Expression::make_integer_z(&val, NULL, loc);
      mpz_clear(val);
      return ret;
    }
  else
    {
      mpfr_t val;
      if (mpfr_init_set_str(val, num.c_str(), 10, MPFR_RNDN) != 0)
	{
	  go_error_at(imp->location(), "bad number in import data: %qs",
		      num.c_str());
	  return Expression::make_error(loc);
	}
      Expression* ret = Expression::make_float(&val, NULL, loc);
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

// Build a new integer value from a multi-precision integer.

Expression*
Expression::make_integer_z(const mpz_t* val, Type* type, Location location)
{
  return new Integer_expression(val, type, false, location);
}

// Build a new integer value from an unsigned long.

Expression*
Expression::make_integer_ul(unsigned long val, Type *type, Location location)
{
  mpz_t zval;
  mpz_init_set_ui(zval, val);
  Expression* ret = Expression::make_integer_z(&zval, type, location);
  mpz_clear(zval);
  return ret;
}

// Build a new integer value from a signed long.

Expression*
Expression::make_integer_sl(long val, Type *type, Location location)
{
  mpz_t zval;
  mpz_init_set_si(zval, val);
  Expression* ret = Expression::make_integer_z(&zval, type, location);
  mpz_clear(zval);
  return ret;
}

// Store an int64_t in an uninitialized mpz_t.

static void
set_mpz_from_int64(mpz_t* zval, int64_t val)
{
  if (val >= 0)
    {
      unsigned long ul = static_cast<unsigned long>(val);
      if (static_cast<int64_t>(ul) == val)
	{
	  mpz_init_set_ui(*zval, ul);
	  return;
	}
    }
  uint64_t uv;
  if (val >= 0)
    uv = static_cast<uint64_t>(val);
  else
    uv = static_cast<uint64_t>(- val);
  unsigned long ul = uv & 0xffffffffUL;
  mpz_init_set_ui(*zval, ul);
  mpz_t hval;
  mpz_init_set_ui(hval, static_cast<unsigned long>(uv >> 32));
  mpz_mul_2exp(hval, hval, 32);
  mpz_add(*zval, *zval, hval);
  mpz_clear(hval);
  if (val < 0)
    mpz_neg(*zval, *zval);
}

// Build a new integer value from an int64_t.

Expression*
Expression::make_integer_int64(int64_t val, Type* type, Location location)
{
  mpz_t zval;
  set_mpz_from_int64(&zval, val);
  Expression* ret = Expression::make_integer_z(&zval, type, location);
  mpz_clear(zval);
  return ret;
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
    mpfr_init_set(this->val_, *val, MPFR_RNDN);
  }

  // Write VAL to export data.
  static void
  export_float(String_dump* exp, const mpfr_t val);

  // Write VAL to dump file.
  static void
  dump_float(Ast_dump_context* ast_dump_context, const mpfr_t val);

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_is_constant() const
  { return true; }

  bool
  do_is_untyped(Type**) const;

  bool
  do_is_zero_value() const
  {
    return mpfr_zero_p(this->val_) != 0
           && mpfr_signbit(this->val_) == 0;
  }

  bool
  do_is_static_initializer() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc)
  {
    nc->set_float(this->type_, this->val_);
    return true;
  }

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  { return Expression::make_float(&this->val_,
				  (this->type_ == NULL
				   ? NULL
				   : this->type_->copy_expressions()),
				  this->location()); }

  Bexpression*
  do_get_backend(Translate_context*);

  int
  do_inlining_cost() const
  { return 1; }

  void
  do_export(Export_function_body*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The floating point value.
  mpfr_t val_;
  // The type so far.
  Type* type_;
};

// Traverse a float expression.  We just need to traverse the type if
// there is one.

int
Float_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

bool
Float_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);
  *ptype = Type::make_abstract_float_type();
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
Float_expression::do_determine_type(Gogo*, const Type_context* context)
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

// Get the backend representation for a float constant.

Bexpression*
Float_expression::do_get_backend(Translate_context* context)
{
  if (this->is_error_expression()
      || (this->type_ != NULL && this->type_->is_error_type()))
    {
      go_assert(saw_errors());
      return context->gogo()->backend()->error_expression();
    }

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
  return Expression::backend_numeric_constant_expression(context, &nc);
}

// Write a floating point number to a string dump.

void
Float_expression::export_float(String_dump *exp, const mpfr_t val)
{
  mpfr_exp_t exponent;
  char* s = mpfr_get_str(NULL, &exponent, 10, 0, val, MPFR_RNDN);
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
Float_expression::do_export(Export_function_body* efb) const
{
  bool exported_type = Expression::export_constant_type(efb, this->type_);

  Float_expression::export_float(efb, this->val_);
  // A trailing space lets us reliably identify the end of the number.
  efb->write_c_string(" ");

  Expression::finish_export_constant_type(efb, exported_type);
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
  Complex_expression(const mpc_t* val, Type* type, Location location)
    : Expression(EXPRESSION_COMPLEX, location),
      type_(type)
  {
    mpc_init2(this->val_, mpc_precision);
    mpc_set(this->val_, *val, MPC_RNDNN);
  }

  // Write VAL to string dump.
  static void
  export_complex(String_dump* exp, const mpc_t val);

  // Write REAL/IMAG to dump context.
  static void
  dump_complex(Ast_dump_context* ast_dump_context, const mpc_t val);

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_is_constant() const
  { return true; }

  bool
  do_is_untyped(Type**) const;

  bool
  do_is_zero_value() const
  {
    return mpfr_zero_p(mpc_realref(this->val_)) != 0
           && mpfr_signbit(mpc_realref(this->val_)) == 0
           && mpfr_zero_p(mpc_imagref(this->val_)) != 0
           && mpfr_signbit(mpc_imagref(this->val_)) == 0;
  }

  bool
  do_is_static_initializer() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc)
  {
    nc->set_complex(this->type_, this->val_);
    return true;
  }

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_complex(&this->val_,
				    (this->type_ == NULL
				     ? NULL
				     : this->type_->copy_expressions()),
				    this->location());
  }

  Bexpression*
  do_get_backend(Translate_context*);

  int
  do_inlining_cost() const
  { return 2; }

  void
  do_export(Export_function_body*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The complex value.
  mpc_t val_;
  // The type if known.
  Type* type_;
};

// Traverse a complex expression.  We just need to traverse the type
// if there is one.

int
Complex_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

bool
Complex_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);
  *ptype = Type::make_abstract_complex_type();
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
Complex_expression::do_determine_type(Gogo*, const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL && context->type->is_numeric_type())
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
  nc.set_complex(NULL, this->val_);
  if (!nc.set_type(this->type_, true, this->location()))
    this->set_is_error();
}

// Get the backend representation for a complex constant.

Bexpression*
Complex_expression::do_get_backend(Translate_context* context)
{
  if (this->is_error_expression()
      || (this->type_ != NULL && this->type_->is_error_type()))
    {
      go_assert(saw_errors());
      return context->gogo()->backend()->error_expression();
    }

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
      // If we still have an abstract type here, this is being
      // used in a constant expression which didn't get reduced.  We
      // just use complex128 and hope for the best.
      resolved_type = Type::lookup_complex_type("complex128");
    }

  Numeric_constant nc;
  nc.set_complex(resolved_type, this->val_);
  return Expression::backend_numeric_constant_expression(context, &nc);
}

// Write REAL/IMAG to export data.

void
Complex_expression::export_complex(String_dump* exp, const mpc_t val)
{
  if (!mpfr_zero_p(mpc_realref(val)))
    {
      Float_expression::export_float(exp, mpc_realref(val));
      if (mpfr_sgn(mpc_imagref(val)) >= 0)
	exp->write_c_string("+");
    }
  Float_expression::export_float(exp, mpc_imagref(val));
  exp->write_c_string("i");
}

// Export a complex number in a constant expression.

void
Complex_expression::do_export(Export_function_body* efb) const
{
  bool exported_type = Expression::export_constant_type(efb, this->type_);

  Complex_expression::export_complex(efb, this->val_);
  // A trailing space lets us reliably identify the end of the number.
  efb->write_c_string(" ");

  Expression::finish_export_constant_type(efb, exported_type);
}

// Dump a complex expression to the dump file.

void
Complex_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  Complex_expression::export_complex(ast_dump_context, this->val_);
}

// Make a complex expression.

Expression*
Expression::make_complex(const mpc_t* val, Type* type, Location location)
{
  return new Complex_expression(val, type, location);
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

// Class Const_expression.

// Traversal.

int
Const_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

// Whether this is the zero value.

bool
Const_expression::do_is_zero_value() const
{
  return this->constant_->const_value()->expr()->is_zero_value();
}

// Lower a constant expression.  This is where we convert the
// predeclared constant iota into an integer value.

Expression*
Const_expression::do_lower(Gogo* gogo, Named_object*, Statement_inserter*)
{
  Location loc = this->location();

  if (this->is_error_expression())
    return Expression::make_error(loc);
  if (this->constant_->const_value()->expr()->is_error_expression())
    return Expression::make_error(loc);

  if (this->is_iota_)
    return Expression::make_integer_ul(this->iota_value_, NULL, loc);

  // Make sure that the constant itself has been lowered.
  gogo->lower_constant(this->constant_);

  return this;
}

// Return a numeric constant value.

bool
Const_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->seen_)
    return false;

  Type* ctype;
  if (this->type_ != NULL)
    ctype = this->type_;
  else
    ctype = this->constant_->const_value()->type();

  if (this->is_iota_)
    {
      nc->set_unsigned_long(ctype,
			    static_cast<unsigned long>(this->iota_value_));
      return true;
    }

  Expression* e = this->constant_->const_value()->expr();

  this->seen_ = true;

  bool r = e->numeric_constant_value(nc);

  this->seen_ = false;

  if (r && ctype != NULL)
    {
      if (!nc->set_type(ctype, false, this->location()))
	return false;
    }

  return r;
}

bool
Const_expression::do_string_constant_value(std::string* val)
{
  if (this->seen_)
    return false;
  if (this->is_iota_)
    return false;

  Expression* e = this->constant_->const_value()->expr();

  this->seen_ = true;
  bool ok = e->string_constant_value(val);
  this->seen_ = false;

  return ok;
}

bool
Const_expression::do_boolean_constant_value(bool* val)
{
  if (this->seen_)
    return false;
  if (this->is_iota_)
    return false;

  Expression* e = this->constant_->const_value()->expr();

  this->seen_ = true;
  bool ok = e->boolean_constant_value(val);
  this->seen_ = false;

  return ok;
}

// Whether this is untyped.

bool
Const_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);

  Named_constant* nc = this->constant_->const_value();
  if (nc->type() != NULL)
    return Expression::is_untyped_type(nc->type(), ptype);

  return nc->expr()->is_untyped(ptype);
}

// Return the type of the const reference.

Type*
Const_expression::do_type()
{
  if (this->type_ == NULL)
    {
      go_assert(saw_errors());
      return Type::make_error_type();
    }

  return this->type_;
}

// Set the type of the const reference.

void
Const_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  if (this->type_ != NULL)
    return;

  // The type may depend on the type of other constants.  Avoid an
  // endless loop.
  if (this->seen_)
    {
      if (!saw_errors())
	go_error_at(this->location(), "constant refers to itself");
      this->set_is_error();
      this->type_ = Type::make_error_type();
      return;
    }

  this->seen_ = true;

  Named_constant* nc = this->constant_->const_value();
  nc->determine_type(gogo);

  Type* ctype = nc->type();

  this->seen_ = false;

  if (ctype == NULL)
    {
      go_error_at(nc->expr()->location(), "constant refers to itself");
      this->set_is_error();
      this->type_ = Type::make_error_type();
    }
  else if (!ctype->is_abstract())
    this->type_ = ctype;
  else if (context->type != NULL
	   && context->type->is_numeric_type()
	   && ctype->is_numeric_type())
    this->type_ = context->type;
  else if (context->type != NULL
	   && context->type->is_string_type()
	   && ctype->is_string_type())
    this->type_ = context->type;
  else if (context->type != NULL
	   && context->type->is_boolean_type()
	   && ctype->is_boolean_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    {
      if (ctype->is_abstract())
	ctype = ctype->make_non_abstract_type();
      this->type_ = ctype;
    }
  else
    this->type_ = ctype;
}

// Check for a loop in which the initializer of a constant refers to
// the constant itself.

void
Const_expression::check_for_init_loop()
{
  if (this->is_error_expression())
    return;
  if (this->type_ != NULL && this->type_->is_error())
    return;
  if (this->constant_->const_value()->expr()->is_error_expression())
    {
      this->set_is_error();
      return;
    }

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

// Set the iota value if this is a reference to iota.

void
Const_expression::set_iota_value(int iota_value)
{
  Named_constant* nc = this->constant_->const_value();
  if (nc->expr()->classification() == EXPRESSION_IOTA)
    {
      this->is_iota_ = true;
      this->iota_value_ = iota_value;
    }
}

// Check types of a const reference.

void
Const_expression::do_check_types(Gogo*)
{
  if (this->is_error_expression())
    return;
  if (this->type_ != NULL && this->type_->is_error())
    return;
  if (this->constant_->const_value()->expr()->is_error_expression())
    {
      this->set_is_error();
      return;
    }

  Expression* expr = this->constant_->const_value()->expr();
  if (expr->classification() == EXPRESSION_IOTA && !this->is_iota_)
    {
      go_error_at(this->location(),
		  "iota is only defined in const declarations");
      // Avoid knock-on errors.
      this->is_iota_ = true;
      this->iota_value_ = 0;
    }

  if (this->is_iota_ && this->type_->is_numeric_type())
    {
      Numeric_constant nc;
      nc.set_unsigned_long(Type::make_abstract_integer_type(),
			   static_cast<unsigned long>(this->iota_value_));
      if (!nc.set_type(this->type_, true, this->location()))
	this->set_is_error();
      return;
    }

  this->check_for_init_loop();

  // Check that numeric constant fits in type.
  if (this->type_->is_numeric_type())
    {
      Numeric_constant nc;
      if (expr->numeric_constant_value(&nc))
	{
	  if (!nc.set_type(this->type_, true, this->location()))
	    this->set_is_error();
	}
    }
}

// Return the backend representation for a const reference.

Bexpression*
Const_expression::do_get_backend(Translate_context* context)
{
  if (this->is_error_expression()
      || (this->type_ != NULL && this->type_->is_error()))
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

  go_assert(!this->is_iota_);

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
	  return e->get_backend(context);
	}
    }

  if (this->type_ != NULL)
    expr = Expression::make_cast(this->type_, expr, this->location());
  return expr->get_backend(context);
}

// When exporting a reference to a const as part of a const
// expression, we export the value.  We ignore the fact that it has
// a name.

void
Const_expression::do_export(Export_function_body* efb) const
{
  this->constant_->const_value()->expr()->export_expression(efb);
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
    case Expression::EXPRESSION_ERROR:
      return TRAVERSE_EXIT;
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
  do_import(Import_expression*, Location);

 protected:
  bool
  do_is_constant() const
  { return true; }

  bool
  do_untyped_type(Type** ptype) const
  {
    *ptype = Type::make_nil_type();
    return true;
  }

  bool
  do_is_zero_value() const
  { return true; }

  bool
  do_is_static_initializer() const
  { return true; }

  Type*
  do_type()
  { return Type::make_nil_type(); }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context* context)
  { return context->backend()->nil_pointer_expression(); }

  int
  do_inlining_cost() const
  { return 1; }

  void
  do_export(Export_function_body* efb) const
  { efb->write_c_string("$nil"); }

  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const
  { ast_dump_context->ostream() << "nil"; }
};

// Import a nil expression.

Expression*
Nil_expression::do_import(Import_expression* imp, Location loc)
{
  if (imp->version() >= EXPORT_FORMAT_V3)
    imp->require_c_string("$");
  imp->require_c_string("nil");
  return Expression::make_nil(loc);
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
  Type*
  do_type()
  { return Type::make_abstract_integer_type(); }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*)
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

// Class Type_conversion_expression.

// Traversal.

int
Type_conversion_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->expr_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return the type of the expression.

Type*
Type_conversion_expression::do_type()
{
  if (this->is_error_expression() || this->expr_->is_error_expression())
    return Type::make_error_type();
  return this->type_;
}

// Convert to a constant at lowering time.  Also lower conversions
// from slice to pointer-to-array, as they can panic.

Expression*
Type_conversion_expression::do_lower(Gogo* gogo, Named_object*,
				     Statement_inserter* inserter)
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

  // According to the language specification on string conversions
  // (http://golang.org/ref/spec#Conversions_to_and_from_a_string_type):
  // When converting an integer into a string, the string will be a UTF-8
  // representation of the integer and integers "outside the range of valid
  // Unicode code points are converted to '\uFFFD'."
  if (type->is_string_type())
    {
      Numeric_constant nc;
      if (val->numeric_constant_value(&nc) && nc.is_int())
        {
          // An integer value doesn't fit in the Unicode code point range if it
          // overflows the Go "int" type or is negative.
          unsigned long ul;
          if (!nc.set_type(Type::lookup_integer_type("int"), false, location)
              || nc.to_unsigned_long(&ul) == Numeric_constant::NC_UL_NEGATIVE)
            return Expression::make_string("\ufffd", location);
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
		      unsigned char c = static_cast<unsigned char>(*p);
		      vals->push_back(Expression::make_integer_ul(c,
								  element_type,
								  location));
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
			  go_warning_at(this->location(), 0,
				     "invalid UTF-8 encoding");
			  adv = 1;
			}
		      p += adv;
		      vals->push_back(Expression::make_integer_ul(c,
								  element_type,
								  location));
		    }
		}

	      return Expression::make_slice_composite_literal(type, vals,
							      location);
	    }
	}
    }

  if (type->points_to() != NULL
      && type->points_to()->array_type() != NULL
      && !type->points_to()->is_slice_type()
      && val->type()->is_slice_type()
      && Type::are_identical(type->points_to()->array_type()->element_type(),
			     val->type()->array_type()->element_type(),
			     0, NULL))
    {
      Temporary_statement* val_temp = NULL;
      if (!val->is_multi_eval_safe())
	{
	  val_temp = Statement::make_temporary(val->type(), NULL, location);
	  inserter->insert(val_temp);
	  val = Expression::make_set_and_use_temporary(val_temp, val,
						       location);
	}

      Type* int_type = Type::lookup_integer_type("int");
      Temporary_statement* vallen_temp =
	Statement::make_temporary(int_type, NULL, location);
      inserter->insert(vallen_temp);

      Expression* arrlen = type->points_to()->array_type()->length();
      Expression* vallen =
	Expression::make_slice_info(val, Expression::SLICE_INFO_LENGTH,
				    location);
      vallen = Expression::make_set_and_use_temporary(vallen_temp, vallen,
						      location);
      Expression* cond = Expression::make_binary(OPERATOR_GT, arrlen, vallen,
						 location);

      vallen = Expression::make_temporary_reference(vallen_temp, location);
      Expression* panic = Runtime::make_call(gogo,
					     Runtime::PANIC_SLICE_CONVERT,
					     location, 2, arrlen, vallen);

      Expression* nil = Expression::make_nil(location);
      Expression* check = Expression::make_conditional(cond, panic, nil,
						       location);

      if (val_temp == NULL)
	val = val->copy();
      else
	val = Expression::make_temporary_reference(val_temp, location);
      Expression* ptr =
	Expression::make_slice_info(val, Expression::SLICE_INFO_VALUE_POINTER,
				    location);
      ptr = Expression::make_unsafe_cast(type, ptr, location);

      Expression* ret = Expression::make_compound(check, ptr, location);
      ret->determine_type_no_context(gogo);
      return ret;
    }

  return this;
}

// Flatten a type conversion by using a temporary variable for the slice
// in slice to string conversions.

Expression*
Type_conversion_expression::do_flatten(Gogo*, Named_object*,
                                       Statement_inserter* inserter)
{
  if (this->type()->is_error_type() || this->expr_->is_error_expression())
    {
      go_assert(saw_errors());
      return Expression::make_error(this->location());
    }

  if (((this->type()->is_string_type()
        && this->expr_->type()->is_slice_type())
       || this->expr_->type()->interface_type() != NULL)
      && !this->expr_->is_multi_eval_safe())
    {
      Temporary_statement* temp =
          Statement::make_temporary(NULL, this->expr_, this->location());
      inserter->insert(temp);
      this->expr_ = Expression::make_temporary_reference(temp, this->location());
    }

  // For interface conversion and string to/from slice conversions,
  // decide if we can allocate on stack.
  if (this->type()->interface_type() != NULL
      || this->type()->is_string_type()
      || this->expr_->type()->is_string_type())
    {
      Node* n = Node::make_node(this);
      if ((n->encoding() & ESCAPE_MASK) == Node::ESCAPE_NONE)
        this->no_escape_ = true;
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

// Return whether a type conversion is a zero value.

bool
Type_conversion_expression::do_is_zero_value() const
{
  if (!this->expr_->is_zero_value())
    return false;

  // Some type conversion from zero value is still not zero value.
  // For example, []byte("") or interface{}(0).
  // Conservatively, only report true if the RHS is nil.
  Type* type = this->type_;
  if (type->integer_type() == NULL
      && type->float_type() == NULL
      && type->complex_type() == NULL
      && !type->is_boolean_type()
      && !type->is_string_type())
    return this->expr_->is_nil_expression();

  return true;
}

// Return whether a type conversion can be used in a constant
// initializer.

bool
Type_conversion_expression::do_is_static_initializer() const
{
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();

  if (type->interface_type() != NULL
      || expr_type->interface_type() != NULL)
    return false;

  if (!this->expr_->is_static_initializer())
    return false;

  if (Type::are_identical(type, expr_type,
			  Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			  NULL))
    return true;

  if (type->is_string_type() && expr_type->is_string_type())
    return true;

  if ((type->is_numeric_type()
       || type->is_boolean_type()
       || type->points_to() != NULL)
      && (expr_type->is_numeric_type()
	  || expr_type->is_boolean_type()
	  || expr_type->points_to() != NULL))
    return true;

  return false;
}

// Return the constant numeric value if there is one.

bool
Type_conversion_expression::do_numeric_constant_value(
    Numeric_constant* nc)
{
  if (!this->type_->is_numeric_type())
    return false;
  if (!this->expr_->numeric_constant_value(nc))
    return false;
  return nc->set_type(this->type_, false, this->location());
}

// Return the constant string value if there is one.

bool
Type_conversion_expression::do_string_constant_value(std::string* val)
{
  if (this->type_->is_string_type() && this->expr_->type()->is_string_type())
    return this->expr_->string_constant_value(val);

  if (this->type_->is_string_type()
      && this->expr_->type()->integer_type() != NULL)
    {
      Numeric_constant nc;
      if (this->expr_->numeric_constant_value(&nc))
	{
	  unsigned long ival;
	  if (nc.to_unsigned_long(&ival) == Numeric_constant::NC_UL_VALID)
	    {
	      unsigned int cval = static_cast<unsigned int>(ival);
	      if (static_cast<unsigned long>(cval) != ival)
		{
		  go_warning_at(this->location(), 0,
				"unicode code point 0x%lx out of range",
				ival);
		  cval = 0xfffd; // Unicode "replacement character."
		}
	      val->clear();
	      Lex::append_char(cval, true, val, this->location());
	      return true;
	    }
	}
    }

  // FIXME: Could handle conversion from const []int here.

  return false;
}

// Return the constant boolean value if there is one.

bool
Type_conversion_expression::do_boolean_constant_value(bool* val)
{
  if (!this->type_->is_boolean_type())
    return false;
  return this->expr_->boolean_constant_value(val);
}

// Determine the resulting type of the conversion.

void
Type_conversion_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  Type_context subcontext(this->type_, false);
  this->expr_->determine_type(gogo, &subcontext);
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

  // We can convert all numeric types if the value is a constant.
  if (type->is_numeric_type()
      && expr_type->is_numeric_type()
      && this->expr_->is_constant())
    return;

  go_error_at(this->location(), "%s", reason.c_str());
  this->set_is_error();
}

// Copy.

Expression*
Type_conversion_expression::do_copy()
{
  Expression* ret = new Type_conversion_expression(this->type_->copy_expressions(),
                                                   this->expr_->copy(),
                                                   this->location());
  ret->conversion_expression()->set_no_copy(this->no_copy_);
  return ret;
}

// Get the backend representation for a type conversion.

Bexpression*
Type_conversion_expression::do_get_backend(Translate_context* context)
{
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();
  Type_context tcontext(type, false);

  Gogo* gogo = context->gogo();
  Btype* btype = type->get_backend(gogo);
  Location loc = this->location();

  if (Type::are_identical(type, expr_type,
			  Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			  NULL))
    {
      Bexpression* bexpr = this->expr_->get_backend(context);
      return gogo->backend()->convert_expression(btype, bexpr, loc);
    }
  else if (type->interface_type() != NULL
           && expr_type->interface_type() == NULL)
    {
      Expression* conversion =
          Expression::convert_type_to_interface(type, this->expr_,
                                                this->no_escape_, loc);
      conversion->determine_type(gogo, &tcontext);
      return conversion->get_backend(context);
    }
  else if (type->interface_type() != NULL
	   || expr_type->interface_type() != NULL)
    {
      Expression* conversion =
          Expression::convert_for_assignment(gogo, type, this->expr_,
                                             loc);
      conversion->determine_type(gogo, &tcontext);
      return conversion->get_backend(context);
    }
  else if (type->is_string_type()
	   && expr_type->integer_type() != NULL)
    {
      mpz_t intval;
      Numeric_constant nc;
      if (this->expr_->numeric_constant_value(&nc)
	  && nc.to_int(&intval))
	{
	  std::string s;
          unsigned int x;
          if (mpz_fits_uint_p(intval))
            x = mpz_get_ui(intval);
          else
            {
              char* ms = mpz_get_str(NULL, 16, intval);
              go_warning_at(loc, 0,
                            "unicode code point 0x%s out of range in string",
                            ms);
              free(ms);
              x = 0xfffd;
            }
	  Lex::append_char(x, true, &s, loc);
	  mpz_clear(intval);
	  Expression* se = Expression::make_string(s, loc);
	  se->determine_type(gogo, &tcontext);
	  return se->get_backend(context);
	}

      Expression* buf;
      if (this->no_escape_)
        {
          Type* byte_type = Type::lookup_integer_type("uint8");
          Expression* buflen =
            Expression::make_integer_ul(4, NULL, loc);
          Type* array_type = Type::make_array_type(byte_type, buflen);
          buf = Expression::make_allocation(array_type, loc);
          buf->allocation_expression()->set_allocate_on_stack();
          buf->allocation_expression()->set_no_zero();
        }
      else
        buf = Expression::make_nil(loc);
      Expression* i2s_expr =
        Runtime::make_call(gogo, Runtime::INTSTRING, loc, 2, buf, this->expr_);
      Expression* ret = Expression::make_cast(type, i2s_expr, loc);
      Type_context tcontext(type, false);
      ret->determine_type(gogo, &tcontext);
      return ret->get_backend(context);
    }
  else if (type->is_string_type() && expr_type->is_slice_type())
    {
      Array_type* a = expr_type->array_type();
      Type* e = a->element_type()->forwarded();
      go_assert(e->integer_type() != NULL);
      go_assert(this->expr_->is_multi_eval_safe());

      Expression* buf;
      if (this->no_escape_ && !this->no_copy_)
        {
          Type* byte_type = Type::lookup_integer_type("uint8");
          Expression* buflen =
            Expression::make_integer_ul(tmp_string_buf_size, NULL, loc);
          Type* array_type = Type::make_array_type(byte_type, buflen);
          buf = Expression::make_allocation(array_type, loc);
          buf->allocation_expression()->set_allocate_on_stack();
          buf->allocation_expression()->set_no_zero();
        }
      else
        buf = Expression::make_nil(loc);

      if (e->integer_type()->is_byte())
        {
	  Expression* ptr =
	    Expression::make_slice_info(this->expr_, SLICE_INFO_VALUE_POINTER,
					loc);
	  Expression* len =
	    Expression::make_slice_info(this->expr_, SLICE_INFO_LENGTH, loc);
          if (this->no_copy_)
            {
              if (gogo->debug_optimization())
                go_debug(loc, "no copy string([]byte)");
              Expression* str = Expression::make_string_value(ptr, len, loc);
              return str->get_backend(context);
            }
	  Expression* ret = Runtime::make_call(gogo, Runtime::SLICEBYTETOSTRING,
					       loc, 3, buf, ptr, len);
	  Type_context tcontext(type, false);
	  ret->determine_type(gogo, &tcontext);
	  return ret->get_backend(context);
        }
      else
        {
          go_assert(e->integer_type()->is_rune());
	  Expression* ret = Runtime::make_call(gogo, Runtime::SLICERUNETOSTRING,
					       loc, 2, buf, this->expr_);
	  Type_context tcontext(type, false);
	  ret->determine_type(gogo, &tcontext);
	  return ret->get_backend(context);
	}
    }
  else if (type->is_slice_type() && expr_type->is_string_type())
    {
      Type* e = type->array_type()->element_type()->forwarded();
      go_assert(e->integer_type() != NULL);

      Runtime::Function code;
      if (e->integer_type()->is_byte())
	code = Runtime::STRINGTOSLICEBYTE;
      else
	{
	  go_assert(e->integer_type()->is_rune());
	  code = Runtime::STRINGTOSLICERUNE;
	}

      Expression* buf;
      if (this->no_escape_)
        {
          Expression* buflen =
            Expression::make_integer_ul(tmp_string_buf_size, NULL, loc);
          Type* array_type = Type::make_array_type(e, buflen);
          buf = Expression::make_allocation(array_type, loc);
          buf->allocation_expression()->set_allocate_on_stack();
          buf->allocation_expression()->set_no_zero();
        }
      else
        buf = Expression::make_nil(loc);
      Expression* s2a = Runtime::make_call(gogo, code, loc, 2, buf,
					   this->expr_);
      Expression* ret = Expression::make_unsafe_cast(type, s2a, loc);
      Type_context tcontext(type, false);
      ret->determine_type(gogo, &tcontext);
      return ret->get_backend(context);
    }
  else if (type->is_numeric_type())
    {
      go_assert(Type::are_convertible(type, expr_type, NULL));
      Bexpression* bexpr = this->expr_->get_backend(context);
      return gogo->backend()->convert_expression(btype, bexpr, loc);
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
      Bexpression* bexpr = this->expr_->get_backend(context);
      return gogo->backend()->convert_expression(btype, bexpr, loc);
    }
  else
    {
      Expression* conversion =
          Expression::convert_for_assignment(gogo, type, this->expr_, loc);
      conversion->determine_type(gogo, &tcontext);
      return conversion->get_backend(context);
    }
}

// Cost of inlining a type conversion.

int
Type_conversion_expression::do_inlining_cost() const
{
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();
  if (type->interface_type() != NULL || expr_type->interface_type() != NULL)
    return 10;
  else if (type->is_string_type() && expr_type->integer_type() != NULL)
    return 10;
  else if (type->is_string_type() && expr_type->is_slice_type())
    return 10;
  else if (type->is_slice_type() && expr_type->is_string_type())
    return 10;
  else
    return 1;
}

// Output a type conversion in a constant expression.

void
Type_conversion_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("$convert(");
  efb->write_type(this->type_);
  efb->write_c_string(", ");

  Type* old_context = efb->type_context();
  efb->set_type_context(this->type_);

  this->expr_->export_expression(efb);

  efb->set_type_context(old_context);

  efb->write_c_string(")");
}

// Import a type conversion or a struct construction.

Expression*
Type_conversion_expression::do_import(Import_expression* imp, Location loc)
{
  imp->require_c_string("$convert(");
  Type* type = imp->read_type();
  imp->require_c_string(", ");
  Expression* val = Expression::import_expression(imp, loc);
  imp->require_c_string(")");
  return Expression::make_cast(type, val, loc);
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

// Class Unsafe_type_conversion_expression.

// Traversal.

int
Unsafe_type_conversion_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->expr_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return whether an unsafe type conversion can be used as a constant
// initializer.

bool
Unsafe_type_conversion_expression::do_is_static_initializer() const
{
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();

  if (type->interface_type() != NULL
      || expr_type->interface_type() != NULL)
    return false;

  if (!this->expr_->is_static_initializer())
    return false;

  if (Type::are_convertible(type, expr_type, NULL))
    return true;

  if (type->is_string_type() && expr_type->is_string_type())
    return true;

  if ((type->is_numeric_type()
       || type->is_boolean_type()
       || type->points_to() != NULL)
      && (expr_type->is_numeric_type()
	  || expr_type->is_boolean_type()
	  || expr_type->points_to() != NULL))
    return true;

  return false;
}

// Copy.

Expression*
Unsafe_type_conversion_expression::do_copy()
{
  return new Unsafe_type_conversion_expression(this->type_->copy_expressions(),
					       this->expr_->copy(),
					       this->location());
}

// Convert to backend representation.

Bexpression*
Unsafe_type_conversion_expression::do_get_backend(Translate_context* context)
{
  // We are only called for a limited number of cases.

  Type* t = this->type_;
  Type* et = this->expr_->type();

  if (t->is_error_type()
      || this->expr_->is_error_expression()
      || et->is_error_type())
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

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
	  return context->backend()->error_expression();
	}

      go_assert(et->struct_type() != NULL
                && Type::are_convertible(t, et, NULL));
    }
  else if (t->map_type() != NULL)
    go_assert(et->map_type() != NULL || et->points_to() != NULL);
  else if (t->channel_type() != NULL)
    go_assert(et->channel_type() != NULL || et->points_to() != NULL);
  else if (t->points_to() != NULL)
    go_assert(et->points_to() != NULL
              || et->channel_type() != NULL
              || et->map_type() != NULL
              || et->function_type() != NULL
	      || et->integer_type() != NULL
              || et->is_nil_type());
  else if (t->function_type() != NULL)
    go_assert(et->points_to() != NULL);
  else if (et->is_unsafe_pointer_type())
    go_assert(t->points_to() != NULL
	      || (t->integer_type() != NULL
		  && t->integer_type() == Type::lookup_integer_type("uintptr")->real_type()));
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
              || et->channel_type() != NULL
	      || et->is_nil_type());
  else
    go_unreachable();

  Gogo* gogo = context->gogo();
  Btype* btype = t->get_backend(gogo);
  Bexpression* bexpr = this->expr_->get_backend(context);
  Location loc = this->location();
  return gogo->backend()->convert_expression(btype, bexpr, loc);
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

// Call the address_taken method of the operand if needed.  This is
// called after escape analysis but before inserting write barriers.

void
Unary_expression::check_operand_address_taken(Gogo*)
{
  if (this->op_ != OPERATOR_AND)
    return;

  // If this->escapes_ is false at this point, then it was set to
  // false by an explicit call to set_does_not_escape, and the value
  // does not escape.  If this->escapes_ is true, we may be able to
  // set it to false based on the escape analysis pass.
  if (this->escapes_)
    {
      Node* n = Node::make_node(this);
      if ((n->encoding() & ESCAPE_MASK) == int(Node::ESCAPE_NONE))
	this->escapes_ = false;
    }

  this->expr_->address_taken(this->escapes_);
}

// If we are taking the address of a composite literal, and the
// contents are not constant, then we want to make a heap expression
// instead.

Expression*
Unary_expression::do_lower(Gogo* gogo, Named_object*, Statement_inserter*)
{
  Location loc = this->location();

  if (this->is_error_expression())
    return Expression::make_error(loc);

  Operator op = this->op_;
  Expression* expr = this->expr_;

  if (expr->is_error_expression())
    return Expression::make_error(loc);

  if (op == OPERATOR_MULT && expr->is_type_expression())
    {
      Expression* ret =
	Expression::make_type(Type::make_pointer_type(expr->type()), loc);
      ret->determine_type_no_context(gogo);
      return ret;
    }

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
		      go_error_at(ue->location(),
				  "invalid operand for unary %<&%>");
		      this->set_is_error();
		    }
		  return ue->expr_;
		}
	      ue->set_does_not_escape();
	    }
	}
    }

  // Check for an invalid pointer dereference.  We need to do this
  // here because Unary_expression::do_type will return an error type
  // in this case.  That can cause code to appear erroneous, and
  // therefore disappear at lowering time, without any error message.
  if (op == OPERATOR_MULT && expr->type()->points_to() == NULL)
    {
      this->report_error(_("expected pointer"));
      return Expression::make_error(this->location());
    }

  if (op == OPERATOR_PLUS || op == OPERATOR_MINUS || op == OPERATOR_XOR)
    {
      Numeric_constant nc;
      if (expr->numeric_constant_value(&nc))
	{
	  Numeric_constant result;
	  bool issued_error;
	  if (Unary_expression::eval_constant(this->type_, op, &nc, loc,
					      &result, &issued_error))
	    {
	      Expression* ret = result.expression(loc);
	      Type_context subcontext(this->type_, this->type_->is_abstract());
	      ret->determine_type(gogo, &subcontext);
	      ret->check_types(gogo);
	      return ret;
	    }
	  else if (issued_error)
	    return Expression::make_error(this->location());
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
  if (this->is_error_expression()
      || this->expr_->is_error_expression()
      || this->expr_->type()->is_error_type())
    {
      go_assert(saw_errors());
      return Expression::make_error(this->location());
    }

  Location location = this->location();
  if (this->op_ == OPERATOR_MULT
      && !this->expr_->is_multi_eval_safe())
    {
      go_assert(this->expr_->type()->points_to() != NULL);
      switch (this->requires_nil_check(gogo))
        {
          case NIL_CHECK_ERROR_ENCOUNTERED:
            {
              go_assert(saw_errors());
              return Expression::make_error(this->location());
            }
          case NIL_CHECK_NOT_NEEDED:
            break;
          case NIL_CHECK_NEEDED:
            this->create_temp_ = true;
            break;
          case NIL_CHECK_DEFAULT:
            go_unreachable();
        }
    }

  if (this->create_temp_ && !this->expr_->is_multi_eval_safe())
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
  if (this->op_ == OPERATOR_MULT || this->op_ == OPERATOR_AND)
    {
      // These are not constant by Go language rules.
      return false;
    }
  else
    return this->expr_->is_constant();
}

bool
Unary_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);

  if (this->op_ == OPERATOR_MULT || this->op_ == OPERATOR_AND)
    return false;
  return this->expr_->is_untyped(ptype);
}

// Return whether a unary expression can be used as a constant
// initializer.

bool
Unary_expression::do_is_static_initializer() const
{
  if (this->op_ == OPERATOR_MULT)
    return false;
  else if (this->op_ == OPERATOR_AND)
    return Unary_expression::base_is_static_initializer(this->expr_);
  else
    return this->expr_->is_static_initializer();
}

// Return whether the address of EXPR can be used as a static
// initializer.

bool
Unary_expression::base_is_static_initializer(Expression* expr)
{
  // The address of a field reference can be a static initializer if
  // the base can be a static initializer.
  Field_reference_expression* fre = expr->field_reference_expression();
  if (fre != NULL)
    return Unary_expression::base_is_static_initializer(fre->expr());

  // The address of an index expression can be a static initializer if
  // the base can be a static initializer and the index is constant.
  Array_index_expression* aind = expr->array_index_expression();
  if (aind != NULL)
    return (aind->end() == NULL
	    && aind->start()->is_constant()
	    && Unary_expression::base_is_static_initializer(aind->array()));

  // The address of a global variable can be a static initializer.
  Var_expression* ve = expr->var_expression();
  if (ve != NULL)
    {
      Named_object* no = ve->named_object();
      return no->is_variable() && no->var_value()->is_global();
    }

  // The address of a composite literal can be used as a static
  // initializer if the composite literal is itself usable as a
  // static initializer.
  if (expr->is_composite_literal() && expr->is_static_initializer())
    return true;

  // The address of a string constant can be used as a static
  // initializer.  This can not be written in Go itself but this is
  // used when building a type descriptor.
  if (expr->string_expression() != NULL)
    return true;

  return false;
}

// Return whether this dereference expression requires an explicit nil
// check. If we are dereferencing the pointer to a large struct
// (greater than the specified size threshold), we need to check for
// nil. We don't bother to check for small structs because we expect
// the system to crash on a nil pointer dereference. However, if we
// know the address of this expression is being taken, we must always
// check for nil.
Unary_expression::Nil_check_classification
Unary_expression::requires_nil_check(Gogo* gogo)
{
  go_assert(this->op_ == OPERATOR_MULT);
  go_assert(this->expr_->type()->points_to() != NULL);

  if (this->issue_nil_check_ == NIL_CHECK_NEEDED)
    return NIL_CHECK_NEEDED;
  else if (this->issue_nil_check_ == NIL_CHECK_NOT_NEEDED)
    return NIL_CHECK_NOT_NEEDED;

  Type* ptype = this->expr_->type()->points_to();
  int64_t type_size = -1;
  if (!ptype->is_void_type())
    {
      bool ok = ptype->backend_type_size(gogo, &type_size);
      if (!ok)
        return NIL_CHECK_ERROR_ENCOUNTERED;
    }

  int64_t size_cutoff = gogo->nil_check_size_threshold();
  if (size_cutoff == -1 || (type_size != -1 && type_size >= size_cutoff))
    this->issue_nil_check_ = NIL_CHECK_NEEDED;
  else
    this->issue_nil_check_ = NIL_CHECK_NOT_NEEDED;
  return this->issue_nil_check_;
}

// Apply unary opcode OP to UNC, setting NC.  Return true if this
// could be done, false if not.  On overflow, issues an error and sets
// *ISSUED_ERROR.

bool
Unary_expression::eval_constant(Type* type, Operator op,
				const Numeric_constant* unc,
				Location location, Numeric_constant* nc,
				bool* issued_error)
{
  *issued_error = false;
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
	  mpfr_neg(val, uval, MPFR_RNDN);
	  Type* utype = unc->type();
	  if (type != NULL
	      && type->is_abstract()
	      && type->is_numeric_type())
	    utype = type;
	  nc->set_float(utype, val);
	  mpfr_clear(uval);
	  mpfr_clear(val);
	  return true;
	}
      else if (unc->is_complex())
	{
	  mpc_t uval;
	  unc->get_complex(&uval);
	  mpc_t val;
	  mpc_init2(val, mpc_precision);
	  mpc_neg(val, uval, MPC_RNDNN);
	  Type* utype = unc->type();
	  if (type != NULL
	      && type->is_abstract()
	      && type->is_numeric_type())
	    utype = type;
	  nc->set_complex(utype, val);
	  mpc_clear(uval);
	  mpc_clear(val);
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

  if (!nc->set_type(unc->type(), true, location))
    {
      *issued_error = true;
      return false;
    }
  return true;
}

// Return the integral constant value of a unary expression, if it has one.

bool
Unary_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->is_error_expression())
    return false;

  Numeric_constant unc;
  if (!this->expr_->numeric_constant_value(&unc))
    return false;
  bool issued_error;
  bool r = Unary_expression::eval_constant(this->type_, this->op_, &unc,
					   this->location(), nc,
					   &issued_error);
  if (issued_error)
    this->set_is_error();
  return r;
}

// Return the boolean constant value of a unary expression, if it has one.

bool
Unary_expression::do_boolean_constant_value(bool* val)
{
  if (this->op_ == OPERATOR_NOT
      && this->expr_->boolean_constant_value(val))
    {
      *val = !*val;
      return true;
    }
  return false;
}

// Return the type of a unary expression.

Type*
Unary_expression::do_type()
{
  if (this->type_ == NULL)
    {
      switch (this->op_)
	{
	case OPERATOR_AND:
	  return Type::make_pointer_type(this->expr_->type());

	case OPERATOR_MULT:
	  {
	    if (this->expr_->is_type_expression())
	      return Type::make_pointer_type(this->expr_->type());

	    Type* subtype = this->expr_->type();
	    Type* points_to = subtype->points_to();
	    if (points_to == NULL)
	      {
		this->report_error(_("expected pointer"));
		this->type_ = Type::make_error_type();
		return this->type_;
	      }
	    return points_to;
	  }

	default:
	  go_assert(saw_errors());
	  return Type::make_error_type();
	}
    }

  return this->type_;
}

// Determine abstract types for a unary expression.

void
Unary_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  switch (this->op_)
    {
    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
    case OPERATOR_NOT:
    case OPERATOR_XOR:
      {
	if (this->type_ != NULL)
	  return;

	Type* dummy;
	Type_context subcontext(*context);
	if (this->expr_->is_untyped(&dummy) && this->expr_->is_constant())
	  {
	    // We evaluate an untyped operator as untyped.  Then we
	    // convert it to the desired type.  Otherwise we may, for
	    // example, give a useless error for one more than the
	    // most positive integer when it is the operand of a unary
	    // minus.
	    subcontext.type = NULL;
	    subcontext.may_be_abstract = true;
	  }
	this->expr_->determine_type(gogo, &subcontext);

	this->type_ = this->expr_->type();

	// If this is an untyped expression in a typed context, use
	// the context type.  If this doesn't work we'll report an
	// error later.
	if (this->type_->is_abstract()
	    && !context->may_be_abstract
	    && context->type != NULL)
	  {
	    if (context->type->interface_type() == NULL)
	      this->type_ = context->type;
	    else
	      this->type_ = this->type_->make_non_abstract_type();
	  }
      }
      break;

    case OPERATOR_AND:
      // Taking the address of something.
      {
	Type* subtype = (context->type == NULL
			 ? NULL
			 : context->type->points_to());
	Type_context subcontext(subtype, false);
	this->expr_->determine_type(gogo, &subcontext);
      }
      break;

    case OPERATOR_MULT:
      {
	if (this->expr_->is_type_expression())
	  {
	    this->expr_->determine_type_no_context(gogo);
	    return;
	  }

	// Indirecting through a pointer.
	Type* subtype = (context->type == NULL
			 ? NULL
			 : Type::make_pointer_type(context->type));
	Type_context subcontext(subtype, false);
	this->expr_->determine_type(gogo, &subcontext);
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
  if (this->is_error_expression())
    return;

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
      if (type->integer_type() == NULL)
	this->report_error(_("expected integer"));
      break;

    case OPERATOR_AND:
      if (!this->expr_->is_addressable())
	{
	  if (!this->create_temp_)
	    {
	      go_error_at(this->location(), "invalid operand for unary %<&%>");
	      this->set_is_error();
	    }
	}
      else
	this->expr_->issue_nil_check();
      break;

    case OPERATOR_MULT:
      if (this->expr_->is_type_expression())
	break;

      // Catching an invalid indirection of unsafe.Pointer here avoid
      // having to deal with TYPE_VOID in other places.
      if (this->expr_->type()->is_unsafe_pointer_type())
	{
	  go_error_at(this->location(),
		      "invalid indirect of %<unsafe.Pointer%>");
	  this->set_is_error();
	  return;
	}

      // Indirecting through a pointer.
      if (type->points_to() == NULL)
	this->report_error(_("expected pointer"));
      if (type->points_to()->is_error())
	this->set_is_error();
      break;

    default:
      go_unreachable();
    }
}

// Get the backend representation for a unary expression.

Bexpression*
Unary_expression::do_get_backend(Translate_context* context)
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
          Bexpression* bvar_expr =
              gogo->backend()->var_expression(bvar, loc);
          Bexpression* bval = sut->expression()->get_backend(context);

          Named_object* fn = context->function();
          go_assert(fn != NULL);
          Bfunction* bfn =
              fn->func_value()->get_or_make_decl(gogo, fn);
          Bstatement* bassign =
              gogo->backend()->assignment_statement(bfn, bvar_expr, bval, loc);
          Bexpression* bvar_addr =
              gogo->backend()->address_expression(bvar_expr, loc);
	  return gogo->backend()->compound_expression(bassign, bvar_addr, loc);
	}
    }

  Bexpression* ret;
  Bexpression* bexpr = this->expr_->get_backend(context);
  Btype* btype = (this->type_ == NULL
		  ? this->expr_->type()->get_backend(gogo)
		  : this->type_->get_backend(gogo));
  switch (this->op_)
    {
    case OPERATOR_PLUS:
      ret = gogo->backend()->convert_expression(btype, bexpr, loc);
      break;

    case OPERATOR_MINUS:
      ret = gogo->backend()->unary_expression(this->op_, bexpr, loc);
      ret = gogo->backend()->convert_expression(btype, ret, loc);
      break;

    case OPERATOR_NOT:
    case OPERATOR_XOR:
      ret = gogo->backend()->unary_expression(this->op_, bexpr, loc);
      ret = gogo->backend()->convert_expression(btype, ret, loc);
      break;

    case OPERATOR_AND:
      if (!this->create_temp_)
	{
	  // We should not see a non-constant constructor here; cases
	  // where we would see one should have been moved onto the
	  // heap at parse time.  Taking the address of a nonconstant
	  // constructor will not do what the programmer expects.

          go_assert(!this->expr_->is_composite_literal()
                    || this->expr_->is_static_initializer());
	  if (this->expr_->classification() == EXPRESSION_UNARY)
	    {
	      Unary_expression* ue =
		static_cast<Unary_expression*>(this->expr_);
	      go_assert(ue->op() != OPERATOR_AND);
	    }
	}

      if (this->is_gc_root_ || this->is_slice_init_)
	{
	  std::string var_name;
	  bool copy_to_heap = false;
	  if (this->is_gc_root_)
	    {
	      // Build a decl for a GC root variable.  GC roots are mutable, so
	      // they cannot be represented as an immutable_struct in the
	      // backend.
	      var_name = gogo->gc_root_name();
	    }
	  else
	    {
	      // Build a decl for a slice value initializer.  An immutable slice
	      // value initializer may have to be copied to the heap if it
	      // contains pointers in a non-constant context.
	      var_name = gogo->initializer_name();

	      Array_type* at = this->expr_->type()->array_type();
	      go_assert(at != NULL);

	      // If we are not copying the value to the heap, we will only
	      // initialize the value once, so we can use this directly
	      // rather than copying it.  In that case we can't make it
	      // read-only, because the program is permitted to change it.
	      copy_to_heap = (context->function() != NULL
                              || context->is_const());
	    }
	  unsigned int flags = (Backend::variable_is_hidden
				| Backend::variable_address_is_taken);
	  if (copy_to_heap)
	    flags |= Backend::variable_is_constant;
	  Bvariable* implicit =
	    gogo->backend()->implicit_variable(var_name, "", btype, flags, 0);
	  gogo->backend()->implicit_variable_set_init(implicit, var_name, btype,
						      flags, bexpr);
	  bexpr = gogo->backend()->var_expression(implicit, loc);

	  // If we are not copying a slice initializer to the heap,
	  // then it can be changed by the program, so if it can
	  // contain pointers we must register it as a GC root.
	  if (this->is_slice_init_
	      && !copy_to_heap
	      && this->expr_->type()->has_pointer())
	    {
	      Bexpression* root =
                  gogo->backend()->var_expression(implicit, loc);
	      root = gogo->backend()->address_expression(root, loc);
	      Type* type = Type::make_pointer_type(this->expr_->type());
	      gogo->add_gc_root(Expression::make_backend(root, type, loc));
	    }
	}
      else if ((this->expr_->is_composite_literal()
		|| this->expr_->string_expression() != NULL)
	       && this->expr_->is_static_initializer())
        {
	  std::string var_name(gogo->initializer_name());
	  unsigned int flags = (Backend::variable_is_hidden
				| Backend::variable_address_is_taken);
          Bvariable* decl =
	    gogo->backend()->immutable_struct(var_name, "", flags, btype, loc);
          gogo->backend()->immutable_struct_set_init(decl, var_name, flags,
						     btype, loc, bexpr);
          bexpr = gogo->backend()->var_expression(decl, loc);
        }
      else if (this->expr_->is_constant())
        {
          std::string var_name(gogo->initializer_name());
	  unsigned int flags = (Backend::variable_is_hidden
				| Backend::variable_is_constant
				| Backend::variable_address_is_taken);
          Bvariable* decl =
	    gogo->backend()->implicit_variable(var_name, "", btype, flags, 0);
          gogo->backend()->implicit_variable_set_init(decl, var_name, btype,
						      flags, bexpr);
          bexpr = gogo->backend()->var_expression(decl, loc);
        }

      go_assert(!this->create_temp_ || this->expr_->is_multi_eval_safe());
      ret = gogo->backend()->address_expression(bexpr, loc);
      break;

    case OPERATOR_MULT:
      {
        go_assert(this->expr_->type()->points_to() != NULL);

        Type* ptype = this->expr_->type()->points_to();
        Btype* pbtype = ptype->get_backend(gogo);
        switch (this->requires_nil_check(gogo))
          {
            case NIL_CHECK_NOT_NEEDED:
              break;
            case NIL_CHECK_ERROR_ENCOUNTERED:
              {
                go_assert(saw_errors());
                return gogo->backend()->error_expression();
              }
            case NIL_CHECK_NEEDED:
              {
                go_assert(this->expr_->is_multi_eval_safe());

                // If we're nil-checking the result of a set-and-use-temporary
                // expression, then pick out the target temp and use that
                // for the final result of the conditional.
                Bexpression* tbexpr = bexpr;
                Bexpression* ubexpr = bexpr;
                Set_and_use_temporary_expression* sut =
                    this->expr_->set_and_use_temporary_expression();
                if (sut != NULL) {
                  Temporary_statement* temp = sut->temporary();
                  Bvariable* bvar = temp->get_backend_variable(context);
                  ubexpr = gogo->backend()->var_expression(bvar, loc);
                }
                Bexpression* nil =
                    Expression::make_nil(loc)->get_backend(context);
                Bexpression* compare =
                    gogo->backend()->binary_expression(OPERATOR_EQEQ, tbexpr,
                                                       nil, loc);
		Expression* crash = Runtime::make_call(gogo, Runtime::PANIC_MEM,
						       loc, 0);
		crash->determine_type_no_context(gogo);
		Bexpression* bcrash = crash->get_backend(context);
                Bfunction* bfn = context->function()->func_value()->get_decl();
                bexpr = gogo->backend()->conditional_expression(bfn, btype,
                                                                compare,
                                                                bcrash, ubexpr,
                                                                loc);
                break;
              }
            case NIL_CHECK_DEFAULT:
              go_unreachable();
          }
        ret = gogo->backend()->indirect_expression(pbtype, bexpr, false, loc);
      }
      break;

    default:
      go_unreachable();
    }

  return ret;
}

// Export a unary expression.

void
Unary_expression::do_export(Export_function_body* efb) const
{
  switch (this->op_)
    {
    case OPERATOR_PLUS:
      efb->write_c_string("+");
      break;
    case OPERATOR_MINUS:
      efb->write_c_string("-");
      break;
    case OPERATOR_NOT:
      efb->write_c_string("!");
      break;
    case OPERATOR_XOR:
      efb->write_c_string("^");
      break;
    case OPERATOR_AND:
      efb->write_c_string("&");
      break;
    case OPERATOR_MULT:
      efb->write_c_string("*");
      break;
    default:
      go_unreachable();
    }
  this->expr_->export_expression(efb);
}

// Import a unary expression.

Expression*
Unary_expression::do_import(Import_expression* imp, Location loc)
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
    case '&':
      op = OPERATOR_AND;
      break;
    case '*':
      op = OPERATOR_MULT;
      break;
    default:
      go_unreachable();
    }
  if (imp->version() < EXPORT_FORMAT_V3)
    imp->require_c_string(" ");
  Expression* expr = Expression::import_expression(imp, loc);
  return Expression::make_unary(op, expr, loc);
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

Expression*
Expression::make_dereference(Expression* ptr,
                             Nil_check_classification docheck,
                             Location location)
{
  Expression* deref = Expression::make_unary(OPERATOR_MULT, ptr, location);
  if (docheck == NIL_CHECK_NEEDED)
    deref->unary_expression()->set_requires_nil_check(true);
  else if (docheck == NIL_CHECK_NOT_NEEDED)
    deref->unary_expression()->set_requires_nil_check(false);
  return deref;
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

// Return whether a binary expression is untyped.

bool
Binary_expression::do_is_untyped(Type** ptype) const
{
  if (this->type_ != NULL)
    return Expression::is_untyped_type(this->type_, ptype);

  switch (this->op_)
    {
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      // Comparisons are untyped by default.
      *ptype = Type::make_boolean_type();
      return true;

    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      // A shift operation is untyped if the left hand expression is
      // untyped.  The right hand expression is irrelevant.
      return this->left_->is_untyped(ptype);

    default:
      break;
    }

  Type* tleft;
  Type* tright;
  if (!this->left_->is_untyped(&tleft)
      || !this->right_->is_untyped(&tright))
    return false;

  // If both sides are numeric, pick a type based on the kind.
  enum kind { INT, RUNE, FLOAT, COMPLEX };
  enum kind kleft, kright;

  if (tleft->integer_type() != NULL)
    kleft = tleft->integer_type()->is_rune() ? RUNE : INT;
  else if (tleft->float_type() != NULL)
    kleft = FLOAT;
  else if (tleft->complex_type() != NULL)
    kleft = COMPLEX;
  else
    {
      // Not numeric.  If the types are different, we will report an
      // error later.
      *ptype = tleft;
      return true;
    }

  if (tright->integer_type() != NULL)
    kright = tright->integer_type()->is_rune() ? RUNE : INT;
  else if (tright->float_type() != NULL)
    kright = FLOAT;
  else if (tright->complex_type() != NULL)
    kright = COMPLEX;
  else
    {
      // Types are different.  We will report an error later.
      *ptype = tleft;
      return true;
    }

  if (kleft > kright)
    *ptype = tleft;
  else
    *ptype = tright;

  return true;
}

// Return whether this expression may be used as a static initializer.

bool
Binary_expression::do_is_static_initializer() const
{
  if (!this->left_->is_static_initializer()
      || !this->right_->is_static_initializer())
    return false;

  // Addresses can be static initializers, but we can't implement
  // arbitray binary expressions of them.
  Unary_expression* lu = this->left_->unary_expression();
  Unary_expression* ru = this->right_->unary_expression();
  if (lu != NULL && lu->op() == OPERATOR_AND)
    {
      if (ru != NULL && ru->op() == OPERATOR_AND)
	return this->op_ == OPERATOR_MINUS;
      else
	return this->op_ == OPERATOR_PLUS || this->op_ == OPERATOR_MINUS;
    }
  else if (ru != NULL && ru->op() == OPERATOR_AND)
    return this->op_ == OPERATOR_PLUS || this->op_ == OPERATOR_MINUS;

  // Other cases should resolve in the backend.
  return true;
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
      mpfr_prec_round(left_val, bits, MPFR_RNDN);
      mpfr_prec_round(right_val, bits, MPFR_RNDN);
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
  mpc_t left_val;
  if (!left_nc->to_complex(&left_val))
    return false;
  mpc_t right_val;
  if (!right_nc->to_complex(&right_val))
    {
      mpc_clear(left_val);
      return false;
    }

  // We already coerced both operands to the same type.  If that type
  // is not an abstract type, we need to round the values accordingly.
  Type* type = left_nc->type();
  if (!type->is_abstract() && type->complex_type() != NULL)
    {
      int bits = type->complex_type()->bits();
      mpfr_prec_round(mpc_realref(left_val), bits / 2, MPFR_RNDN);
      mpfr_prec_round(mpc_imagref(left_val), bits / 2, MPFR_RNDN);
      mpfr_prec_round(mpc_realref(right_val), bits / 2, MPFR_RNDN);
      mpfr_prec_round(mpc_imagref(right_val), bits / 2, MPFR_RNDN);
    }

  *cmp = mpc_cmp(left_val, right_val) != 0;

  mpc_clear(left_val);
  mpc_clear(right_val);

  return true;
}

// Apply binary opcode OP to LEFT_NC and RIGHT_NC, setting NC.  Return
// true if this could be done, false if not.  Issue errors at LOCATION
// as appropriate, and sets *ISSUED_ERROR if it did.

bool
Binary_expression::eval_constant(Operator op, Numeric_constant* left_nc,
				 Numeric_constant* right_nc,
				 Location location, Numeric_constant* nc,
				 bool* issued_error)
{
  *issued_error = false;
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
  if (is_shift
      && right_type->integer_type() == NULL
      && !right_type->is_abstract())
    return false;

  bool r;
  if (type->complex_type() != NULL)
    r = Binary_expression::eval_complex(op, left_nc, right_nc, location, nc,
					issued_error);
  else if (type->float_type() != NULL)
    r = Binary_expression::eval_float(op, left_nc, right_nc, location, nc,
				      issued_error);
  else
    r = Binary_expression::eval_integer(op, left_nc, right_nc, location, nc,
					issued_error);

  if (r)
    {
      r = nc->set_type(type, true, location);
      if (!r)
	*issued_error = true;
    }

  return r;
}

// Apply binary opcode OP to LEFT_NC and RIGHT_NC, setting NC, using
// integer operations.  Return true if this could be done, false if
// not.

bool
Binary_expression::eval_integer(Operator op, const Numeric_constant* left_nc,
				const Numeric_constant* right_nc,
				Location location, Numeric_constant* nc,
				bool* issued_error)
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
	  go_error_at(location, "constant addition overflow");
          nc->set_invalid();
	  mpz_set_ui(val, 1);
	  *issued_error = true;
	}
      break;
    case OPERATOR_MINUS:
      mpz_sub(val, left_val, right_val);
      if (mpz_sizeinbase(val, 2) > 0x100000)
	{
	  go_error_at(location, "constant subtraction overflow");
          nc->set_invalid();
	  mpz_set_ui(val, 1);
	  *issued_error = true;
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
	  go_error_at(location, "constant multiplication overflow");
          nc->set_invalid();
	  mpz_set_ui(val, 1);
	  *issued_error = true;
	}
      break;
    case OPERATOR_DIV:
      if (mpz_sgn(right_val) != 0)
	mpz_tdiv_q(val, left_val, right_val);
      else
	{
	  go_error_at(location, "division by zero");
          nc->set_invalid();
	  mpz_set_ui(val, 0);
	  *issued_error = true;
	}
      break;
    case OPERATOR_MOD:
      if (mpz_sgn(right_val) != 0)
	mpz_tdiv_r(val, left_val, right_val);
      else
	{
	  go_error_at(location, "division by zero");
          nc->set_invalid();
	  mpz_set_ui(val, 0);
	  *issued_error = true;
	}
      break;
    case OPERATOR_LSHIFT:
      {
	unsigned long shift = mpz_get_ui(right_val);
	if (mpz_cmp_ui(right_val, shift) == 0 && shift <= 0x100000)
	  mpz_mul_2exp(val, left_val, shift);
	else
	  {
	    go_error_at(location, "shift count overflow");
            nc->set_invalid();
	    mpz_set_ui(val, 1);
	    *issued_error = true;
	  }
	break;
      }
      break;
    case OPERATOR_RSHIFT:
      {
	unsigned long shift = mpz_get_ui(right_val);
	if (mpz_cmp_ui(right_val, shift) != 0)
	  {
	    go_error_at(location, "shift count overflow");
            nc->set_invalid();
	    mpz_set_ui(val, 1);
	    *issued_error = true;
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
			      Location location, Numeric_constant* nc,
			      bool* issued_error)
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
      mpfr_add(val, left_val, right_val, MPFR_RNDN);
      break;
    case OPERATOR_MINUS:
      mpfr_sub(val, left_val, right_val, MPFR_RNDN);
      break;
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_AND:
    case OPERATOR_BITCLEAR:
    case OPERATOR_MOD:
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      mpfr_set_ui(val, 0, MPFR_RNDN);
      ret = false;
      break;
    case OPERATOR_MULT:
      mpfr_mul(val, left_val, right_val, MPFR_RNDN);
      break;
    case OPERATOR_DIV:
      if (!mpfr_zero_p(right_val))
	mpfr_div(val, left_val, right_val, MPFR_RNDN);
      else
	{
	  go_error_at(location, "division by zero");
          nc->set_invalid();
	  mpfr_set_ui(val, 0, MPFR_RNDN);
	  *issued_error = true;
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
				Location location, Numeric_constant* nc,
				bool* issued_error)
{
  mpc_t left_val;
  if (!left_nc->to_complex(&left_val))
    return false;
  mpc_t right_val;
  if (!right_nc->to_complex(&right_val))
    {
      mpc_clear(left_val);
      return false;
    }

  mpc_t val;
  mpc_init2(val, mpc_precision);

  bool ret = true;
  switch (op)
    {
    case OPERATOR_PLUS:
      mpc_add(val, left_val, right_val, MPC_RNDNN);
      break;
    case OPERATOR_MINUS:
      mpc_sub(val, left_val, right_val, MPC_RNDNN);
      break;
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_AND:
    case OPERATOR_BITCLEAR:
    case OPERATOR_MOD:
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      mpc_set_ui(val, 0, MPC_RNDNN);
      ret = false;
      break;
    case OPERATOR_MULT:
      mpc_mul(val, left_val, right_val, MPC_RNDNN);
      break;
    case OPERATOR_DIV:
      if (mpc_cmp_si(right_val, 0) == 0)
	{
	  go_error_at(location, "division by zero");
          nc->set_invalid();
	  mpc_set_ui(val, 0, MPC_RNDNN);
	  *issued_error = true;
	  break;
	}
      mpc_div(val, left_val, right_val, MPC_RNDNN);
      break;
    default:
      go_unreachable();
    }

  mpc_clear(left_val);
  mpc_clear(right_val);

  nc->set_complex(NULL, val);
  mpc_clear(val);

  return ret;
}

// Lower a binary expression.  We have to evaluate constant
// expressions now, in order to implement Go's unlimited precision
// constants.

Expression*
Binary_expression::do_lower(Gogo* gogo, Named_object*,
			    Statement_inserter* inserter)
{
  Location location = this->location();

  if (this->is_error_expression())
    return Expression::make_error(location);

  Operator op = this->op_;
  Expression* left = this->left_;
  Expression* right = this->right_;

  if (left->is_error_expression() || right->is_error_expression())
    return Expression::make_error(location);

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
	Expression* ret;
	if (is_comparison)
	  {
	    bool result;
	    if (!Binary_expression::compare_constant(op, &left_nc,
						     &right_nc, location,
						     &result))
	      return this;
	    ret = Expression::make_boolean(result, location);
	  }
	else
	  {
	    Numeric_constant nc;
	    bool issued_error;
	    if (!Binary_expression::eval_constant(op, &left_nc, &right_nc,
						  location, &nc,
						  &issued_error))
	      {
		if (issued_error)
		  return Expression::make_error(location);
                return this;
	      }
	    ret = nc.expression(location);
	  }

	Type_context subcontext(this->type_, this->type_->is_abstract());
	ret->determine_type(gogo, &subcontext);
	ret->check_types(gogo);
	return ret;
      }
  }

  // String constant expressions.
  //
  // Avoid constant folding here if the left and right types are incompatible
  // (leave the operation intact so that the type checker can complain about it
  // later on). If concatenating an abstract string with a named string type,
  // result type needs to be of the named type (see issue 31412).
  if (left->type()->is_string_type()
      && right->type()->is_string_type()
      && (left->type()->named_type() == NULL
          || right->type()->named_type() == NULL
          || left->type()->named_type() == right->type()->named_type()))
    {
      std::string left_string;
      std::string right_string;
      if (left->string_constant_value(&left_string)
	  && right->string_constant_value(&right_string))
	{
	  Expression* ret = NULL;
	  if (op == OPERATOR_PLUS)
            {
	      delete left;
	      delete right;
              ret = Expression::make_string_typed(left_string + right_string,
						  this->type_, location);
            }
	  else if (is_comparison)
	    {
	      int cmp = left_string.compare(right_string);
	      bool r = Binary_expression::cmp_to_bool(op, cmp);
	      delete left;
	      delete right;
	      ret = Expression::make_boolean(r, location);
	    }

	  if (ret != NULL)
	    {
	      Type_context subcontext(this->type_, this->type_->is_abstract());
	      ret->determine_type(gogo, &subcontext);
	      ret->check_types(gogo);
	      return ret;
	    }
	}
    }

  // Lower struct, array, and some interface comparisons.
  if (op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ)
    {
      if (left->type()->struct_type() != NULL
	  && right->type()->struct_type() != NULL)
	return this->lower_struct_comparison(gogo, inserter);
      else if (left->type()->array_type() != NULL
	       && !left->type()->is_slice_type()
	       && right->type()->array_type() != NULL
	       && !right->type()->is_slice_type())
	return this->lower_array_comparison(gogo, inserter);
      else if ((left->type()->interface_type() != NULL
                && right->type()->interface_type() == NULL)
               || (left->type()->interface_type() == NULL
                   && right->type()->interface_type() != NULL))
	return this->lower_interface_value_comparison(gogo, inserter);
    }

  // Lower string concatenation to String_concat_expression, so that
  // we can group sequences of string additions.
  if (this->left_->type()->is_string_type() && this->op_ == OPERATOR_PLUS)
    {
      Expression_list* exprs;
      String_concat_expression* left_sce =
	this->left_->string_concat_expression();
      if (left_sce != NULL)
	exprs = left_sce->exprs();
      else
	{
	  exprs = new Expression_list();
	  exprs->push_back(this->left_);
	}

      String_concat_expression* right_sce =
	this->right_->string_concat_expression();
      if (right_sce != NULL)
	exprs->append(right_sce->exprs());
      else
	exprs->push_back(this->right_);

      return Expression::make_string_concat(exprs);
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
  if (st != st2
      && !Type::are_identical(st, st2,
			      Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			      NULL))
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

  ret->determine_type_no_context(gogo);

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
  if (at != at2
      && !Type::are_identical(at, at2,
			      Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			      NULL))
    return this;
  if (!Type::are_compatible_for_comparison(true, this->left_->type(),
					   this->right_->type(), NULL))
    return this;

  // Call memcmp directly if possible.  This may let the middle-end
  // optimize the call.
  if (at->compare_is_identity(gogo))
    return this->lower_compare_to_memcmp(gogo, inserter);

  // Call the array comparison function.
  Named_object* equal_fn =
    at->equal_function(gogo, this->left_->type()->named_type(), NULL);

  Location loc = this->location();

  Expression* func = Expression::make_func_reference(equal_fn, NULL, loc);

  Expression_list* args = new Expression_list();
  args->push_back(this->operand_address(inserter, this->left_));
  args->push_back(this->operand_address(inserter, this->right_));

  Call_expression* ce = Expression::make_call(func, args, false, loc);

  // Record that this is a call to a generated equality function.  We
  // need to do this because a comparison returns an abstract boolean
  // type, but the function necessarily returns "bool".  The
  // difference shows up in code like
  //     type mybool bool
  //     var b mybool = [10]string{} == [10]string{}
  // The comparison function returns "bool", but since a comparison
  // has an abstract boolean type we need an implicit conversion to
  // "mybool".  The implicit conversion is inserted in
  // Call_expression::do_flatten.
  ce->set_is_equal_function();

  Expression* ret = ce;
  if (this->op_ == OPERATOR_NOTEQ)
    ret = Expression::make_unary(OPERATOR_NOT, ret, loc);

  ret->determine_type_no_context(gogo);

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
Binary_expression::lower_compare_to_memcmp(Gogo* gogo,
					   Statement_inserter* inserter)
{
  Location loc = this->location();

  Expression* a1 = this->operand_address(inserter, this->left_);
  Expression* a2 = this->operand_address(inserter, this->right_);
  Expression* len = Expression::make_type_info(this->left_->type(),
					       TYPE_INFO_SIZE);

  Expression* call = Runtime::make_call(gogo, Runtime::MEMCMP, loc, 3,
					a1, a2, len);
  Type* int32_type = Type::lookup_integer_type("int32");
  Expression* zero = Expression::make_integer_ul(0, int32_type, loc);
  Expression* ret = Expression::make_binary(this->op_, call, zero, loc);
  Type_context context(this->type_, this->type_->is_abstract());
  ret->determine_type(gogo, &context);
  return ret;
}

Expression*
Binary_expression::do_flatten(Gogo* gogo, Named_object*,
                              Statement_inserter* inserter)
{
  Location loc = this->location();
  if (this->left_->type()->is_error_type()
      || this->right_->type()->is_error_type()
      || this->left_->is_error_expression()
      || this->right_->is_error_expression())
    {
      go_assert(saw_errors());
      return Expression::make_error(loc);
    }

  Temporary_statement* temp;

  Type* left_type = this->left_->type();
  bool is_shift_op = (this->op_ == OPERATOR_LSHIFT
                      || this->op_ == OPERATOR_RSHIFT);
  bool is_idiv_op = ((this->op_ == OPERATOR_DIV &&
                      left_type->integer_type() != NULL)
                     || this->op_ == OPERATOR_MOD);
  bool is_string_op = (left_type->is_string_type()
                       && this->right_->type()->is_string_type());

  if (is_string_op)
    {
      // Mark string([]byte) operands to reuse the backing store.
      // String comparison does not keep the reference, so it is safe.
      Type_conversion_expression* lce =
        this->left_->conversion_expression();
      if (lce != NULL && lce->expr()->type()->is_slice_type())
        lce->set_no_copy(true);
      Type_conversion_expression* rce =
        this->right_->conversion_expression();
      if (rce != NULL && rce->expr()->type()->is_slice_type())
        rce->set_no_copy(true);
    }

  if (is_shift_op
      || (is_idiv_op
	  && (gogo->check_divide_by_zero() || gogo->check_divide_overflow()))
      || is_string_op)
    {
      if (!this->left_->is_multi_eval_safe())
        {
          temp = Statement::make_temporary(NULL, this->left_, loc);
          inserter->insert(temp);
          this->left_ = Expression::make_temporary_reference(temp, loc);
        }
      if (!this->right_->is_multi_eval_safe())
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
Binary_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->is_error_expression())
    return false;

  Numeric_constant left_nc;
  if (!this->left_->numeric_constant_value(&left_nc))
    return false;
  Numeric_constant right_nc;
  if (!this->right_->numeric_constant_value(&right_nc))
    return false;
  bool issued_error;
  bool r = Binary_expression::eval_constant(this->op_, &left_nc, &right_nc,
					    this->location(), nc,
					    &issued_error);
  if (issued_error)
    this->set_is_error();
  return r;
}

// Return the boolean constant value, if it has one.

bool
Binary_expression::do_boolean_constant_value(bool* val)
{
  if (this->is_error_expression())
    return false;

  bool is_comparison = false;
  switch (this->op_)
    {
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      is_comparison = true;
      break;
    case OPERATOR_ANDAND:
    case OPERATOR_OROR:
      break;
    default:
      return false;
    }

  Numeric_constant left_nc, right_nc;
  if (is_comparison
      && this->left_->numeric_constant_value(&left_nc)
      && this->right_->numeric_constant_value(&right_nc))
    return Binary_expression::compare_constant(this->op_, &left_nc,
                                               &right_nc,
                                               this->location(),
                                               val);

  std::string left_str, right_str;
  if (is_comparison
      && this->left_->string_constant_value(&left_str)
      && this->right_->string_constant_value(&right_str))
    {
      *val = Binary_expression::cmp_to_bool(this->op_,
                                            left_str.compare(right_str));
      return true;
    }

  bool left_bval;
  if (this->left_->boolean_constant_value(&left_bval))
    {
      if (this->op_ == OPERATOR_ANDAND && !left_bval)
        {
          *val = false;
          return true;
        }
      else if (this->op_ == OPERATOR_OROR && left_bval)
        {
          *val = true;
          return true;
        }

      bool right_bval;
      if (this->right_->boolean_constant_value(&right_bval))
        {
          switch (this->op_)
            {
            case OPERATOR_EQEQ:
              *val = (left_bval == right_bval);
              return true;
            case OPERATOR_NOTEQ:
              *val = (left_bval != right_bval);
              return true;
            case OPERATOR_ANDAND:
            case OPERATOR_OROR:
              *val = right_bval;
              return true;
            default:
              go_unreachable();
            }
        }
    }

  return false;
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
  if (this->type_ == NULL)
    {
      go_assert(saw_errors());
      return Type::make_error_type();
    }

  return this->type_;
}

// Set type for a binary expression.

void
Binary_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  if (this->type_ != NULL)
    return;

  // For a shift operation, the type of the binary expression is the
  // type of the left operand.  If the left operand is a constant,
  // then it gets its type from the context.
  bool is_shift_op = (this->op_ == OPERATOR_LSHIFT
		      || this->op_ == OPERATOR_RSHIFT);

  // For a comparison operation, the type of the binary expression is
  // a boolean type.
  bool is_comparison = (this->op_ == OPERATOR_EQEQ
			|| this->op_ == OPERATOR_NOTEQ
			|| this->op_ == OPERATOR_LT
			|| this->op_ == OPERATOR_LE
			|| this->op_ == OPERATOR_GT
			|| this->op_ == OPERATOR_GE);

  // For constant expressions, the context of the result is not useful in
  // determining the types of the operands.  It is only legal to use abstract
  // boolean, numeric, and string constants as operands where it is legal to
  // use non-abstract boolean, numeric, and string constants, respectively.
  // Any issues with the operation will be resolved in the check_types pass.
  bool left_is_constant = this->left_->is_constant();
  bool right_is_constant = this->right_->is_constant();
  bool is_constant_expr = left_is_constant && right_is_constant;

  Type_context subcontext(*context);
  if (is_comparison)
    subcontext.type = NULL;

  Type* tleft;
  bool left_is_untyped = this->left_->is_untyped(&tleft);
  if (!left_is_untyped)
    {
      this->left_->determine_type(gogo, &subcontext);
      tleft = this->left_->type();
    }

  Type* tright;
  bool right_is_untyped = this->right_->is_untyped(&tright);
  if (!right_is_untyped)
    {
      // For a shift operation, the right operand should always be an
      // integer.
      if (is_shift_op)
	{
	  subcontext.type = Type::lookup_integer_type("uint");
	  subcontext.may_be_abstract = false;
	}

      this->right_->determine_type(gogo, &subcontext);
      tright = this->right_->type();
    }

  // For each operand we have the real type or, if the operand is a
  // untyped, a guess at the type.  Use this to determine the types of
  // untyped operands.

  subcontext = *context;
  if (left_is_untyped && (right_is_untyped || is_shift_op) && is_constant_expr)
    {
      // We evaluate the operands of an untyped expression as untyped
      // values.  Then we convert to the desired type.  Otherwise we
      // may, for example, mishandle a floating-point constant
      // division as an integer division.
      subcontext.type = NULL;
      subcontext.may_be_abstract = true;
    }
  else if (is_comparison)
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
      if (subcontext.type == NULL
	  && right_is_constant
	  && context->may_be_abstract)
	subcontext.type = Type::make_abstract_integer_type();
    }
  else if (!tleft->is_abstract())
    subcontext.type = tleft;
  else if (!tright->is_abstract())
    subcontext.type = tright;
  else if (subcontext.type == NULL)
    {
      if ((tleft->integer_type() != NULL && tright->integer_type() != NULL)
	  || (tleft->float_type() != NULL && tright->float_type() != NULL)
	  || (tleft->complex_type() != NULL && tright->complex_type() != NULL)
	  || (tleft->is_boolean_type() && tright->is_boolean_type()))
	{
	  // Both sides have an abstract integer, abstract float,
	  // abstract complex, or abstract boolean type.  Just let
	  // CONTEXT determine whether they may remain abstract or not.
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
    }

  if (left_is_untyped)
    {
      this->left_->determine_type(gogo, &subcontext);
      tleft = this->left_->type();
    }

  if (is_shift_op)
    {
      // We may have inherited an unusable type for the shift operand.
      // Give a useful error if that happened.
      if (left_is_untyped
	  && !is_constant_expr
	  && subcontext.type != NULL
	  && !subcontext.may_be_abstract
	  && subcontext.type->interface_type() == NULL
	  && subcontext.type->integer_type() == NULL
	  && !tleft->is_error()
	  && !tright->is_error())
	this->report_error(("invalid context-determined non-integer type "
			    "for left operand of shift"));

      // The context for the right hand operand is the same as for the
      // left hand operand, except for a shift operator.
      subcontext.type = Type::lookup_integer_type("uint");
      subcontext.may_be_abstract = false;
    }

  if (right_is_untyped)
    {
      this->right_->determine_type(gogo, &subcontext);
      tright = this->right_->type();
    }

  if (this->left_->is_error_expression()
      || tleft->is_error()
      || this->right_->is_error_expression()
      || tright->is_error())
    {
      this->set_is_error();
      return;
    }

  if (is_comparison)
    {
      if (context->type != NULL && context->type->is_boolean_type())
	this->type_ = context->type;
      else if (!context->may_be_abstract)
	this->type_ = Type::lookup_bool_type();
      else
	this->type_ = Type::make_boolean_type();
    }
  else
    {
      if (is_shift_op)
	{
	  // Shifts only work with integers, so force an abstract
	  // floating-point type (such as 1.0 << 1) into an integer.
	  if (tleft->is_abstract()
	      && tleft->integer_type() == NULL
	      && context->type == NULL)
	    {
	      this->type_ = Type::make_abstract_integer_type();
	      if (!context->may_be_abstract)
		this->type_ = this->type_->make_non_abstract_type();
	    }
	  else
	    this->type_ = tleft;
	}
      else
	{
	  if (!Binary_expression::operation_type(this->op_, tleft, tright,
						 &this->type_))
	    {
	      this->report_error("incompatible types in binary expression");
	      this->type_ = Type::make_error_type();
	      return;
	    }
	}

      // If this is an untyped expression in a typed context, use the
      // context type.  If this doesn't work we'll report an error
      // later.
      if (this->type_->is_abstract()
	  && !context->may_be_abstract
	  && context->type != NULL)
	{
	  if (context->type->interface_type() == NULL
	      && ((this->type_->is_numeric_type()
		   && context->type->is_numeric_type())
		  || (this->type_->is_string_type()
		      && context->type->is_string_type())
		  || (this->type_->is_boolean_type()
		      && context->type->is_boolean_type())))
	    this->type_ = context->type;
	  else if (context->type->interface_type() != NULL)
	    this->type_ = this->type_->make_non_abstract_type();
	}
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
      if (!type->is_boolean_type()
          || !otype->is_boolean_type())
	{
	  go_error_at(location, "expected boolean type");
	  return false;
	}
      break;

    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
      {
	std::string reason;
	if (!Type::are_compatible_for_comparison(true, type, otype, &reason))
	  {
	    go_error_at(location, "%s", reason.c_str());
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
	    go_error_at(location, "%s", reason.c_str());
	    return false;
	  }
      }
      break;

    case OPERATOR_PLUS:
    case OPERATOR_PLUSEQ:
      if ((!type->is_numeric_type() && !type->is_string_type())
          || (!otype->is_numeric_type() && !otype->is_string_type()))
	{
	  go_error_at(location,
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
      if (!type->is_numeric_type() || !otype->is_numeric_type())
	{
	  go_error_at(location, "expected integer, floating, or complex type");
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
      if (type->integer_type() == NULL || otype->integer_type() == NULL)
	{
	  go_error_at(location, "expected integer type");
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
      if (left_type->integer_type() == NULL
	  && !left_type->is_abstract()
	  && !this->is_constant())
	this->report_error(_("shift of non-integer operand"));

      if (right_type->is_string_type())
        this->report_error(_("shift count not integer"));
      else if (!right_type->is_abstract()
	       && right_type->integer_type() == NULL)
	this->report_error(_("shift count not integer"));
      else
	{
	  Numeric_constant nc;
	  if (this->right_->numeric_constant_value(&nc))
	    {
	      mpz_t val;
	      if (!nc.to_int(&val))
		this->report_error(_("shift count not integer"));
	      else
		{
		  if (mpz_sgn(val) < 0)
		    {
		      this->report_error(_("negative shift count"));
		      Location rloc = this->right_->location();
		      this->right_ = Expression::make_integer_ul(0, right_type,
								 rloc);
		    }
		  mpz_clear(val);
		}
	    }
	}
    }
}

// Get the backend representation for a binary expression.

Bexpression*
Binary_expression::do_get_backend(Translate_context* context)
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
      return Expression::comparison(context, this->type_, this->op_,
				    this->left_, this->right_, loc);

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
      // Fall through.
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

  // The only binary operation for string is +, and that should have
  // been converted to a String_concat_expression in do_lower.
  go_assert(!left_type->is_string_type());

  Bexpression* left = this->left_->get_backend(context);
  Bexpression* right = this->right_->get_backend(context);

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
  // We also need to check for a negative shift count.
  if (is_shift_op)
    {
      go_assert(left_type->integer_type() != NULL);
      go_assert(right_type->integer_type() != NULL);

      int bits = left_type->integer_type()->bits();

      Numeric_constant nc;
      unsigned long ul;
      if (!this->right_->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&ul) != Numeric_constant::NC_UL_VALID
	  || ul >= static_cast<unsigned long>(bits))
	{
	  mpz_t bitsval;
	  mpz_init_set_ui(bitsval, bits);
	  Bexpression* bits_expr =
	    gogo->backend()->integer_constant_expression(right_btype, bitsval);
	  Bexpression* compare =
	    gogo->backend()->binary_expression(OPERATOR_LT,
					       right, bits_expr, loc);

	  Bexpression* zero_expr =
	    gogo->backend()->integer_constant_expression(left_btype, zero);
	  overflow = zero_expr;
	  Bfunction* bfn = context->function()->func_value()->get_decl();
	  if (this->op_ == OPERATOR_RSHIFT
	      && !left_type->integer_type()->is_unsigned())
	    {
	      Bexpression* neg_expr =
		gogo->backend()->binary_expression(OPERATOR_LT, left,
						   zero_expr, loc);
	      Bexpression* neg_one_expr =
		gogo->backend()->integer_constant_expression(left_btype,
							     neg_one);
	      overflow = gogo->backend()->conditional_expression(bfn,
								 btype,
								 neg_expr,
								 neg_one_expr,
								 zero_expr,
								 loc);
	    }
	  ret = gogo->backend()->conditional_expression(bfn, btype, compare,
							ret, overflow, loc);
	  mpz_clear(bitsval);
	}

      if (!right_type->integer_type()->is_unsigned()
	  && (!this->right_->numeric_constant_value(&nc)
	      || nc.to_unsigned_long(&ul) != Numeric_constant::NC_UL_VALID))
	{
	  Bexpression* zero_expr =
	    gogo->backend()->integer_constant_expression(right_btype, zero);
	  Bexpression* compare =
	    gogo->backend()->binary_expression(OPERATOR_LT, right, zero_expr,
					       loc);
	  Expression* crash = Runtime::make_call(gogo, Runtime::PANIC_SHIFT,
						 loc, 0);
	  crash->determine_type_no_context(gogo);
	  Bexpression* bcrash = crash->get_backend(context);
	  Bfunction* bfn = context->function()->func_value()->get_decl();
	  ret = gogo->backend()->conditional_expression(bfn, btype, compare,
							bcrash, ret, loc);
	}
    }

  // Add checks for division by zero and division overflow as needed.
  if (is_idiv_op)
    {
      if (gogo->check_divide_by_zero())
	{
	  // right == 0
          Bexpression* zero_expr =
              gogo->backend()->integer_constant_expression(right_btype, zero);
          Bexpression* check =
              gogo->backend()->binary_expression(OPERATOR_EQEQ,
                                                 right, zero_expr, loc);

	  Expression* crash = Runtime::make_call(gogo, Runtime::PANIC_DIVIDE,
						 loc, 0);
	  crash->determine_type_no_context(gogo);
	  Bexpression* bcrash = crash->get_backend(context);

	  // right == 0 ? (panicdivide(), 0) : ret
          Bfunction* bfn = context->function()->func_value()->get_decl();
          ret = gogo->backend()->conditional_expression(bfn, btype,
                                                        check, bcrash,
							ret, loc);
	}

      if (gogo->check_divide_overflow())
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
          Bfunction* bfn = context->function()->func_value()->get_decl();

	  if (type->integer_type()->is_unsigned())
	    {
	      // An unsigned -1 is the largest possible number, so
	      // dividing is always 1 or 0.

              Bexpression* cmp =
                  gogo->backend()->binary_expression(OPERATOR_EQEQ,
                                                     left, right, loc);
	      if (this->op_ == OPERATOR_DIV)
                overflow =
                    gogo->backend()->conditional_expression(bfn, btype, cmp,
                                                            one_expr, zero_expr,
                                                            loc);
	      else
                overflow =
                    gogo->backend()->conditional_expression(bfn, btype, cmp,
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
                  overflow = negate_expr->get_backend(context);
                }
	      else
                overflow = zero_expr;
	    }
          overflow = gogo->backend()->convert_expression(btype, overflow, loc);

	  // right == -1 ? - left : ret
          ret = gogo->backend()->conditional_expression(bfn, btype,
                                                        check, overflow,
                                                        ret, loc);
	}
    }

  mpz_clear(zero);
  mpz_clear(one);
  mpz_clear(neg_one);
  return ret;
}

// Export a binary expression.

void
Binary_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("(");
  this->left_->export_expression(efb);
  switch (this->op_)
    {
    case OPERATOR_OROR:
      efb->write_c_string(" || ");
      break;
    case OPERATOR_ANDAND:
      efb->write_c_string(" && ");
      break;
    case OPERATOR_EQEQ:
      efb->write_c_string(" == ");
      break;
    case OPERATOR_NOTEQ:
      efb->write_c_string(" != ");
      break;
    case OPERATOR_LT:
      efb->write_c_string(" < ");
      break;
    case OPERATOR_LE:
      efb->write_c_string(" <= ");
      break;
    case OPERATOR_GT:
      efb->write_c_string(" > ");
      break;
    case OPERATOR_GE:
      efb->write_c_string(" >= ");
      break;
    case OPERATOR_PLUS:
      efb->write_c_string(" + ");
      break;
    case OPERATOR_MINUS:
      efb->write_c_string(" - ");
      break;
    case OPERATOR_OR:
      efb->write_c_string(" | ");
      break;
    case OPERATOR_XOR:
      efb->write_c_string(" ^ ");
      break;
    case OPERATOR_MULT:
      efb->write_c_string(" * ");
      break;
    case OPERATOR_DIV:
      efb->write_c_string(" / ");
      break;
    case OPERATOR_MOD:
      efb->write_c_string(" % ");
      break;
    case OPERATOR_LSHIFT:
      efb->write_c_string(" << ");
      break;
    case OPERATOR_RSHIFT:
      efb->write_c_string(" >> ");
      break;
    case OPERATOR_AND:
      efb->write_c_string(" & ");
      break;
    case OPERATOR_BITCLEAR:
      efb->write_c_string(" &^ ");
      break;
    default:
      go_unreachable();
    }
  this->right_->export_expression(efb);
  efb->write_c_string(")");
}

// Import a binary expression.

Expression*
Binary_expression::do_import(Import_expression* imp, Location loc)
{
  imp->require_c_string("(");

  Expression* left = Expression::import_expression(imp, loc);

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
  else if (imp->match_c_string(")"))
    {
      // Not a binary operator after all.
      imp->advance(1);
      return left;
    }
  else
    {
      go_error_at(imp->location(), "unrecognized binary operator");
      return Expression::make_error(loc);
    }

  Expression* right = Expression::import_expression(imp, loc);

  imp->require_c_string(")");

  return Expression::make_binary(op, left, right, loc);
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
  Gogo* gogo = context->gogo();
  Type* left_type = left->type();
  Type* right_type = right->type();

  Expression* zexpr = Expression::make_integer_ul(0, NULL, location);

  if (left_type->is_string_type() && right_type->is_string_type())
    {
      go_assert(left->is_multi_eval_safe());
      go_assert(right->is_multi_eval_safe());

      if (op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ)
	{
          // (l.len == r.len
          //  ? (l.ptr == r.ptr ? true : memcmp(l.ptr, r.ptr, r.len) == 0)
          //  : false)
          Expression* llen = Expression::make_string_info(left,
                                                          STRING_INFO_LENGTH,
                                                          location);
          Expression* rlen = Expression::make_string_info(right,
                                                          STRING_INFO_LENGTH,
                                                          location);
          Expression* leneq = Expression::make_binary(OPERATOR_EQEQ, llen, rlen,
                                                      location);
          Expression* lptr = Expression::make_string_info(left->copy(),
                                                          STRING_INFO_DATA,
                                                          location);
          Expression* rptr = Expression::make_string_info(right->copy(),
                                                          STRING_INFO_DATA,
                                                          location);
          Expression* ptreq = Expression::make_binary(OPERATOR_EQEQ, lptr, rptr,
                                                      location);
          Expression* btrue = Expression::make_boolean(true, location);
          Expression* call = Runtime::make_call(gogo, Runtime::MEMCMP,
						location, 3,
                                                lptr->copy(), rptr->copy(),
                                                rlen->copy());
          Type* int32_type = Type::lookup_integer_type("int32");
          Expression* zero = Expression::make_integer_ul(0, int32_type, location);
          Expression* cmp = Expression::make_binary(OPERATOR_EQEQ, call, zero,
                                                    location);
          Expression* cond = Expression::make_conditional(ptreq, btrue, cmp,
                                                          location);
          Expression* bfalse = Expression::make_boolean(false, location);
          left = Expression::make_conditional(leneq, cond, bfalse, location);
	  right = Expression::make_boolean(true, location);
	}
      else
	{
	  left = Runtime::make_call(gogo, Runtime::CMPSTRING, location, 2,
				    left, right);
	  right = zexpr;
	}
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
      // address if it is not a direct interface type.
      Expression* pointer_arg = NULL;
      if (right_type->is_direct_iface_type())
        pointer_arg = Expression::unpack_direct_iface(right, location);
      else
	{
          go_assert(right->is_addressable());
          pointer_arg = Expression::make_unary(OPERATOR_AND, right,
                                               location);
	}

      Expression* descriptor =
          Expression::make_type_descriptor(right_type, location);
      left = Runtime::make_call(gogo,
				(left_type->interface_type()->is_empty()
				 ? Runtime::EFACEVALEQ
				 : Runtime::IFACEVALEQ),
				location, 3, left, descriptor,
				pointer_arg);
      go_assert(op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ);
      right = Expression::make_boolean(true, location);
    }
  else if (left_type->interface_type() != NULL
	   && right_type->interface_type() != NULL)
    {
      Runtime::Function compare_function;
      if (left_type->interface_type()->is_empty()
	  && right_type->interface_type()->is_empty())
	compare_function = Runtime::EFACEEQ;
      else if (!left_type->interface_type()->is_empty()
	       && !right_type->interface_type()->is_empty())
	compare_function = Runtime::IFACEEQ;
      else
	{
	  if (left_type->interface_type()->is_empty())
	    {
	      std::swap(left_type, right_type);
	      std::swap(left, right);
	    }
	  go_assert(!left_type->interface_type()->is_empty());
	  go_assert(right_type->interface_type()->is_empty());
	  compare_function = Runtime::IFACEEFACEEQ;
	}

      left = Runtime::make_call(gogo, compare_function, location, 2,
				left, right);
      go_assert(op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ);
      right = Expression::make_boolean(true, location);
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

  left->determine_type_no_context(gogo);
  right->determine_type_no_context(gogo);

  Bexpression* left_bexpr = left->get_backend(context);
  Bexpression* right_bexpr = right->get_backend(context);

  Bexpression* ret = gogo->backend()->binary_expression(op, left_bexpr,
                                                        right_bexpr, location);
  if (result_type != NULL)
    ret = gogo->backend()->convert_expression(result_type->get_backend(gogo),
                                              ret, location);
  return ret;
}

// Class String_concat_expression.

bool
String_concat_expression::do_is_constant() const
{
  for (Expression_list::const_iterator pe = this->exprs_->begin();
       pe != this->exprs_->end();
       ++pe)
    {
      if (!(*pe)->is_constant())
	return false;
    }
  return true;
}

bool
String_concat_expression::do_is_untyped(Type** ptype) const
{
  for (Expression_list::iterator pe = this->exprs_->begin();
       pe != this->exprs_->end();
       ++pe)
    {
      if (!(*pe)->is_untyped(ptype))
	return false;
    }

  *ptype = Type::make_string_type();
  return true;
}

bool
String_concat_expression::do_is_zero_value() const
{
  for (Expression_list::const_iterator pe = this->exprs_->begin();
       pe != this->exprs_->end();
       ++pe)
    {
      if (!(*pe)->is_zero_value())
	return false;
    }
  return true;
}

bool
String_concat_expression::do_is_static_initializer() const
{
  for (Expression_list::const_iterator pe = this->exprs_->begin();
       pe != this->exprs_->end();
       ++pe)
    {
      if (!(*pe)->is_static_initializer())
	return false;
    }
  return true;
}

Type*
String_concat_expression::do_type()
{
  Type* t = this->exprs_->front()->type();
  Expression_list::iterator pe = this->exprs_->begin();
  ++pe;
  for (; pe != this->exprs_->end(); ++pe)
    {
      Type* t1;
      if (!Binary_expression::operation_type(OPERATOR_PLUS, t,
					     (*pe)->type(),
					     &t1))
	return Type::make_error_type();
      t = t1;
    }
  return t;
}

void
String_concat_expression::do_determine_type(Gogo* gogo,
					    const Type_context* context)
{
  Type_context subcontext(*context);
  for (Expression_list::iterator pe = this->exprs_->begin();
       pe != this->exprs_->end();
       ++pe)
    {
      Type* t = (*pe)->type();
      if (!t->is_abstract())
	{
	  subcontext.type = t;
	  break;
	}
    }
  if (subcontext.type == NULL)
    subcontext.type = this->exprs_->front()->type();
  for (Expression_list::iterator pe = this->exprs_->begin();
       pe != this->exprs_->end();
       ++pe)
    (*pe)->determine_type(gogo, &subcontext);
}

void
String_concat_expression::do_check_types(Gogo*)
{
  if (this->is_error_expression())
    return;
  Type* t = this->exprs_->front()->type();
  if (t->is_error())
    {
      this->set_is_error();
      return;
    }
  Expression_list::iterator pe = this->exprs_->begin();
  ++pe;
  for (; pe != this->exprs_->end(); ++pe)
    {
      Type* t1 = (*pe)->type();
      if (!Type::are_compatible_for_binop(t, t1))
	{
	  this->report_error("incompatible types in binary expression");
	  return;
	}
      if (!Binary_expression::check_operator_type(OPERATOR_PLUS, t, t1,
						  this->location()))
	{
	  this->set_is_error();
	  return;
	}
    }
}

Expression*
String_concat_expression::do_flatten(Gogo* gogo, Named_object*,
				     Statement_inserter* inserter)
{
  if (this->is_error_expression())
    return this;
  Location loc = this->location();
  Type* type = this->type();

  // Mark string([]byte) operands to reuse the backing store.
  // runtime.concatstrings does not keep the reference.
  //
  // Note: in the gc runtime, if all but one inputs are empty,
  // concatstrings returns the only nonempty input without copy.
  // So it is not safe to reuse the backing store if it is a
  // string([]byte) conversion. So the gc compiler does the
  // no-copy optimization only when there is at least one
  // constant nonempty input. Currently the gccgo runtime
  // doesn't do this, so we don't do the check.
  for (Expression_list::iterator p = this->exprs_->begin();
       p != this->exprs_->end();
       ++p)
    {
      Type_conversion_expression* tce = (*p)->conversion_expression();
      if (tce != NULL)
        tce->set_no_copy(true);
    }

  Expression* buf = NULL;
  Node* n = Node::make_node(this);
  if ((n->encoding() & ESCAPE_MASK) == Node::ESCAPE_NONE)
    {
      size_t size = 0;
      for (Expression_list::iterator p = this->exprs_->begin();
           p != this->exprs_->end();
           ++p)
        {
          std::string s;
          if ((*p)->string_constant_value(&s))
            size += s.length();
        }
      // Make a buffer on stack if the result does not escape.
      // But don't do this if we know it won't fit.
      if (size < (size_t)tmp_string_buf_size)
        {
          Type* byte_type = Type::lookup_integer_type("uint8");
          Expression* buflen =
            Expression::make_integer_ul(tmp_string_buf_size, NULL, loc);
          Expression::make_integer_ul(tmp_string_buf_size, NULL, loc);
          Type* array_type = Type::make_array_type(byte_type, buflen);
          buf = Expression::make_allocation(array_type, loc);
          buf->allocation_expression()->set_allocate_on_stack();
          buf->allocation_expression()->set_no_zero();
        }
    }
  if (buf == NULL)
    buf = Expression::make_nil(loc);
  go_assert(this->exprs_->size() > 1);
  Expression* len =
    Expression::make_integer_ul(this->exprs_->size(), NULL, loc);
  Array_type* array_type = Type::make_array_type(type, len);
  array_type->set_is_array_incomparable();
  Expression* array =
    Expression::make_array_composite_literal(array_type, this->exprs_,
                                             loc);
  Temporary_statement* ts =
    Statement::make_temporary(array_type, array, loc);
  ts->determine_types(gogo);
  inserter->insert(ts);
  Expression* ref = Expression::make_temporary_reference(ts, loc);
  ref = Expression::make_unary(OPERATOR_AND, ref, loc);
  Expression* call =
    Runtime::make_call(gogo, Runtime::CONCATSTRINGS, loc, 3, buf,
                       ref, len->copy());
  Expression* ret = Expression::make_cast(type, call, loc);
  Type_context context(type, false);
  ret->determine_type(gogo, &context);
  return ret;
}

void
String_concat_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "concat(";
  ast_dump_context->dump_expression_list(this->exprs_, false);
  ast_dump_context->ostream() << ")";
}

Expression*
Expression::make_string_concat(Expression_list* exprs)
{
  return new String_concat_expression(exprs);
}

// Class Bound_method_expression.

// Traversal.

int
Bound_method_expression::do_traverse(Traverse* traverse)
{
  return Expression::traverse(&this->expr_, traverse);
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
Bound_method_expression::do_determine_type(Gogo* gogo, const Type_context*)
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
    this->expr_->determine_type_no_context(gogo);
  else
    {
      Type_context subcontext(fntype->receiver()->type(), false);
      this->expr_->determine_type(gogo, &subcontext);
    }
}

// Check the types of a method expression.

void
Bound_method_expression::do_check_types(Gogo*)
{
  Named_object* fn = this->function();
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
  if (!Type::are_identical(rtype, etype, Type::COMPARE_TAGS, NULL))
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

// Find or create the thunk for FN.

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
      ins.first->second =
	Named_object::make_erroneous_name(gogo->thunk_name());
      return ins.first->second;
    }

  Struct_field_list* sfl = new Struct_field_list();
  // The type here is wrong--it should be the C function type.  But it
  // doesn't really matter.
  Type* vt = Type::make_pointer_type(Type::make_void_type());
  sfl->push_back(Struct_field(Typed_identifier("fn", vt, loc)));
  sfl->push_back(Struct_field(Typed_identifier("val",
					       orig_fntype->receiver()->type(),
					       loc)));
  Struct_type* st = Type::make_struct_type(sfl, loc);
  st->set_is_struct_incomparable();
  Type* closure_type = Type::make_pointer_type(st);

  Function_type* new_fntype = orig_fntype->copy_with_names();

  std::string thunk_name = gogo->thunk_name();
  Named_object* new_no = gogo->start_function(thunk_name, new_fntype,
					      false, loc);

  Variable* cvar = new Variable(closure_type, NULL, false, false, false, loc);
  cvar->set_is_used();
  cvar->set_is_closure();
  Named_object* cp = Named_object::make_variable("$closure" + thunk_name,
						 NULL, cvar);
  new_no->func_value()->set_closure_var(cp);

  gogo->start_block(loc);

  // Field 0 of the closure is the function code pointer, field 1 is
  // the value on which to invoke the method.
  Expression* arg = Expression::make_var_reference(cp, loc);
  arg = Expression::make_dereference(arg, NIL_CHECK_NOT_NEEDED, loc);
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

  Statement* s = Statement::make_return_from_call(new_no, call, loc);
  s->determine_types(gogo);
  gogo->add_statement(s);
  Block* b = gogo->finish_block(loc);
  gogo->add_block(b, loc);

  // This is called after lowering.
  gogo->lower_block(new_no, b);

  gogo->finish_function(loc);

  ins.first->second = new_no;
  return new_no;
}

// Look up a thunk for FN.

Named_object*
Bound_method_expression::lookup_thunk(Named_object* fn)
{
  Method_value_thunks::const_iterator p =
    Bound_method_expression::method_value_thunks.find(fn);
  if (p == Bound_method_expression::method_value_thunks.end())
    return NULL;
  return p->second;
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
      *ref = Expression::make_dereference(*ref, Expression::NIL_CHECK_DEFAULT,
                                          loc);
      go_assert((*ref)->type()->struct_type() == stype);
    }
  *ref = Expression::make_field_reference(*ref, field_indexes->field_index,
					  loc);
  return cond;
}

// Flatten a method value into a struct with nil checks.  We can't do
// this in the lowering phase, because if the method value is called
// directly we don't need a thunk.  That case will have been handled
// by Call_expression::do_lower, so if we get here then we do need a
// thunk.

Expression*
Bound_method_expression::do_flatten(Gogo* gogo, Named_object*,
				    Statement_inserter* inserter)
{
  Location loc = this->location();

  Named_object* thunk = Bound_method_expression::lookup_thunk(this->function_);

  // The thunk should have been created during the
  // create_function_descriptors pass.
  if (thunk == NULL || thunk->is_erroneous())
    {
      go_assert(saw_errors());
      return Expression::make_error(loc);
    }

  // Force the expression into a variable.  This is only necessary if
  // we are going to do nil checks below, but it's easy enough to
  // always do it.
  Expression* expr = this->expr_;
  if (!expr->is_multi_eval_safe())
    {
      Temporary_statement* etemp = Statement::make_temporary(NULL, expr, loc);
      inserter->insert(etemp);
      expr = Expression::make_temporary_reference(etemp, loc);
    }

  // If the method expects a value, and we have a pointer, we need to
  // dereference the pointer.

  Named_object* fn = this->method_->named_object();
  Function_type *fntype;
  if (fn->is_function())
    fntype = fn->func_value()->type();
  else if (fn->is_function_declaration())
    fntype = fn->func_declaration_value()->type();
  else
    go_unreachable();

  Expression* val = expr;
  if (fntype->receiver()->type()->points_to() == NULL
      && val->type()->points_to() != NULL)
    val = Expression::make_dereference(val, NIL_CHECK_DEFAULT, loc);

  // Note that we are ignoring this->expr_type_ here.  The thunk will
  // expect a closure whose second field has type this->expr_type_ (if
  // that is not NULL).  We are going to pass it a closure whose
  // second field has type this->expr_->type().  Since
  // this->expr_type_ is only not-NULL for pointer types, we can get
  // away with this.

  Struct_field_list* fields = new Struct_field_list();
  fields->push_back(Struct_field(Typed_identifier("fn",
						  thunk->func_value()->type(),
						  loc)));
  fields->push_back(Struct_field(Typed_identifier("val", val->type(), loc)));
  Struct_type* st = Type::make_struct_type(fields, loc);
  st->set_is_struct_incomparable();

  Expression_list* vals = new Expression_list();
  vals->push_back(Expression::make_func_code_reference(thunk, loc));
  vals->push_back(val);

  Expression* ret = Expression::make_struct_composite_literal(st, vals, loc);
  ret = Expression::make_heap_expression(ret, loc);

  Node* node = Node::make_node(this);
  if ((node->encoding() & ESCAPE_MASK) == Node::ESCAPE_NONE)
    ret->heap_expression()->set_allocate_on_stack();
  else if (gogo->compiling_runtime()
	   && gogo->package_name() == "runtime"
	   && !saw_errors())
    go_error_at(loc, "%s escapes to heap, not allowed in runtime",
                node->ast_format(gogo).c_str());

  // If necessary, check whether the expression or any embedded
  // pointers are nil.

  Expression* nil_check = NULL;
  if (this->method_->field_indexes() != NULL)
    {
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
      Expression* crash = Runtime::make_call(gogo, Runtime::PANIC_MEM, loc, 0);
      // Fix the type of the conditional expression by pretending to
      // evaluate to RET either way through the conditional.
      crash->determine_type_no_context(gogo);
      crash = Expression::make_compound(crash, ret, loc);
      ret = Expression::make_conditional(nil_check, crash, ret, loc);
    }

  // RET is a pointer to a struct, but we want a function type.
  ret = Expression::make_unsafe_cast(this->type(), ret, loc);

  ret->determine_type_no_context(gogo);

  return ret;
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

// A general selector.  This is a Parser_expression for LEFT.NAME.  It
// is lowered after we know the type of the left hand side.

class Selector_expression : public Parser_expression
{
 public:
  Selector_expression(Expression* left, const std::string& name,
		      Location location)
    : Parser_expression(EXPRESSION_SELECTOR, location),
      left_(left), name_(name), resolved_(NULL)
  { }

  // Return the resolved selector.  This will typically be a
  // Field_reference_expression or a Bound_method_expression or an
  // Interface_field_reference_expression.
  Expression*
  resolved()
  { return this->resolved_; }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->left_, traverse); }

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*);

  bool
  do_is_addressable() const;

  void
  do_issue_nil_check();

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*);

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
  // The resolved expression.
  Expression* resolved_;
};

void
Selector_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  if (this->is_error_expression() || this->resolved_ != NULL)
    return;
  Expression* left = this->left_;
  left->determine_type_no_context(gogo);
  if (left->is_error_expression())
    this->set_is_error();
  else
    {
      if (left->is_type_expression())
	this->resolved_ = this->lower_method_expression(gogo);
      else
	this->resolved_ = Type::bind_field_or_method(gogo, left->type(), left,
						     this->name_,
						     this->location());
      this->resolved_->determine_type(gogo, context);
    }
}

Type*
Selector_expression::do_type()
{
  if (this->is_error_expression())
    return Type::make_error_type();
  go_assert(this->resolved_ != NULL);
  return this->resolved_->type();
}

bool
Selector_expression::do_is_addressable() const
{
  if (this->is_error_expression())
    return true;
  go_assert(this->resolved_ != NULL);
  return this->resolved_->is_addressable();
}

void
Selector_expression::do_issue_nil_check()
{
  if (this->is_error_expression())
    return;
  go_assert(this->resolved_ != NULL);
  this->resolved_->issue_nil_check();
}

// Lower a selector expression to the resolved value.

Expression*
Selector_expression::do_lower(Gogo*, Named_object*, Statement_inserter*)
{
  if (this->is_error_expression() || this->resolved_ == NULL)
    return Expression::make_error(this->location());
  return this->resolved_;
}

// Lower a method expression T.M or (*T).M.  We turn this into a
// function literal.

Expression*
Selector_expression::lower_method_expression(Gogo* gogo)
{
  Location location = this->location();
  Type* left_type = this->left_->type();
  Type* type = left_type;
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
  Struct_type* st = type->struct_type();
  bool is_ambiguous = false;
  Method* method = NULL;
  if (nt != NULL)
    method = nt->method_function(name, &is_ambiguous);
  else if (st != NULL)
    method = st->method_function(name, &is_ambiguous);
  const Typed_identifier* imethod = NULL;
  if (method == NULL && !is_pointer)
    {
      Interface_type* it = type->interface_type();
      if (it != NULL)
	imethod = it->find_method(name);
    }

  if ((method == NULL && imethod == NULL)
      || (left_type->named_type() != NULL && left_type->points_to() != NULL))
    {
      if (nt != NULL)
	{
	  if (!is_ambiguous)
	    go_error_at(location, "type %<%s%s%> has no method %qs",
			is_pointer ? "*" : "",
			nt->message_name().c_str(),
			Gogo::message_name(name).c_str());
	  else
	    go_error_at(location, "method %<%s%s%> is ambiguous in type %qs",
			Gogo::message_name(name).c_str(),
			is_pointer ? "*" : "",
			nt->message_name().c_str());
	}
      else
	{
	  if (!is_ambiguous)
	    go_error_at(location, "type has no method %qs",
			Gogo::message_name(name).c_str());
	  else
	    go_error_at(location, "method %qs is ambiguous",
			Gogo::message_name(name).c_str());
	}
      return Expression::make_error(location);
    }

  if (method != NULL && !is_pointer && !method->is_value_method())
    {
      go_error_at(location, "method requires pointer (use %<(*%s).%s%>)",
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
	  if (!p->name().empty() && !Gogo::is_sink_name(p->name()))
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

  Named_object* no = gogo->start_function(gogo->thunk_name(), fntype, false,
					  location);

  Named_object* vno = gogo->lookup(receiver_name, NULL);
  go_assert(vno != NULL);
  Expression* ve = Expression::make_var_reference(vno, location);
  Expression* bm;
  if (method != NULL)
    bm = Type::bind_field_or_method(gogo, type, ve, name, location);
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

  Statement* s = Statement::make_return_from_call(no, call, location);
  s->determine_types(gogo);
  gogo->add_statement(s);

  Block* b = gogo->finish_block(location);

  gogo->add_block(b, location);

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

// Class Builtin_call_expression.  This is used for a call to a
// builtin function.

Builtin_call_expression::Builtin_call_expression(Gogo* gogo,
						 Expression* fn,
						 Expression_list* args,
						 bool is_varargs,
						 Location location)
  : Call_expression(fn, args, is_varargs, location),
    gogo_(gogo), code_(BUILTIN_INVALID), seen_(false),
    recover_arg_is_set_(false)
{
  const Named_object* no;
  if (fn->is_error_expression())
    {
      this->code_ = BUILTIN_INVALID;
      return;
    }
  else if (fn->func_expression() != NULL)
    no = fn->func_expression()->named_object();
  else if (fn->unknown_expression() != NULL)
    no = fn->unknown_expression()->named_object();
  else
    go_unreachable();

  const std::string& name(no->name());
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
  else if (name == "Add")
    this->code_ = BUILTIN_ADD;
  else if (name == "Alignof")
    this->code_ = BUILTIN_ALIGNOF;
  else if (name == "Offsetof")
    this->code_ = BUILTIN_OFFSETOF;
  else if (name == "Sizeof")
    this->code_ = BUILTIN_SIZEOF;
  else if (name == "Slice")
    this->code_ = BUILTIN_SLICE;
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
  this->recover_arg_is_set_ = true;
}

// Lower a builtin call expression.  This turns new and make into
// specific expressions.  We also convert to a constant if we can.

Expression*
Builtin_call_expression::do_lower(Gogo* gogo, Named_object* function,
				  Statement_inserter* inserter)
{
  if (this->is_error_expression())
    return this;

  Location loc = this->location();

  if (this->code_ == BUILTIN_OFFSETOF)
    {
      Expression* arg = this->one_arg();
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
	{
	  Expression* ret = nc.expression(loc);
	  Type_context subcontext;
	  if (this->type() != NULL)
	    subcontext = Type_context(this->type(),
				      this->type()->is_abstract());
	  ret->determine_type(gogo, &subcontext);
	  return ret;
	}
    }

  switch (this->code_)
    {
    default:
      break;

    case BUILTIN_NEW:
      return Expression::make_allocation(this->one_arg()->type(), loc);

    case BUILTIN_MAKE:
      return this->lower_make(gogo, inserter);

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

    case BUILTIN_DELETE:
      {
        const Expression_list* args = this->args();
	Type* key_type =
	  args->front()->type()->map_type()->key_type();
	Expression_list::iterator pa = this->args()->begin();
	pa++;
	Type* arg_type = (*pa)->type();
	if (!Type::are_identical(key_type, arg_type, 0, NULL))
	  *pa = Expression::make_cast(key_type, *pa, loc);
      }
      break;

    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      // Force all the arguments into temporary variables, so that we
      // don't try to evaluate something while holding the print lock.
      if (this->args() == NULL)
	break;
      for (Expression_list::iterator pa = this->args()->begin();
	   pa != this->args()->end();
	   ++pa)
	{
	  if (!(*pa)->is_multi_eval_safe())
	    {
	      Temporary_statement* temp =
		Statement::make_temporary(NULL, *pa, loc);
	      inserter->insert(temp);
	      *pa = Expression::make_temporary_reference(temp, loc);
	    }
	}
      break;
    }

  return this;
}

// Flatten a builtin call expression.  This turns the arguments of some
// builtin calls into temporary expressions.  Also expand copy and append
// to runtime calls.

Expression*
Builtin_call_expression::do_flatten(Gogo* gogo, Named_object* function,
                                    Statement_inserter* inserter)
{
  if (this->is_error_expression())
    {
      go_assert(saw_errors());
      return this;
    }

  Location loc = this->location();

  switch (this->code_)
    {
    default:
      break;

    case BUILTIN_APPEND:
      return this->flatten_append(gogo, function, inserter, NULL, NULL);

    case BUILTIN_COPY:
      {
	Type* at = this->args()->front()->type();
	for (Expression_list::iterator pa = this->args()->begin();
	     pa != this->args()->end();
	     ++pa)
	  {
	    if ((*pa)->is_error_expression())
	      {
		go_assert(saw_errors());
		return Expression::make_error(loc);
	      }
	    if ((*pa)->is_nil_expression())
	      {
		Expression* nil = Expression::make_nil(loc);
		Expression* zero = Expression::make_integer_ul(0, NULL, loc);
		*pa = Expression::make_slice_value(at, nil, zero, zero, loc);
	      }
	    if (!(*pa)->is_multi_eval_safe())
	      {
		Temporary_statement* temp =
                  Statement::make_temporary(NULL, *pa, loc);
		inserter->insert(temp);
		*pa = Expression::make_temporary_reference(temp, loc);
	      }
	  }

        // Lower to runtime call.
        const Expression_list* args = this->args();
        go_assert(args != NULL && args->size() == 2);
        Expression* arg1 = args->front();
        Expression* arg2 = args->back();
	go_assert(arg1->is_multi_eval_safe());
	go_assert(arg2->is_multi_eval_safe());
        bool arg2_is_string = arg2->type()->is_string_type();

        Expression* ret;
        Type* et = at->array_type()->element_type();
        if (et->has_pointer())
          {
            Expression* td = Expression::make_type_descriptor(et, loc);
	    Expression* pd =
	      Expression::make_slice_info(arg1, SLICE_INFO_VALUE_POINTER, loc);
	    Expression* ld =
	      Expression::make_slice_info(arg1, SLICE_INFO_LENGTH, loc);
	    Expression* ps =
	      Expression::make_slice_info(arg2, SLICE_INFO_VALUE_POINTER, loc);
	    Expression* ls =
	      Expression::make_slice_info(arg2, SLICE_INFO_LENGTH, loc);
            ret = Runtime::make_call(gogo, Runtime::TYPEDSLICECOPY, loc,
                                     5, td, pd, ld, ps, ls);
          }
        else
          {
            Type* int_type = Type::lookup_integer_type("int");
            Type* uintptr_type = Type::lookup_integer_type("uintptr");

            // l1 = len(arg1)
            Named_object* lenfn = gogo->lookup_global("len");
            Expression* lenref = Expression::make_func_reference(lenfn, NULL, loc);
            Expression_list* len_args = new Expression_list();
            len_args->push_back(arg1->copy());
            Expression* len1 = Expression::make_call(lenref, len_args, false, loc);
            gogo->lower_expression(function, inserter, &len1);
            gogo->flatten_expression(function, inserter, &len1);
            Temporary_statement* l1tmp = Statement::make_temporary(int_type, len1, loc);
	    l1tmp->determine_types(gogo);
            inserter->insert(l1tmp);

            // l2 = len(arg2)
            len_args = new Expression_list();
            len_args->push_back(arg2->copy());
            Expression* len2 = Expression::make_call(lenref, len_args, false, loc);
            gogo->lower_expression(function, inserter, &len2);
            gogo->flatten_expression(function, inserter, &len2);
            Temporary_statement* l2tmp = Statement::make_temporary(int_type, len2, loc);
	    l2tmp->determine_types(gogo);
            inserter->insert(l2tmp);

            // n = (l1 < l2 ? l1 : l2)
            Expression* l1ref = Expression::make_temporary_reference(l1tmp, loc);
            Expression* l2ref = Expression::make_temporary_reference(l2tmp, loc);
            Expression* cond = Expression::make_binary(OPERATOR_LT, l1ref, l2ref, loc);
            Expression* n = Expression::make_conditional(cond,
                                                         l1ref->copy(),
                                                         l2ref->copy(),
                                                         loc);
            Temporary_statement* ntmp = Statement::make_temporary(NULL, n, loc);
	    ntmp->determine_types(gogo);
            inserter->insert(ntmp);

            // sz = n * sizeof(elem_type)
            Expression* nref = Expression::make_temporary_reference(ntmp, loc);
            nref = Expression::make_cast(uintptr_type, nref, loc);
            Expression* sz = Expression::make_type_info(et, TYPE_INFO_SIZE);
            sz = Expression::make_binary(OPERATOR_MULT, sz, nref, loc);

            // memmove(arg1.ptr, arg2.ptr, sz)
            Expression* p1 = Expression::make_slice_info(arg1,
                                                         SLICE_INFO_VALUE_POINTER,
                                                         loc);
            Expression* p2 = (arg2_is_string
                              ? Expression::make_string_info(arg2,
                                                             STRING_INFO_DATA,
                                                             loc)
                              : Expression::make_slice_info(arg2,
                                                            SLICE_INFO_VALUE_POINTER,
                                                            loc));
            Expression* call = Runtime::make_call(gogo,
						  Runtime::BUILTIN_MEMMOVE,
						  loc, 3,
                                                  p1, p2, sz);

            // n is the return value of copy
            nref = Expression::make_temporary_reference(ntmp, loc);
            ret = Expression::make_compound(call, nref, loc);
          }
	ret->determine_type_no_context(gogo);
        return ret;
      }
      break;

    case BUILTIN_PANIC:
      for (Expression_list::iterator pa = this->args()->begin();
	   pa != this->args()->end();
	   ++pa)
	{
	  if (!(*pa)->is_multi_eval_safe()
	      && (*pa)->type()->interface_type() != NULL)
	    {
	      Temporary_statement* temp =
		Statement::make_temporary(NULL, *pa, loc);
	      inserter->insert(temp);
	      *pa = Expression::make_temporary_reference(temp, loc);
	    }
	}
      break;

    case BUILTIN_LEN:
    case BUILTIN_CAP:
      {
	Expression_list::iterator pa = this->args()->begin();
	if (!(*pa)->is_multi_eval_safe()
	    && ((*pa)->type()->map_type() != NULL
		|| (*pa)->type()->channel_type() != NULL))
	  {
	    Temporary_statement* temp =
	      Statement::make_temporary(NULL, *pa, loc);
	    inserter->insert(temp);
	    *pa = Expression::make_temporary_reference(temp, loc);
	  }
      }
      break;

    case BUILTIN_DELETE:
      {
        // Lower to a runtime function call.
        const Expression_list* args = this->args();

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

        Expression* e1 = Expression::make_type_descriptor(mt, loc);
        Expression* e2 = Expression::make_temporary_reference(map_temp,
                                                              loc);
        Expression* e3 = Expression::make_temporary_reference(key_temp,
                                                              loc);

        Runtime::Function code;
        switch (mt->algorithm(gogo))
          {
            case Map_type::MAP_ALG_FAST32:
            case Map_type::MAP_ALG_FAST32PTR:
              {
                code = Runtime::MAPDELETE_FAST32;
                Type* uint32_type = Type::lookup_integer_type("uint32");
                Type* uint32_ptr_type = Type::make_pointer_type(uint32_type);
                e3 = Expression::make_unary(OPERATOR_AND, e3, loc);
                e3 = Expression::make_unsafe_cast(uint32_ptr_type, e3,
                                                  loc);
                e3 = Expression::make_dereference(e3,
                                                  Expression::NIL_CHECK_NOT_NEEDED,
                                                  loc);
                break;
              }
            case Map_type::MAP_ALG_FAST64:
            case Map_type::MAP_ALG_FAST64PTR:
              {
                code = Runtime::MAPDELETE_FAST64;
                Type* uint64_type = Type::lookup_integer_type("uint64");
                Type* uint64_ptr_type = Type::make_pointer_type(uint64_type);
                e3 = Expression::make_unary(OPERATOR_AND, e3, loc);
                e3 = Expression::make_unsafe_cast(uint64_ptr_type, e3,
                                                  loc);
                e3 = Expression::make_dereference(e3,
                                                  Expression::NIL_CHECK_NOT_NEEDED,
                                                  loc);
                break;
              }
            case Map_type::MAP_ALG_FASTSTR:
              code = Runtime::MAPDELETE_FASTSTR;
              break;
            default:
              code = Runtime::MAPDELETE;

              // If the call to delete is deferred, and is in a loop,
              // then the loop will only have a single instance of the
              // temporary variable.  Passing the address of the
              // temporary variable here means that the deferred call
              // will see the last value in the loop, not the current
              // value.  So for this unusual case copy the value into
              // the heap.
              if (!this->is_deferred())
                e3 = Expression::make_unary(OPERATOR_AND, e3, loc);
              else
                {
                  Expression* a = Expression::make_allocation(mt->key_type(),
                                                              loc);
                  Temporary_statement* atemp =
                    Statement::make_temporary(NULL, a, loc);
		  atemp->determine_types(gogo);
                  inserter->insert(atemp);

                  a = Expression::make_temporary_reference(atemp, loc);
                  a = Expression::make_dereference(a, NIL_CHECK_NOT_NEEDED, loc);
                  Statement* s = Statement::make_assignment(a, e3, loc);
		  s->determine_types(gogo);
                  inserter->insert(s);

                  e3 = Expression::make_temporary_reference(atemp, loc);
                }
          }

        Expression* ret = Runtime::make_call(gogo, code, loc, 3, e1, e2, e3);
	ret->determine_type_no_context(gogo);
	return ret;
      }

    case BUILTIN_ADD:
      {
	Expression* ptr = this->args()->front();
	Type* uintptr_type = Type::lookup_integer_type("uintptr");
	ptr = Expression::make_cast(uintptr_type, ptr, loc);
	Expression* len = this->args()->back();
	len = Expression::make_cast(uintptr_type, len, loc);
	Expression* add = Expression::make_binary(OPERATOR_PLUS, ptr, len,
						  loc);
	Expression* ret = Expression::make_cast(this->args()->front()->type(),
						add, loc);
	ret->determine_type_no_context(gogo);
	return ret;
      }

    case BUILTIN_SLICE:
      {
	Expression* ptr = this->args()->front();
	Temporary_statement* ptr_temp = NULL;
	if (!ptr->is_multi_eval_safe())
	  {
	    ptr_temp = Statement::make_temporary(NULL, ptr, loc);
	    inserter->insert(ptr_temp);
	    ptr = Expression::make_temporary_reference(ptr_temp, loc);
	  }

	Expression* len = this->args()->back();
	Temporary_statement* len_temp = NULL;
	if (!len->is_multi_eval_safe())
	  {
	    len_temp = Statement::make_temporary(NULL, len, loc);
	    inserter->insert(len_temp);
	    len = Expression::make_temporary_reference(len_temp, loc);
	  }

	bool fits_in_int;
	Numeric_constant nc;
	if (this->args()->back()->numeric_constant_value(&nc))
	  {
	    // We gave an error for constants that don't fit in int in
	    // check_types.
	    fits_in_int = true;
	  }
	else
	  {
	    Integer_type* itype = this->args()->back()->type()->integer_type();
	    go_assert(itype != NULL);
	    int ebits = itype->bits();
	    int intbits =
	      Type::lookup_integer_type("int")->integer_type()->bits();

	    // We can treat ebits == intbits as small even for an
	    // unsigned integer type, because we will convert the
	    // value to int and then reject it in the runtime if it is
	    // negative.

	    fits_in_int = ebits <= intbits;
	  }

	Runtime::Function code = (fits_in_int
				  ? Runtime::UNSAFESLICE
				  : Runtime::UNSAFESLICE64);
	Expression* td =
	  Expression::make_type_descriptor(ptr->type()->points_to(), loc);
	Expression* check = Runtime::make_call(gogo, code, loc, 3,
					       td, ptr, len);

	if (ptr_temp == NULL)
	  ptr = ptr->copy();
	else
	  ptr = Expression::make_temporary_reference(ptr_temp, loc);
	Expression* nil = Expression::make_nil(loc);
	nil = Expression::make_cast(ptr->type(), nil, loc);
	Expression* is_nil = Expression::make_binary(OPERATOR_EQEQ, ptr, nil,
						     loc);

	if (len_temp == NULL)
	  len = len->copy();
	else
	  len = Expression::make_temporary_reference(len_temp, loc);
	Expression* zero = Expression::make_integer_ul(0, len->type(), loc);
	Expression* is_zero = Expression::make_binary(OPERATOR_EQEQ, len, zero,
						      loc);

	Expression* cond = Expression::make_binary(OPERATOR_ANDAND, is_nil,
						   is_zero, loc);

	Type* slice_type = Type::make_array_type(ptr->type()->points_to(),
						 NULL);
	nil = Expression::make_nil(loc);
	Expression* nil_slice = Expression::make_cast(slice_type, nil, loc);

	if (ptr_temp == NULL)
	  ptr = ptr->copy();
	else
	  ptr = Expression::make_temporary_reference(ptr_temp, loc);

	if (len_temp == NULL)
	  len = len->copy();
	else
	  len = Expression::make_temporary_reference(len_temp, loc);

	Expression* cap;
	if (len_temp == NULL)
	  cap = len->copy();
	else
	  cap = Expression::make_temporary_reference(len_temp, loc);

	Expression* slice = Expression::make_slice_value(slice_type, ptr,
							 len, cap, loc);

	slice = Expression::make_conditional(cond, nil_slice, slice, loc);

	Expression* ret = Expression::make_compound(check, slice, loc);
	ret->determine_type_no_context(gogo);
	return ret;
      }
    }

  return this;
}

// Lower a make expression.

Expression*
Builtin_call_expression::lower_make(Gogo* gogo, Statement_inserter* inserter)
{
  Location loc = this->location();

  const Expression_list* args = this->args();

  Expression_list::const_iterator parg = args->begin();

  Expression* first_arg = *parg;
  go_assert(first_arg->is_type_expression());
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
    go_unreachable();

  ++parg;
  Expression* len_arg;
  bool len_small = false;
  if (parg == args->end())
    {
      go_assert(!is_slice);
      len_arg = Expression::make_integer_ul(0, NULL, loc);
      len_small = true;
    }
  else
    {
      len_arg = *parg;
      if (!this->check_int_value(len_arg, true, &len_small))
	return Expression::make_error(this->location());
      ++parg;
    }

  Expression* cap_arg = NULL;
  bool cap_small = false;
  Numeric_constant nclen;
  Numeric_constant nccap;
  unsigned long vlen;
  unsigned long vcap;
  if (is_slice && parg != args->end())
    {
      cap_arg = *parg;
      if (!this->check_int_value(cap_arg, false, &cap_small))
	return Expression::make_error(this->location());

      if (len_arg->numeric_constant_value(&nclen)
	  && cap_arg->numeric_constant_value(&nccap)
	  && nclen.to_unsigned_long(&vlen) == Numeric_constant::NC_UL_VALID
	  && nccap.to_unsigned_long(&vcap) == Numeric_constant::NC_UL_VALID
	  && vlen > vcap)
	{
	  this->report_error(_("len larger than cap"));
	  return Expression::make_error(this->location());
	}

      ++parg;
    }

  go_assert(parg == args->end());

  Location type_loc = first_arg->location();

  Expression* call;
  if (is_slice)
    {
      Temporary_statement* len_temp = NULL;
      if (!len_arg->is_constant())
	{
	  len_temp = Statement::make_temporary(NULL, len_arg, loc);
	  inserter->insert(len_temp);
	  len_arg = Expression::make_temporary_reference(len_temp, loc);
	}

      if (cap_arg == NULL)
	{
          cap_small = len_small;
	  if (len_temp == NULL)
	    cap_arg = len_arg->copy();
	  else
	    cap_arg = Expression::make_temporary_reference(len_temp, loc);
	}
      else if (!cap_arg->is_constant())
	{
	  Temporary_statement* cap_temp = Statement::make_temporary(NULL,
								    cap_arg,
								    loc);
	  inserter->insert(cap_temp);
	  cap_arg = Expression::make_temporary_reference(cap_temp, loc);
	}

      Type* et = type->array_type()->element_type();
      Expression* type_arg = Expression::make_type_descriptor(et, type_loc);
      Runtime::Function code = Runtime::MAKESLICE;
      if (!len_small || !cap_small)
	code = Runtime::MAKESLICE64;
      Expression* mem = Runtime::make_call(gogo, code, loc, 3,
					   type_arg, len_arg, cap_arg);
      mem = Expression::make_unsafe_cast(Type::make_pointer_type(et), mem,
					 loc);
      Type* int_type = Type::lookup_integer_type("int");
      len_arg = Expression::make_cast(int_type, len_arg->copy(), loc);
      cap_arg = Expression::make_cast(int_type, cap_arg->copy(), loc);
      call = Expression::make_slice_value(type, mem, len_arg, cap_arg, loc);
    }
  else if (is_map)
    {
      Expression* type_arg = Expression::make_type_descriptor(type, type_loc);
      if (!len_small)
	call = Runtime::make_call(gogo, Runtime::MAKEMAP64, loc, 3, type_arg,
				  len_arg,
				  Expression::make_nil(loc));
      else
	{
	  if (len_arg->numeric_constant_value(&nclen)
	      && nclen.to_unsigned_long(&vlen) == Numeric_constant::NC_UL_VALID
	      && vlen <= Map_type::bucket_size)
	    call = Runtime::make_call(gogo, Runtime::MAKEMAP_SMALL, loc, 0);
	  else
	    call = Runtime::make_call(gogo, Runtime::MAKEMAP, loc, 3, type_arg,
				      len_arg,
				      Expression::make_nil(loc));
	}
    }
  else if (is_chan)
    {
      Expression* type_arg = Expression::make_type_descriptor(type, type_loc);
      Runtime::Function code = Runtime::MAKECHAN;
      if (!len_small)
	code = Runtime::MAKECHAN64;
      call = Runtime::make_call(gogo, code, loc, 2, type_arg, len_arg);
    }
  else
    go_unreachable();

  Expression* ret = Expression::make_unsafe_cast(type, call, loc);
  ret->determine_type_no_context(gogo);
  return ret;
}

// Flatten a call to the predeclared append function.  We do this in
// the flatten phase, not the lowering phase, so that we run after
// type checking and after order_evaluations.  If ASSIGN_LHS is not
// NULL, this append is the right-hand-side of an assignment and
// ASSIGN_LHS is the left-hand-side; in that case, set LHS directly
// rather than returning a slice.  This lets us omit a write barrier
// in common cases like a = append(a, ...) when the slice does not
// need to grow.  ENCLOSING is not NULL iff ASSIGN_LHS is not NULL.

Expression*
Builtin_call_expression::flatten_append(Gogo* gogo, Named_object* function,
					Statement_inserter* inserter,
					Expression* assign_lhs,
					Block* enclosing)
{
  if (this->is_error_expression())
    return this;

  Location loc = this->location();

  const Expression_list* args = this->args();
  go_assert(args != NULL && !args->empty());

  Type* slice_type = args->front()->type();
  go_assert(slice_type->is_slice_type());
  Type* element_type = slice_type->array_type()->element_type();

  if (args->size() == 1)
    {
      // append(s) evaluates to s.
      if (assign_lhs != NULL)
	return NULL;
      return args->front();
    }

  Type* int_type = Type::lookup_integer_type("int");
  Type_context int_context(int_type, false);
  Type* uint_type = Type::lookup_integer_type("uint");

  // Implementing
  //   append(s1, s2...)
  // or
  //   append(s1, a1, a2, a3, ...)

  // s1tmp := s1
  Temporary_statement* s1tmp = Statement::make_temporary(NULL, args->front(),
							 loc);
  inserter->insert(s1tmp);

  // l1tmp := len(s1tmp)
  Named_object* lenfn = gogo->lookup_global("len");
  Expression* lenref = Expression::make_func_reference(lenfn, NULL, loc);
  Expression_list* call_args = new Expression_list();
  call_args->push_back(Expression::make_temporary_reference(s1tmp, loc));
  Expression* len = Expression::make_call(lenref, call_args, false, loc);
  len->determine_type(gogo, &int_context);
  gogo->lower_expression(function, inserter, &len);
  gogo->flatten_expression(function, inserter, &len);
  Temporary_statement* l1tmp = Statement::make_temporary(int_type, len, loc);
  inserter->insert(l1tmp);

  Temporary_statement* s2tmp = NULL;
  Temporary_statement* l2tmp = NULL;
  Expression_list* add = NULL;
  Expression* len2;
  Call_expression* makecall = NULL;
  if (this->is_varargs())
    {
      go_assert(args->size() == 2);

      std::pair<Call_expression*, Temporary_statement*> p =
        Expression::find_makeslice_call(args->back());
      makecall = p.first;
      if (makecall != NULL)
        {
          // We are handling
          // 	append(s, make([]T, len[, cap])...))
          // which has already been lowered to
          // 	append(s, runtime.makeslice(T, len, cap)).
          // We will optimize this to directly zeroing the tail,
          // instead of allocating a new slice then copy.

          // Retrieve the length and capacity. Cannot reference s2 as
          // we will remove the makeslice call.
          Expression* len_arg = makecall->args()->at(1);
          len_arg = Expression::make_cast(int_type, len_arg, loc);
          l2tmp = Statement::make_temporary(int_type, len_arg, loc);
	  l2tmp->determine_types(gogo);
          inserter->insert(l2tmp);

          Expression* cap_arg = makecall->args()->at(2);
          cap_arg = Expression::make_cast(int_type, cap_arg, loc);
          Temporary_statement* c2tmp =
            Statement::make_temporary(int_type, cap_arg, loc);
	  c2tmp->determine_types(gogo);
          inserter->insert(c2tmp);

          // Check bad len/cap here.
	  // checkmakeslice(type, len, cap)
	  // (Note that if len and cap are constants, we won't see a
	  // makeslice call here, as it will be rewritten to a stack
	  // allocated array by Mark_address_taken::expression.)
	  Expression* elem = Expression::make_type_descriptor(element_type,
							      loc);
          len2 = Expression::make_temporary_reference(l2tmp, loc);
          Expression* cap2 = Expression::make_temporary_reference(c2tmp, loc);
	  Expression* check = Runtime::make_call(gogo,
						 Runtime::CHECK_MAKE_SLICE,
						 loc, 3, elem, len2, cap2);
	  check->determine_type_no_context(gogo);
          gogo->lower_expression(function, inserter, &check);
          gogo->flatten_expression(function, inserter, &check);
          Statement* s = Statement::make_statement(check, false);
          inserter->insert(s);

          // Remove the original makeslice call.
          Temporary_statement* ts = p.second;
          if (ts != NULL && ts->uses() == 1)
            ts->set_init(Expression::make_nil(loc));
        }
      else
        {
          // s2tmp := s2
          s2tmp = Statement::make_temporary(NULL, args->back(), loc);
          inserter->insert(s2tmp);

          // l2tmp := len(s2tmp)
          lenref = Expression::make_func_reference(lenfn, NULL, loc);
          call_args = new Expression_list();
          call_args->push_back(Expression::make_temporary_reference(s2tmp, loc));
          len = Expression::make_call(lenref, call_args, false, loc);
	  len->determine_type(gogo, &int_context);
          gogo->lower_expression(function, inserter, &len);
          gogo->flatten_expression(function, inserter, &len);
          l2tmp = Statement::make_temporary(int_type, len, loc);
	  l2tmp->determine_types(gogo);
          inserter->insert(l2tmp);
        }

      // len2 = l2tmp
      len2 = Expression::make_temporary_reference(l2tmp, loc);
    }
  else
    {
      // We have to ensure that all the arguments are in variables
      // now, because otherwise if one of them is an index expression
      // into the current slice we could overwrite it before we fetch
      // it.
      add = new Expression_list();
      Expression_list::const_iterator pa = args->begin();
      for (++pa; pa != args->end(); ++pa)
	{
	  if ((*pa)->is_multi_eval_safe())
	    add->push_back(*pa);
	  else
	    {
	      Temporary_statement* tmp = Statement::make_temporary(NULL, *pa,
								   loc);
	      inserter->insert(tmp);
	      add->push_back(Expression::make_temporary_reference(tmp, loc));
	    }
	}

      // len2 = len(add)
      len2 = Expression::make_integer_ul(add->size(), int_type, loc);
    }

  // ntmp := l1tmp + len2
  Expression* ref = Expression::make_temporary_reference(l1tmp, loc);
  Expression* sum = Expression::make_binary(OPERATOR_PLUS, ref, len2, loc);
  sum->determine_type(gogo, &int_context);
  gogo->lower_expression(function, inserter, &sum);
  gogo->flatten_expression(function, inserter, &sum);
  Temporary_statement* ntmp = Statement::make_temporary(int_type, sum, loc);
  ntmp->determine_types(gogo);
  inserter->insert(ntmp);

  // s1tmp = uint(ntmp) > uint(cap(s1tmp)) ?
  //   growslice(type, s1tmp, ntmp) :
  //   s1tmp[:ntmp]
  // Using uint here means that if the computation of ntmp overflowed,
  // we will call growslice which will panic.

  Named_object* capfn = gogo->lookup_global("cap");
  Expression* capref = Expression::make_func_reference(capfn, NULL, loc);
  call_args = new Expression_list();
  call_args->push_back(Expression::make_temporary_reference(s1tmp, loc));
  Expression* cap = Expression::make_call(capref, call_args, false, loc);
  cap->determine_type(gogo, &int_context);
  gogo->lower_expression(function, inserter, &cap);
  gogo->flatten_expression(function, inserter, &cap);
  Temporary_statement* c1tmp = Statement::make_temporary(int_type, cap, loc);
  c1tmp->determine_types(gogo);
  inserter->insert(c1tmp);

  Expression* left = Expression::make_temporary_reference(ntmp, loc);
  left = Expression::make_cast(uint_type, left, loc);
  Expression* right = Expression::make_temporary_reference(c1tmp, loc);
  right = Expression::make_cast(uint_type, right, loc);

  Expression* cond = Expression::make_binary(OPERATOR_GT, left, right, loc);

  Type* unsafe_ptr_type = Type::make_pointer_type(Type::make_void_type());
  Expression* a1 = Expression::make_type_descriptor(element_type, loc);
  Expression* a2 = Expression::make_temporary_reference(s1tmp, loc);
  a2 = slice_type->array_type()->get_value_pointer(gogo, a2);
  a2 = Expression::make_cast(unsafe_ptr_type, a2, loc);
  Expression* a3 = Expression::make_temporary_reference(l1tmp, loc);
  Expression* a4 = Expression::make_temporary_reference(c1tmp, loc);
  Expression* a5 = Expression::make_temporary_reference(ntmp, loc);
  Expression* call = Runtime::make_call(gogo, Runtime::GROWSLICE, loc, 5,
					a1, a2, a3, a4, a5);
  call = Expression::make_unsafe_cast(slice_type, call, loc);

  ref = Expression::make_temporary_reference(s1tmp, loc);
  Expression* zero = Expression::make_integer_ul(0, int_type, loc);
  Expression* ref2 = Expression::make_temporary_reference(ntmp, loc);
  ref = Expression::make_array_index(ref, zero, ref2, NULL, loc);
  ref->array_index_expression()->set_needs_bounds_check(false);

  if (assign_lhs == NULL)
    {
      Expression* rhs = Expression::make_conditional(cond, call, ref, loc);

      rhs->determine_type_no_context(gogo);
      gogo->lower_expression(function, inserter, &rhs);
      gogo->flatten_expression(function, inserter, &rhs);

      ref = Expression::make_temporary_reference(s1tmp, loc);
      Statement* assign = Statement::make_assignment(ref, rhs, loc);
      assign->determine_types(gogo);
      inserter->insert(assign);
    }
  else
    {
      cond->determine_type_no_context(gogo);
      gogo->lower_expression(function, inserter, &cond);
      gogo->flatten_expression(function, inserter, &cond);
      call->determine_type_no_context(gogo);
      gogo->lower_expression(function, inserter, &call);
      gogo->flatten_expression(function, inserter, &call);
      ref->determine_type_no_context(gogo);
      gogo->lower_expression(function, inserter, &ref);
      gogo->flatten_expression(function, inserter, &ref);

      Block* then_block = new Block(enclosing, loc);
      Assignment_statement* assign =
	Statement::make_assignment(assign_lhs, call, loc);
      assign->determine_types(gogo);
      then_block->add_statement(assign);

      Block* else_block = new Block(enclosing, loc);
      assign = Statement::make_assignment(assign_lhs->copy(), ref, loc);
      // This assignment will not change the pointer value, so it does
      // not need a write barrier.
      assign->set_omit_write_barrier();
      assign->determine_types(gogo);
      else_block->add_statement(assign);

      Statement* s = Statement::make_if_statement(cond, then_block,
						  else_block, loc);
      s->determine_types(gogo);
      inserter->insert(s);

      ref = Expression::make_temporary_reference(s1tmp, loc);
      assign = Statement::make_assignment(ref, assign_lhs->copy(), loc);
      assign->determine_types(gogo);
      inserter->insert(assign);
    }

  Type* uintptr_type = Type::lookup_integer_type("uintptr");

  if (this->is_varargs())
    {
      if (makecall != NULL)
        {
          // memclr(&s1tmp[l1tmp], l2tmp*sizeof(elem))
          a1 = Expression::make_temporary_reference(s1tmp, loc);
          ref = Expression::make_temporary_reference(l1tmp, loc);
          a1 = Expression::make_array_index(a1, ref, NULL, NULL, loc);
          a1->array_index_expression()->set_needs_bounds_check(false);
          a1 = Expression::make_unary(OPERATOR_AND, a1, loc);

          ref = Expression::make_temporary_reference(l2tmp, loc);
          ref = Expression::make_cast(uintptr_type, ref, loc);
          a2 = Expression::make_type_info(element_type, TYPE_INFO_SIZE);
          a2 = Expression::make_binary(OPERATOR_MULT, a2, ref, loc);

          if (element_type->has_pointer())
            call = Runtime::make_call(gogo, Runtime::MEMCLRHASPTR, loc, 2,
				      a1, a2);
          else
            {
              Type* int32_type = Type::lookup_integer_type("int32");
              zero = Expression::make_integer_ul(0, int32_type, loc);
              call = Runtime::make_call(gogo, Runtime::BUILTIN_MEMSET, loc, 3,
					a1, zero, a2);
            }

          if (element_type->has_pointer())
            {
              // For a slice containing pointers, growslice already zeroed
              // the memory. We only need to zero in non-growing case.
              // Note: growslice does not zero the memory in non-pointer case.
              ref = Expression::make_temporary_reference(ntmp, loc);
              ref = Expression::make_cast(uint_type, ref, loc);
              ref2 = Expression::make_temporary_reference(c1tmp, loc);
              ref2 = Expression::make_cast(uint_type, ref2, loc);
              cond = Expression::make_binary(OPERATOR_GT, ref, ref2, loc);
              zero = Expression::make_integer_ul(0, int_type, loc);
              call = Expression::make_conditional(cond, zero, call, loc);
            }
        }
      else
        {
          if (element_type->has_pointer())
            {
              // copy(s1tmp[l1tmp:], s2tmp)
              a1 = Expression::make_temporary_reference(s1tmp, loc);
              ref = Expression::make_temporary_reference(l1tmp, loc);
              Expression* nil = Expression::make_nil(loc);
              a1 = Expression::make_array_index(a1, ref, nil, NULL, loc);
              a1->array_index_expression()->set_needs_bounds_check(false);

              a2 = Expression::make_temporary_reference(s2tmp, loc);

              Named_object* copyfn = gogo->lookup_global("copy");
              Expression* copyref = Expression::make_func_reference(copyfn, NULL, loc);
              call_args = new Expression_list();
              call_args->push_back(a1);
              call_args->push_back(a2);
              call = Expression::make_call(copyref, call_args, false, loc);
            }
          else
            {
              // memmove(&s1tmp[l1tmp], s2tmp.ptr, l2tmp*sizeof(elem))
              a1 = Expression::make_temporary_reference(s1tmp, loc);
              ref = Expression::make_temporary_reference(l1tmp, loc);
              a1 = Expression::make_array_index(a1, ref, NULL, NULL, loc);
              a1->array_index_expression()->set_needs_bounds_check(false);
              a1 = Expression::make_unary(OPERATOR_AND, a1, loc);

              a2 = Expression::make_temporary_reference(s2tmp, loc);
              a2 = (a2->type()->is_string_type()
                    ? Expression::make_string_info(a2,
                                                   STRING_INFO_DATA,
                                                   loc)
                    : Expression::make_slice_info(a2,
                                                  SLICE_INFO_VALUE_POINTER,
                                                  loc));

              ref = Expression::make_temporary_reference(l2tmp, loc);
              ref = Expression::make_cast(uintptr_type, ref, loc);
              a3 = Expression::make_type_info(element_type, TYPE_INFO_SIZE);
              a3 = Expression::make_binary(OPERATOR_MULT, a3, ref, loc);

              call = Runtime::make_call(gogo, Runtime::BUILTIN_MEMMOVE, loc, 3,
                                        a1, a2, a3);
            }
        }
      call->determine_type_no_context(gogo);
      gogo->lower_expression(function, inserter, &call);
      gogo->flatten_expression(function, inserter, &call);
      inserter->insert(Statement::make_statement(call, false));
    }
  else
    {
      // For each argument:
      //  s1tmp[l1tmp+i] = a
      unsigned long i = 0;
      for (Expression_list::const_iterator pa = add->begin();
	   pa != add->end();
	   ++pa, ++i)
	{
	  ref = Expression::make_temporary_reference(s1tmp, loc);
	  ref2 = Expression::make_temporary_reference(l1tmp, loc);
	  Expression* off = Expression::make_integer_ul(i, int_type, loc);
	  ref2 = Expression::make_binary(OPERATOR_PLUS, ref2, off, loc);
	  Expression* lhs = Expression::make_array_index(ref, ref2, NULL,
                                                         NULL, loc);
          lhs->array_index_expression()->set_needs_bounds_check(false);
	  lhs->determine_type_no_context(gogo);
	  gogo->lower_expression(function, inserter, &lhs);
	  gogo->flatten_expression(function, inserter, &lhs);
      Expression* elem = *pa;
      if (!Type::are_identical(element_type, elem->type(), 0, NULL)
          && element_type->interface_type() != NULL)
        elem = Expression::make_cast(element_type, elem, loc);
	  // The flatten pass runs after the write barrier pass, so we
	  // need to insert a write barrier here if necessary.
	  // However, if ASSIGN_LHS is not NULL, we have been called
	  // directly before the write barrier pass.
	  Statement* assign;
	  if (assign_lhs != NULL
	      || !gogo->assign_needs_write_barrier(lhs, NULL))
	    assign = Statement::make_assignment(lhs, elem, loc);
	  else
	    {
	      Function* f = function == NULL ? NULL : function->func_value();
	      assign = gogo->assign_with_write_barrier(f, NULL, inserter,
						       lhs, elem, loc);
	    }
	  assign->determine_types(gogo);
	  inserter->insert(assign);
	}
    }

  if (assign_lhs != NULL)
    return NULL;

  return Expression::make_temporary_reference(s1tmp, loc);
}

// Return whether an expression has an integer value.  Report an error
// if not.  This is used when handling calls to the predeclared make
// function.  Set *SMALL if the value is known to fit in type "int".

bool
Builtin_call_expression::check_int_value(Expression* e, bool is_length,
					 bool *small)
{
  *small = false;

  Numeric_constant nc;
  if (e->numeric_constant_value(&nc))
    {
      unsigned long v;
      switch (nc.to_unsigned_long(&v))
	{
	case Numeric_constant::NC_UL_VALID:
	  break;
	case Numeric_constant::NC_UL_NOTINT:
	  go_error_at(e->location(), "non-integer %s argument to make",
		      is_length ? "len" : "cap");
	  return false;
	case Numeric_constant::NC_UL_NEGATIVE:
	  go_error_at(e->location(), "negative %s argument to make",
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
	  go_error_at(e->location(), "%s argument too large for make",
		      is_length ? "len" : "cap");
	  return false;
	}

      *small = true;
      return true;
    }

  if (e->type()->integer_type() != NULL)
    {
      int ebits = e->type()->integer_type()->bits();
      int intbits = Type::lookup_integer_type("int")->integer_type()->bits();

      // We can treat ebits == intbits as small even for an unsigned
      // integer type, because we will convert the value to int and
      // then reject it in the runtime if it is negative.
      *small = ebits <= intbits;

      return true;
    }

  go_error_at(e->location(), "non-integer %s argument to make",
	      is_length ? "len" : "cap");
  return false;
}

// Return the type of the real or imag functions, given the type of
// the argument.  We need to map complex64 to float32 and complex128
// to float64, so it has to be done by name.  This returns NULL if it
// can't figure out the type.

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
  Expression* expr = *pexpr;
  if (!expr->is_constant()
      && (expr->call_expression() != NULL
	  || expr->receive_expression() != NULL))
    {
      this->found_ = true;
      return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Return whether calling len or cap on EXPR, of array type, is a
// constant.  The language spec says "the expressions len(s) and
// cap(s) are constants if the type of s is an array or pointer to an
// array and the expression s does not contain channel receives or
// (non-constant) function calls."

bool
Builtin_call_expression::array_len_is_constant(Expression* expr)
{
  go_assert(expr->type()->deref()->array_type() != NULL
	    && !expr->type()->deref()->is_slice_type());
  if (expr->is_constant())
    return true;
  Find_call_expression find_call;
  Expression::traverse(&expr, &find_call);
  return !find_call.found();
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

	// We may be called before the determine_types pass.
	arg->determine_type_no_context(this->gogo_);

	Type* arg_type = arg->type();
	if (arg_type->is_error())
	  return true;

	if (arg_type->points_to() != NULL
	    && arg_type->points_to()->array_type() != NULL
	    && !arg_type->points_to()->is_slice_type())
	  arg_type = arg_type->points_to();

	if (arg_type->array_type() != NULL
	    && arg_type->array_type()->length() != NULL)
          {
	    this->seen_ = true;
	    bool ret = Builtin_call_expression::array_len_is_constant(arg);
	    this->seen_ = false;
	    return ret;
          }

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
	return (arg->field_reference_expression() != NULL
		|| arg->classification() == Expression::EXPRESSION_SELECTOR);
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

// Return whether a builtin call is untyped.  Most builtin functions
// have a known type, but complex, real, and imag can be untyped.

bool
Builtin_call_expression::do_is_untyped(Type** ptype) const
{
  if (this->is_error_expression())
    return false;

  switch (this->code_)
    {
    default:
      return false;

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() != 2)
	  return false;
	Type* dummy;
	if (!args->front()->is_untyped(&dummy)
	    || !args->back()->is_untyped(&dummy))
	  return false;
	*ptype = Type::make_abstract_complex_type();
	return true;
      }

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	Expression* arg = this->one_arg();
	if (arg == NULL)
	  return false;
	if (!arg->is_untyped(ptype))
	  return false;
	*ptype = Type::make_abstract_float_type();
	return true;
      }
    }
}

// Return a numeric constant if possible.

bool
Builtin_call_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->code_ == BUILTIN_LEN
      || this->code_ == BUILTIN_CAP)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;

      // We may be called before the determine_types pass.
      arg->determine_type_no_context(this->gogo_);

      Type* arg_type = arg->type();
      if (arg_type->is_error())
	return false;

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

	  if (!arg_type->is_error())
	    {
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
    }
  else if (this->code_ == BUILTIN_SIZEOF
	   || this->code_ == BUILTIN_ALIGNOF)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;

      // We may be called before the determine_types pass.
      arg->determine_type_no_context(this->gogo_);

      Type* arg_type = arg->type();
      if (arg_type->is_error())
	return false;
      if (arg_type->is_abstract())
	arg_type = arg_type->make_non_abstract_type();
      if (this->seen_)
        return false;

      int64_t ret;
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

      mpz_t zval;
      set_mpz_from_int64(&zval, ret);
      nc->set_int(Type::lookup_integer_type("uintptr"), zval);
      mpz_clear(zval);
      return true;
    }
  else if (this->code_ == BUILTIN_OFFSETOF)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;

      // We may be called before the determine_types pass.
      arg->determine_type_no_context(this->gogo_);

      Field_reference_expression* farg = arg->field_reference_expression();
      if (farg == NULL)
	return false;
      if (this->seen_)
        return false;

      int64_t total_offset = 0;
      while (true)
        {
          Expression* struct_expr = farg->expr();
          Type* st = struct_expr->type();
          if (st->struct_type() == NULL)
            return false;
          if (st->named_type() != NULL)
            st->named_type()->convert(this->gogo_);
          if (st->is_error_type())
            {
              go_assert(saw_errors());
              return false;
            }
          int64_t offset;
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
      mpz_t zval;
      set_mpz_from_int64(&zval, total_offset);
      nc->set_int(Type::lookup_integer_type("uintptr"), zval);
      mpz_clear(zval);
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

      mpc_t val;
      if (!argnc.to_complex(&val))
	return false;

      Type* type = Builtin_call_expression::real_imag_type(argnc.type());
      if (this->code_ == BUILTIN_REAL)
	nc->set_float(type, mpc_realref(val));
      else
	nc->set_float(type, mpc_imagref(val));
      mpc_clear(val);
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
	  && !Type::are_identical(rnc.type(), inc.type(),
				  Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
				  NULL))
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

      mpc_t val;
      mpc_init2(val, mpc_precision);
      mpc_set_fr_fr(val, r, i, MPC_RNDNN);
      mpfr_clear(r);
      mpfr_clear(i);

      Type* type = Builtin_call_expression::complex_type(arg_type);
      nc->set_complex(type, val);

      mpc_clear(val);

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
    case BUILTIN_ADD:
    case BUILTIN_ALIGNOF:
    case BUILTIN_OFFSETOF:
    case BUILTIN_SIZEOF:
    case BUILTIN_SLICE:
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
  if (this->is_error_expression())
    return Type::make_error_type();

  Type* type = this->type();
  if (type != NULL)
    return type;

  switch (this->code_)
    {
    case BUILTIN_INVALID:
    default:
      return Type::make_error_type();

    case BUILTIN_NEW:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->empty())
	  return Type::make_error_type();
	return Type::make_pointer_type(args->front()->type());
      }

    case BUILTIN_MAKE:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->empty())
	  return Type::make_error_type();
	return args->front()->type();
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

    case BUILTIN_ADD:
      return Type::make_pointer_type(Type::make_void_type());

    case BUILTIN_SLICE:
      const Expression_list* args = this->args();
      if (args == NULL || args->size() != 2)
	return Type::make_error_type();
      Type* pt = args->front()->type()->points_to();
      if (pt == NULL)
	return Type::make_error_type();
      return Type::make_array_type(pt, NULL);
    }
}

// Determine the type.

void
Builtin_call_expression::do_determine_type(Gogo* gogo,
					   const Type_context* context)
{
  if (!this->determining_types())
    return;

  this->fn()->determine_type_no_context(gogo);

  this->simplify_multiple_results(gogo);

  const Expression_list* args = this->args();

  bool is_print;
  Type* arg_type = NULL;
  Type* trailing_arg_types = NULL;
  switch (this->code_)
    {
    case BUILTIN_MAKE:
      trailing_arg_types = Type::lookup_integer_type("int");
      is_print = false;
      break;

    case BUILTIN_PANIC:
      arg_type =
	Type::make_empty_interface_type(Linemap::predeclared_location());
      is_print = false;
      break;

    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      // Do not force a large integer constant to "int".
      is_print = true;
      break;

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	// We need the argument to determine the type, so check it now
	// before any call to the do_type method.
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 1)
	  {
	    this->report_error(_("not enough arguments"));
	    return;
	  }
	else if (args->size() > 1)
	  {
	    this->report_error(_("too many arguments"));
	    return;
	  }

	Type* dummy;
	if (context->type != NULL
	    && context->type->is_numeric_type()
	    && this->is_untyped(&dummy))
	  {
	    Type* type = context->type;
	    if (type->is_abstract() && !context->may_be_abstract)
	      type = type->make_non_abstract_type();
	    this->set_type(type);
	  }
	else if (context->may_be_abstract && this->is_constant())
	  this->set_type(Type::make_abstract_float_type());

	arg_type = Builtin_call_expression::complex_type(context->type);
	if (arg_type == NULL)
	  {
	    if (context->may_be_abstract)
	      arg_type = Type::make_abstract_complex_type();
	    else
	      arg_type = Type::lookup_complex_type("complex128");
	  }

	if (!args->front()->is_untyped(&dummy))
	  {
	    Type_context subcontext(arg_type, context->may_be_abstract);
	    args->front()->determine_type(gogo, &subcontext);
	  }

	is_print = false;
      }
      break;

    case BUILTIN_COMPLEX:
      {
	// We need the arguments to determine the type, so check them
	// now before any call to the do_type method.
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 2)
	  {
	    this->report_error(_("not enough arguments"));
	    return;
	  }
	else if (args->size() > 2)
	  {
	    this->report_error(_("too many arguments"));
	    return;
	  }

	Type* dummy;
	if (context->type != NULL
	    && context->type->is_numeric_type()
	    && this->is_untyped(&dummy))
	  {
	    Type* type = context->type;
	    if (type->is_abstract() && !context->may_be_abstract)
	      type = type->make_non_abstract_type();
	    this->set_type(type);
	  }
	else if (context->may_be_abstract && this->is_constant())
	  this->set_type(Type::make_abstract_complex_type());

	// For the complex function the type of one operand can
	// determine the type of the other, as in a binary expression.
	arg_type = Builtin_call_expression::real_imag_type(context->type);
	if (arg_type == NULL)
	  {
	    if (context->may_be_abstract)
	      arg_type = Type::make_abstract_float_type();
	    else
	      arg_type = Type::lookup_float_type("float64");
	  }

	Type_context subcontext(arg_type, context->may_be_abstract);
	if (!args->front()->is_untyped(&dummy))
	  {
	    args->front()->determine_type(gogo, &subcontext);
	    arg_type = args->front()->type();
	  }
	else if (!args->back()->is_untyped(&dummy))
	  {
	    args->back()->determine_type(gogo, &subcontext);
	    arg_type = args->back()->type();
	  }

	is_print = false;
      }
      break;

    case BUILTIN_APPEND:
      if (!this->is_varargs()
	  && args != NULL
	  && !args->empty())
	{
	  args->front()->determine_type_no_context(gogo);
	  if (args->front()->type()->is_slice_type())
	    trailing_arg_types =
	      args->front()->type()->array_type()->element_type();
	}
      is_print = false;
      break;

    case BUILTIN_ADD:
    case BUILTIN_SLICE:
      // Both unsafe.Add and unsafe.Slice take two arguments, and the
      // second arguments defaults to "int".
      if (args != NULL && args->size() == 2)
	{
	  if (this->code_ == BUILTIN_SLICE)
	    args->front()->determine_type_no_context(gogo);
	  else
	    {
	      Type* pointer = Type::make_pointer_type(Type::make_void_type());
	      Type_context subcontext(pointer, false);
	      args->front()->determine_type(gogo, &subcontext);
	    }
	  Type* int_type = Type::lookup_integer_type("int");
	  Type_context subcontext(int_type, false);
	  args->back()->determine_type(gogo, &subcontext);
	  return;
	}
      is_print = false;
      break;

    case BUILTIN_DELETE:
      if (args != NULL && args->size() == 2)
	{
	  args->front()->determine_type_no_context(gogo);
	  Map_type* mt = args->front()->type()->map_type();
	  if (mt != NULL)
	    trailing_arg_types = mt->key_type();
	}
      is_print = false;
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

	  if (is_print && (*pa)->is_constant())
	    {
	      // We want to print large constants, we so can't just
	      // use the appropriate nonabstract type.  Use uint64 for
	      // an integer if we know it is nonnegative, otherwise
	      // use int64 for a integer, otherwise use float64 for a
	      // float or complex128 for a complex.
	      Type* atype;
	      if ((*pa)->is_untyped(&atype))
		{
		  Type* want_type = NULL;
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

	  (*pa)->determine_type(gogo, &subcontext);

	  if (trailing_arg_types != NULL)
	    {
	      arg_type = trailing_arg_types;
	      trailing_arg_types = NULL;
	    }
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
Builtin_call_expression::do_check_types(Gogo* gogo)
{
  if (this->is_error_expression())
    return;

  if (this->is_varargs() && this->code_ != BUILTIN_APPEND)
    {
      go_error_at(this->location(),
		  "invalid use of %<...%> with built-in function");
      this->set_is_error();
      return;
    }

  switch (this->code_)
    {
    case BUILTIN_INVALID:
      return;

    case BUILTIN_NEW:
      if (this->check_one_arg())
	{
	  Expression* arg = this->one_arg();
	  if (!arg->is_type_expression())
	    {
	      go_error_at(arg->location(), "expected type");
	      this->set_is_error();
	    }
	}
      break;

    case BUILTIN_MAKE:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 1)
	  {
	    this->report_error(_("not enough arguments"));
	    return;
	  }

	Expression* first_arg = args->front();
	if (!first_arg->is_type_expression())
	  {
	    go_error_at(first_arg->location(), "expected type");
	    this->set_is_error();
	    return;
	  }

	Type* type = first_arg->type();
	if (!type->in_heap())
	  go_error_at(first_arg->location(),
		      "cannot make slice of go:notinheap type");

	bool is_slice = type->is_slice_type();
	if (!is_slice
	    && type->map_type() == NULL
	    && type->channel_type() == NULL)
	  {
	    this->report_error(_("invalid type for make function"));
	    return;
	  }

	Expression_list::const_iterator parg = args->begin();
	++parg;
	if (parg == args->end())
	  {
	    if (is_slice)
	      {
		this->report_error(_("length required when "
				     "allocating a slice"));
		return;
	      }
	  }
	else
	  {
	    if ((*parg)->type()->integer_type() == NULL)
	      {
		go_error_at((*parg)->location(),
			    "non-integer len argument in make");
		return;
	      }
	    ++parg;

	    if (is_slice && parg != args->end())
	      {
		if ((*parg)->type()->integer_type() == NULL)
		  {
		    go_error_at((*parg)->location(),
				"non-integer cap argument in make");
		    return;
		  }
		++parg;
	      }
	  }

	if (parg != args->end())
	  {
	    this->report_error(_("too many arguments to make"));
	    return;
	  }
      }
      break;

    case BUILTIN_DELETE:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 2)
	  this->report_error(_("not enough arguments"));
	else if (args->size() > 2)
	  this->report_error(_("too many arguments"));
	else if (args->front()->type()->map_type() == NULL)
	  this->report_error(_("argument 1 must be a map"));
	else
	  {
            Type* key_type =
              args->front()->type()->map_type()->key_type();
            Expression_list::iterator pa = this->args()->begin();
            pa++;
            Type* arg_type = (*pa)->type();
            std::string reason;
            if (!Type::are_assignable(key_type, arg_type, &reason))
              {
                if (reason.empty())
                  go_error_at(this->location(),
			      "argument 2 has incompatible type");
                else
                  go_error_at(this->location(),
			      "argument 2 has incompatible type (%s)",
                              reason.c_str());
                this->set_is_error();
              }
	  }
      }
      break;

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
	if (args != NULL)
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
		  {
		    // Report errors in the expression first.
		    (*p)->check_types(gogo);
		    if (!(*p)->is_error_expression())
		      this->report_error(_("unsupported argument type to "
					   "builtin function"));
		  }
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
      if (this->check_one_arg())
        {
	  Expression* arg = this->one_arg();
	  if (arg->type()->is_void_type())
	    this->report_error(_("argument to builtin has void type"));
        }
      break;

    case BUILTIN_RECOVER:
      if (this->args() != NULL
	  && !this->args()->empty()
	  && !this->recover_arg_is_set_)
	this->report_error(_("too many arguments"));
      break;

    case BUILTIN_OFFSETOF:
      if (this->check_one_arg())
	{
	  Expression* arg = this->one_arg();
	  if (arg->classification() == Expression::EXPRESSION_SELECTOR)
	    {
	      Selector_expression* se = static_cast<Selector_expression*>(arg);
	      Expression* resolved = se->resolved();
	      if (resolved != NULL)
		arg = resolved;
	    }
	  if (arg->is_error_expression())
	    ;
	  else if (arg->bound_method_expression() != NULL
		   || arg->interface_field_reference_expression() != NULL)
	    this->report_error(_("invalid use of method value as "
				 "argument of Offsetof"));
	  else if (arg->field_reference_expression() == NULL)
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
	  {
	    this->set_is_error();
	    break;
	  }

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
	    if (!Type::are_identical(e1, e2, Type::COMPARE_TAGS, NULL))
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
	if (args == NULL || args->empty())
	  {
	    this->report_error(_("not enough arguments"));
	    break;
	  }

	Type* slice_type = args->front()->type();
	if (!slice_type->is_slice_type())
	  {
	    if (slice_type->is_error_type())
	      break;
	    if (slice_type->is_nil_type())
	      go_error_at(args->front()->location(), "use of untyped nil");
	    else
	      go_error_at(args->front()->location(),
			  "argument 1 must be a slice");
	    this->set_is_error();
	    break;
	  }

	Type* element_type = slice_type->array_type()->element_type();
	if (!element_type->in_heap())
	  go_error_at(args->front()->location(),
		      "cannot append to slice of go:notinheap type");
	if (this->is_varargs())
	  {
	    if (!args->back()->type()->is_slice_type()
		&& !args->back()->type()->is_string_type())
	      {
		go_error_at(args->back()->location(),
			    "invalid use of %<...%> with non-slice/non-string");
		this->set_is_error();
		break;
	      }

	    if (args->size() < 2)
	      {
		this->report_error(_("not enough arguments"));
		break;
	      }
	    if (args->size() > 2)
	      {
		this->report_error(_("too many arguments"));
		break;
	      }

	    if (args->back()->type()->is_string_type()
		&& element_type->integer_type() != NULL
		&& element_type->integer_type()->is_byte())
	      {
		// Permit append(s1, s2...) when s1 is a slice of
		// bytes and s2 is a string type.
	      }
	    else
	      {
		// We have to test for assignment compatibility to a
		// slice of the element type, which is not necessarily
		// the same as the type of the first argument: the
		// first argument might have a named type.
		Type* check_type = Type::make_array_type(element_type, NULL);
		std::string reason;
		if (!Type::are_assignable(check_type, args->back()->type(),
					  &reason))
		  {
		    if (reason.empty())
		      go_error_at(args->back()->location(),
				  "argument 2 has invalid type");
		    else
		      go_error_at(args->back()->location(),
				  "argument 2 has invalid type (%s)",
				  reason.c_str());
		    this->set_is_error();
		    break;
		  }
	      }
	  }
	else
	  {
	    Expression_list::const_iterator pa = args->begin();
	    int i = 2;
	    for (++pa; pa != args->end(); ++pa, ++i)
	      {
		std::string reason;
		if (!Type::are_assignable(element_type, (*pa)->type(),
					  &reason))
		  {
		    if (reason.empty())
		      go_error_at((*pa)->location(),
				  "argument %d has incompatible type", i);
		    else
		      go_error_at((*pa)->location(),
				  "argument %d has incompatible type (%s)",
				  i, reason.c_str());
		    this->set_is_error();
		  }
	      }
	  }
      }
      break;

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
	go_assert(args != NULL && args->size() == 2);
	if (args->front()->is_error_expression()
	    || args->front()->type()->is_error()
	    || args->back()->is_error_expression()
	    || args->back()->type()->is_error())
	  this->set_is_error();
	else if (!Type::are_identical(args->front()->type(),
				      args->back()->type(),
				      Type::COMPARE_TAGS, NULL))
	  this->report_error(_("complex arguments must have identical types"));
	else if (args->front()->type()->float_type() == NULL)
	  this->report_error(_("complex arguments must have "
			       "floating-point type"));
      }
      break;

    case BUILTIN_ADD:
    case BUILTIN_SLICE:
      {
	Numeric_constant nc;
	unsigned long v;
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
	else if (args->back()->type()->integer_type() == NULL
		 && (!args->back()->type()->is_abstract()
		     || !args->back()->numeric_constant_value(&nc)
		     || (nc.to_unsigned_long(&v)
			 == Numeric_constant::NC_UL_NOTINT)))
	  {
	    if (this->code_ == BUILTIN_ADD)
	      go_error_at(args->back()->location(), "non-integer offset");
	    else
	      go_error_at(args->back()->location(), "non-integer size");
	  }
	else if (this->code_ == BUILTIN_ADD)
	  {
	    Type* pointer_type =
	      Type::make_pointer_type(Type::make_void_type());
	    std::string reason;
	    if (!Type::are_assignable(pointer_type, args->front()->type(),
				      &reason))
	      {
		if (reason.empty())
		  go_error_at(args->front()->location(),
			      "argument 1 has incompatible type");
		else
		  go_error_at(args->front()->location(),
			      "argument 1 has incompatible type (%s)",
			      reason.c_str());
		this->set_is_error();
	      }
	  }
	else
	  {
	    if (args->front()->type()->points_to() == NULL)
	      {
		go_error_at(args->front()->location(),
			    "argument 1 must be a pointer");
		this->set_is_error();
	      }

	    unsigned int int_bits =
	      Type::lookup_integer_type("int")->integer_type()->bits();

	    mpz_t ival;
	    if (args->back()->numeric_constant_value(&nc) && nc.to_int(&ival))
	      {
		if (mpz_sgn(ival) < 0
		    || mpz_sizeinbase(ival, 2) >= int_bits)
		  {
		    go_error_at(args->back()->location(),
				"slice length out of range");
		    this->set_is_error();
		  }
		mpz_clear(ival);
	      }
	  }
      }
      break;

    default:
      go_unreachable();
    }
}

Expression*
Builtin_call_expression::do_copy()
{
  Call_expression* bce =
    new Builtin_call_expression(this->gogo_, this->fn()->copy(),
				(this->args() == NULL
				 ? NULL
				 : this->args()->copy()),
				this->is_varargs(),
				this->location());

  if (this->varargs_are_lowered())
    bce->set_varargs_are_lowered();
  if (this->is_deferred())
    bce->set_is_deferred();
  if (this->is_concurrent())
    bce->set_is_concurrent();
  return bce;
}

// Return the backend representation for a builtin function.

Bexpression*
Builtin_call_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location location = this->location();

  if (this->is_erroneous_call())
    {
      go_assert(saw_errors());
      return gogo->backend()->error_expression();
    }

  switch (this->code_)
    {
    case BUILTIN_INVALID:
    case BUILTIN_NEW:
    case BUILTIN_MAKE:
    case BUILTIN_ADD:
    case BUILTIN_SLICE:
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
	    return context->backend()->error_expression();
	  }
	this->seen_ = true;
	this->seen_ = false;
	if (arg_type->points_to() != NULL)
	  {
	    arg_type = arg_type->points_to();
	    go_assert(arg_type->array_type() != NULL
		       && !arg_type->is_slice_type());
            arg = Expression::make_dereference(arg, NIL_CHECK_DEFAULT,
                                               location);
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
		    return context->backend()->error_expression();
		  }
		this->seen_ = true;
	        val = arg_type->array_type()->get_length(gogo, arg);
		this->seen_ = false;
	      }
	    else if (arg_type->map_type() != NULL
		     || arg_type->channel_type() != NULL)
	      {
		// The first field is the length.  If the pointer is
		// nil, the length is zero.
		Type* pint_type = Type::make_pointer_type(int_type);
		arg = Expression::make_unsafe_cast(pint_type, arg, location);
		Expression* nil = Expression::make_nil(location);
		nil = Expression::make_cast(pint_type, nil, location);
		Expression* cmp = Expression::make_binary(OPERATOR_EQEQ,
							  arg, nil, location);
		Expression* zero = Expression::make_integer_ul(0, int_type,
							       location);
                Expression* indir =
                    Expression::make_dereference(arg, NIL_CHECK_NOT_NEEDED,
                                                 location);
		val = Expression::make_conditional(cmp, zero, indir, location);
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
		    return context->backend()->error_expression();
		  }
		this->seen_ = true;
                val = arg_type->array_type()->get_capacity(gogo, arg);
		this->seen_ = false;
	      }
	    else if (arg_type->channel_type() != NULL)
	      {
		// The second field is the capacity.  If the pointer
		// is nil, the capacity is zero.
		Type* uintptr_type = Type::lookup_integer_type("uintptr");
		Type* pint_type = Type::make_pointer_type(int_type);
		Expression* parg = Expression::make_unsafe_cast(uintptr_type,
								arg,
								location);
		int off = int_type->integer_type()->bits() / 8;
		Expression* eoff = Expression::make_integer_ul(off,
							       uintptr_type,
							       location);
		parg = Expression::make_binary(OPERATOR_PLUS, parg, eoff,
					       location);
		parg = Expression::make_unsafe_cast(pint_type, parg, location);
		Expression* nil = Expression::make_nil(location);
		nil = Expression::make_cast(pint_type, nil, location);
		Expression* cmp = Expression::make_binary(OPERATOR_EQEQ,
							  arg, nil, location);
		Expression* zero = Expression::make_integer_ul(0, int_type,
							       location);
                Expression* indir =
                    Expression::make_dereference(parg, NIL_CHECK_NOT_NEEDED,
                                                 location);
		val = Expression::make_conditional(cmp, zero, indir, location);
	      }
	    else
	      go_unreachable();
	  }

	Expression* e = Expression::make_cast(int_type, val, location);
	e->determine_type_no_context(gogo);
	return e->get_backend(context);
      }

    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      {
	const bool is_ln = this->code_ == BUILTIN_PRINTLN;

	Expression* print_stmts = Runtime::make_call(gogo, Runtime::PRINTLOCK,
						     location, 0);

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
		      Runtime::make_call(gogo, Runtime::PRINTSP, location, 0);

                    print_stmts =
                        Expression::make_compound(print_stmts, print_space,
                                                  location);
		  }

                Expression* arg = *p;
		Type* type = arg->type();
                Runtime::Function code;
		if (type->is_string_type())
                  code = Runtime::PRINTSTRING;
		else if (type->integer_type() != NULL
			 && type->integer_type()->is_unsigned())
		  {
		    Type* itype = Type::lookup_integer_type("uint64");
		    arg = Expression::make_cast(itype, arg, location);
                    if (gogo->compiling_runtime()
                        && type->named_type() != NULL
                        && gogo->unpack_hidden_name(type->named_type()->name())
                           == "hex")
                      code = Runtime::PRINTHEX;
                    else
                      code = Runtime::PRINTUINT;
		  }
		else if (type->integer_type() != NULL)
		  {
		    Type* itype = Type::lookup_integer_type("int64");
		    arg = Expression::make_cast(itype, arg, location);
                    code = Runtime::PRINTINT;
		  }
		else if (type->float_type() != NULL)
		  {
                    Type* dtype = Type::lookup_float_type("float64");
                    arg = Expression::make_cast(dtype, arg, location);
                    code = Runtime::PRINTFLOAT;
		  }
		else if (type->complex_type() != NULL)
		  {
                    Type* ctype = Type::lookup_complex_type("complex128");
                    arg = Expression::make_cast(ctype, arg, location);
                    code = Runtime::PRINTCOMPLEX;
		  }
		else if (type->is_boolean_type())
                  code = Runtime::PRINTBOOL;
		else if (type->points_to() != NULL
			 || type->channel_type() != NULL
			 || type->map_type() != NULL
			 || type->function_type() != NULL)
		  {
                    arg = Expression::make_cast(type, arg, location);
                    code = Runtime::PRINTPOINTER;
		  }
		else if (type->interface_type() != NULL)
		  {
		    if (type->interface_type()->is_empty())
                      code = Runtime::PRINTEFACE;
		    else
                      code = Runtime::PRINTIFACE;
		  }
		else if (type->is_slice_type())
                  code = Runtime::PRINTSLICE;
		else
		  {
		    go_assert(saw_errors());
		    return context->backend()->error_expression();
		  }

                Expression* call = Runtime::make_call(gogo, code, location, 1,
						      arg);
		print_stmts = Expression::make_compound(print_stmts, call,
							location);
	      }
	  }

	if (is_ln)
	  {
            Expression* print_nl =
	      Runtime::make_call(gogo, Runtime::PRINTNL, location, 0);
	    print_stmts = Expression::make_compound(print_stmts, print_nl,
						    location);
	  }

	Expression* unlock = Runtime::make_call(gogo, Runtime::PRINTUNLOCK,
						location, 0);
	print_stmts = Expression::make_compound(print_stmts, unlock, location);

	print_stmts->determine_type_no_context(gogo);

        return print_stmts->get_backend(context);
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
	  Runtime::make_call(gogo, Runtime::GOPANIC, location, 1, arg);
	panic->determine_type_no_context(gogo);
        return panic->get_backend(context);
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
        nil = Expression::make_interface_value(empty, nil, nil, location);

	// We need to handle a deferred call to recover specially,
	// because it changes whether it can recover a panic or not.
	// See test7 in test/recover1.go.
        Expression* recover = Runtime::make_call(gogo,
						 (this->is_deferred()
                                                  ? Runtime::DEFERREDRECOVER
                                                  : Runtime::GORECOVER),
                                                 location, 0);
        Expression* cond =
            Expression::make_conditional(arg, recover, nil, location);
	cond->determine_type_no_context(gogo);
        return cond->get_backend(context);
      }

    case BUILTIN_CLOSE:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
        Expression* close = Runtime::make_call(gogo, Runtime::CLOSE, location,
					       1, arg);
	close->determine_type_no_context(gogo);
        return close->get_backend(context);
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
	    return context->backend()->error_expression();
	  }
	Type* uintptr_type = Type::lookup_integer_type("uintptr");
        mpz_t ival;
        nc.get_int(&ival);
        Expression* int_cst =
            Expression::make_integer_z(&ival, uintptr_type, location);
        mpz_clear(ival);
        return int_cst->get_backend(context);
      }

    case BUILTIN_COPY:
      // Handled in Builtin_call_expression::do_flatten.
      go_unreachable();

    case BUILTIN_APPEND:
      // Handled in Builtin_call_expression::flatten_append.
      go_unreachable();

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);

        Bexpression* ret;
        Bexpression* bcomplex = args->front()->get_backend(context);
        if (this->code_ == BUILTIN_REAL)
          ret = gogo->backend()->real_part_expression(bcomplex, location);
        else
          ret = gogo->backend()->imag_part_expression(bcomplex, location);
        return ret;
      }

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	Bexpression* breal = args->front()->get_backend(context);
	Bexpression* bimag = args->back()->get_backend(context);
	return gogo->backend()->complex_expression(breal, bimag, location);
      }

    default:
      go_unreachable();
    }
}

// We have to support exporting a builtin call expression, because
// code can set a constant to the result of a builtin expression.

void
Builtin_call_expression::do_export(Export_function_body* efb) const
{
  if (this->code_ == BUILTIN_ADD || this->code_ == BUILTIN_SLICE)
    {
      char buf[50];
      snprintf(buf, sizeof buf, "<p%d>%s", efb->unsafe_package_index(),
	       (this->code_ == BUILTIN_ADD ? "Add" : "Slice"));
      efb->write_c_string(buf);
      this->export_arguments(efb);
    }
  else
    {
      const char *s = NULL;
      switch (this->code_)
	{
	default:
	  go_unreachable();
	case BUILTIN_APPEND:
	  s = "append";
	  break;
	case BUILTIN_COPY:
	  s = "copy";
	  break;
	case BUILTIN_LEN:
	  s = "len";
	  break;
	case BUILTIN_CAP:
	  s = "cap";
	  break;
	case BUILTIN_DELETE:
	  s = "delete";
	  break;
	case BUILTIN_PRINT:
	  s = "print";
	  break;
	case BUILTIN_PRINTLN:
	  s = "println";
	  break;
	case BUILTIN_PANIC:
	  s = "panic";
	  break;
	case BUILTIN_RECOVER:
	  s = "recover";
	  break;
	case BUILTIN_CLOSE:
	  s = "close";
	  break;
	case BUILTIN_REAL:
	  s = "real";
	  break;
	case BUILTIN_IMAG:
	  s = "imag";
	  break;
	case BUILTIN_COMPLEX:
	  s = "complex";
	  break;
	}
      efb->write_c_string(s);
      this->export_arguments(efb);
    }
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
  if (this->lowered_ != NULL)
    return Expression::traverse(&this->lowered_, traverse);

  // If we are calling a function in a different package that returns
  // an unnamed type, this may be the only chance we get to traverse
  // that type.  We don't traverse this->type_ because it may be a
  // Call_multiple_result_type that will just lead back here.
  if (this->type_ != NULL && !this->type_->is_error_type())
    {
      Function_type *fntype = this->get_function_type();
      if (fntype != NULL && Type::traverse(fntype, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (Expression::traverse(&this->fn_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->args_ != NULL)
    {
      if (this->args_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Lower a Call_expression to a Builtin_call_expression.  This happens
// early on, before determine_types.

Expression*
Call_expression::lower_builtin(Gogo* gogo)
{
  // This is called before determine_types, so we can't call
  // this->fn_->type().  Fortunately builtin calls require a direct
  // reference to the builtin.
  Expression* fn = this->fn_;
  Named_object* no;
  if (fn->func_expression() != NULL)
    no = fn->func_expression()->named_object();
  else if (fn->unknown_expression() != NULL)
    {
      no = fn->unknown_expression()->named_object();
      if (no->is_unknown())
	{
	  no = no->unknown_value()->real_named_object();
	  if (no == NULL)
	    return this;
	}
    }
  else
    return this;

  if (!no->is_function_declaration())
    return this;
  if (!no->func_declaration_value()->type()->is_builtin())
    return this;

  if (fn->unknown_expression() != NULL)
    fn = Expression::make_func_reference(no, NULL, fn->location());

  Builtin_call_expression* bce = new Builtin_call_expression(gogo, fn,
							     this->args_,
							     this->is_varargs_,
							     this->location());
  if (this->is_deferred_)
    bce->set_is_deferred();
  if (this->is_concurrent_)
    bce->set_is_concurrent();
  return bce;
}

// A type conversion can be a constant.

bool
Call_expression::do_is_constant() const
{
  if (this->lowered_ != NULL)
    return this->lowered_->is_constant();
  if (this->fn_->is_type_expression()
      && this->args_ != NULL
      && this->args_->size() == 1)
    return this->args_->front()->is_constant();
  return false;
}

bool
Call_expression::do_is_untyped(Type** ptype) const
{
  if (this->lowered_ != NULL)
    return this->lowered_->is_untyped(ptype);
  return false;
}

bool
Call_expression::do_numeric_constant_value(Numeric_constant* nc)
{
  if (this->lowered_ != NULL)
    return this->lowered_->numeric_constant_value(nc);
  if (this->fn_->is_type_expression()
      && this->args_ != NULL
      && this->args_->size() == 1)
    {
      // If we get here, it's before the determine_types pass, so we
      // have to pull apart the type carefully.  This is a hack that
      // is needed because the finalize_methods needs to be able to
      // determine whether the length of an array is 1.

      Type* type;
      if (this->fn_->classification() == EXPRESSION_TYPE)
	type = this->fn_->type();
      else if (this->fn_->unknown_expression() != NULL)
	{
	  Named_object* no = this->fn_->unknown_expression()->named_object();
	  if (no->is_unknown())
	    {
	      no = no->unknown_value()->real_named_object();
	      go_assert(no != NULL);
	    }
	  type = no->type_value();
	}
      else
	return false;

      if (!type->is_numeric_type())
	return false;
      if (!this->args_->front()->numeric_constant_value(nc))
	return false;
      return nc->set_type(type, false, this->location());
    }
  return false;
}

bool
Call_expression::do_discarding_value()
{
  if (this->fn_->is_type_expression())
    {
      this->unused_value_error();
      return false;
    }
  return true;
}

// Lower a call statement.

Expression*
Call_expression::do_lower(Gogo* gogo, Named_object*,
			  Statement_inserter* inserter)
{
  if (this->lowered_ != NULL)
    return this->lowered_;

  Location loc = this->location();

  if (this->is_error_expression())
    return Expression::make_error(loc);

  // Although we've already lowered calls to builtin functions, we may
  // still see calls generated to builtins elsewhere in the lowering
  // pass.  It's simpler to handle them here.
  Expression* builtin = this->lower_builtin(gogo);
  if (builtin != this)
    return builtin;

  // If this call returns multiple results, create a temporary
  // variable to hold them.
  if (this->result_count() > 1 && this->call_temp_ == NULL)
    {
      Struct_field_list* sfl = new Struct_field_list();
      const Typed_identifier_list* results =
	this->get_function_type()->results();

      int i = 0;
      char buf[20];
      for (Typed_identifier_list::const_iterator p = results->begin();
           p != results->end();
           ++p, ++i)
        {
          snprintf(buf, sizeof buf, "res%d", i);
          sfl->push_back(Struct_field(Typed_identifier(buf, p->type(), loc)));
        }

      Struct_type* st = Type::make_struct_type(sfl, loc);
      st->set_is_struct_incomparable();
      st->set_is_results_struct();
      this->call_temp_ = Statement::make_temporary(st, NULL, loc);
      inserter->insert(this->call_temp_);
    }

  // If this is call to a method, call the method directly passing the
  // object as the first parameter.
  Bound_method_expression* bme = this->fn_->bound_method_expression();
  if (bme != NULL && !this->is_deferred_ && !this->is_concurrent_)
    {
      Named_object* methodfn = bme->function();
      Function_type* mft = (methodfn->is_function()
                            ? methodfn->func_value()->type()
                            : methodfn->func_declaration_value()->type());
      Expression* first_arg = bme->first_argument();

      // We always pass a pointer when calling a method, except for
      // direct interface types when calling a value method.
      if (!first_arg->type()->is_error()
          && first_arg->type()->points_to() == NULL
          && !first_arg->type()->is_direct_iface_type())
	{
	  first_arg = Expression::make_unary(OPERATOR_AND, first_arg, loc);
	  // We may need to create a temporary variable so that we can
	  // take the address.  We can't do that here because it will
	  // mess up the order of evaluation.
	  Unary_expression* ue = static_cast<Unary_expression*>(first_arg);
	  ue->set_create_temp();
	}
      else if (mft->receiver()->type()->points_to() == NULL
               && first_arg->type()->points_to() != NULL
               && first_arg->type()->points_to()->is_direct_iface_type())
        first_arg = Expression::make_dereference(first_arg,
                                                 Expression::NIL_CHECK_DEFAULT,
                                                 loc);

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

      first_arg->determine_type_no_context(gogo);
      first_arg->check_types(gogo);

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

  // If this is a call to an imported function for which we have an
  // inlinable function body, add it to the list of functions to give
  // to the backend as inlining opportunities.
  Func_expression* fe = this->fn_->func_expression();
  if (fe != NULL
      && fe->named_object()->is_function_declaration()
      && fe->named_object()->func_declaration_value()->has_imported_body())
    gogo->add_imported_inlinable_function(fe->named_object());

  return this;
}

// Flatten a call with multiple results into a temporary.

Expression*
Call_expression::do_flatten(Gogo* gogo, Named_object*,
			    Statement_inserter* inserter)
{
  if (this->is_erroneous_call())
    {
      go_assert(saw_errors());
      return Expression::make_error(this->location());
    }

  if (this->is_flattened_)
    return this;
  this->is_flattened_ = true;

  // Add temporary variables for all arguments that require type
  // conversion.
  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    {
      go_assert(saw_errors());
      return this;
    }
  if (this->args_ != NULL && !this->args_->empty()
      && fntype->parameters() != NULL && !fntype->parameters()->empty())
    {
      bool is_interface_method =
	this->fn_->interface_field_reference_expression() != NULL;

      Expression_list *args = new Expression_list();
      Typed_identifier_list::const_iterator pp = fntype->parameters()->begin();
      Expression_list::const_iterator pa = this->args_->begin();
      if (!is_interface_method && fntype->is_method())
	{
	  // The receiver argument.
	  args->push_back(*pa);
	  ++pa;
	}
      for (; pa != this->args_->end(); ++pa, ++pp)
	{
	  go_assert(pp != fntype->parameters()->end());
	  if (Type::are_identical(pp->type(), (*pa)->type(),
				  Type::COMPARE_TAGS, NULL))
	    args->push_back(*pa);
	  else
	    {
	      Location loc = (*pa)->location();
	      Expression* arg = *pa;
	      if (!arg->is_multi_eval_safe())
		{
		  Temporary_statement *temp =
		    Statement::make_temporary(NULL, arg, loc);
		  inserter->insert(temp);
		  arg = Expression::make_temporary_reference(temp, loc);
		}
	      arg = Expression::convert_for_assignment(gogo, pp->type(), arg,
						       loc);
	      args->push_back(arg);
	    }
	}
      delete this->args_;
      this->args_ = args;
    }

  // Lower to compiler intrinsic if possible.
  Func_expression* fe = this->fn_->func_expression();
  if (!this->is_concurrent_ && !this->is_deferred_
      && fe != NULL
      && (fe->named_object()->is_function_declaration()
          || fe->named_object()->is_function()))
    {
      Expression* ret = this->intrinsify(gogo, inserter);
      if (ret != NULL)
	{
	  ret->determine_type_no_context(gogo);
	  return ret;
	}
    }

  // Add an implicit conversion to a boolean type, if needed.  See the
  // comment in Binary_expression::lower_array_comparison.
  if (this->is_equal_function_
      && this->type_ != NULL
      && this->type_ != Type::lookup_bool_type())
    return Expression::make_cast(this->type_, this, this->location());

  return this;
}

// Lower a call to a compiler intrinsic if possible.
// Returns NULL if it is not an intrinsic.

Expression*
Call_expression::intrinsify(Gogo* gogo,
                            Statement_inserter* inserter)
{
  Func_expression* fe = this->fn_->func_expression();
  Named_object* no = fe->named_object();
  std::string name = Gogo::unpack_hidden_name(no->name());
  std::string package = (no->package() != NULL
                         ? no->package()->pkgpath()
                         : gogo->pkgpath());
  bool is_method = ((no->is_function() && no->func_value()->is_method())
		    || (no->is_function_declaration()
			&& no->func_declaration_value()->is_method()));
  Location loc = this->location();

  Type* int_type = Type::lookup_integer_type("int");
  Type* int32_type = Type::lookup_integer_type("int32");
  Type* int64_type = Type::lookup_integer_type("int64");
  Type* uint_type = Type::lookup_integer_type("uint");
  Type* uint8_type = Type::lookup_integer_type("uint8");
  Type* uint32_type = Type::lookup_integer_type("uint32");
  Type* uint64_type = Type::lookup_integer_type("uint64");
  Type* uintptr_type = Type::lookup_integer_type("uintptr");
  Type* pointer_type = Type::make_pointer_type(Type::make_void_type());

  int int_size = int_type->named_type()->real_type()->integer_type()->bits() / 8;
  int ptr_size = uintptr_type->named_type()->real_type()->integer_type()->bits() / 8;

  if (package == "sync/atomic")
    {
      if (is_method)
	return NULL;

      // sync/atomic functions and runtime/internal/atomic functions
      // are very similar. In order not to duplicate code, we just
      // redirect to the latter and let the code below to handle them.
      // Note: no StorePointer, SwapPointer, and CompareAndSwapPointer,
      // as they need write barriers.
      if (name == "LoadInt32")
        name = "Loadint32";
      else if (name == "LoadInt64")
        name = "Loadint64";
      else if (name == "LoadUint32")
        name = "Load";
      else if (name == "LoadUint64")
        name = "Load64";
      else if (name == "LoadUintptr")
        name = "Loaduintptr";
      else if (name == "LoadPointer")
        name = "Loadp";
      else if (name == "StoreInt32")
        name = "Storeint32";
      else if (name == "StoreInt64")
        name = "Storeint64";
      else if (name == "StoreUint32")
        name = "Store";
      else if (name == "StoreUint64")
        name = "Store64";
      else if (name == "StoreUintptr")
        name = "Storeuintptr";
      else if (name == "AddInt32")
        name = "Xaddint32";
      else if (name == "AddInt64")
        name = "Xaddint64";
      else if (name == "AddUint32")
        name = "Xadd";
      else if (name == "AddUint64")
        name = "Xadd64";
      else if (name == "AddUintptr")
        name = "Xadduintptr";
      else if (name == "SwapInt32")
        name = "Xchgint32";
      else if (name == "SwapInt64")
        name = "Xchgint64";
      else if (name == "SwapUint32")
        name = "Xchg";
      else if (name == "SwapUint64")
        name = "Xchg64";
      else if (name == "SwapUintptr")
        name = "Xchguintptr";
      else if (name == "CompareAndSwapInt32")
        name = "Casint32";
      else if (name == "CompareAndSwapInt64")
        name = "Casint64";
      else if (name == "CompareAndSwapUint32")
        name = "Cas";
      else if (name == "CompareAndSwapUint64")
        name = "Cas64";
      else if (name == "CompareAndSwapUintptr")
        name = "Casuintptr";
      else
        return NULL;

      package = "runtime/internal/atomic";
    }

  if (package == "runtime/internal/sys")
    {
      if (is_method)
	return NULL;

      // runtime/internal/sys functions and math/bits functions
      // are very similar. In order not to duplicate code, we just
      // redirect to the latter and let the code below to handle them.
      if (name == "Bswap32")
        name = "ReverseBytes32";
      else if (name == "Bswap64")
        name = "ReverseBytes64";
      else if (name == "Ctz32")
        name = "TrailingZeros32";
      else if (name == "Ctz64")
        name = "TrailingZeros64";
      else
        return NULL;

      package = "math/bits";
    }

  if (package == "runtime")
    {
      if (is_method)
	return NULL;

      // Handle a couple of special runtime functions.  In the runtime
      // package, getcallerpc returns the PC of the caller, and
      // getcallersp returns the frame pointer of the caller.  Implement
      // these by turning them into calls to GCC builtin functions.  We
      // could implement them in normal code, but then we would have to
      // explicitly unwind the stack.  These functions are intended to be
      // efficient.  Note that this technique obviously only works for
      // direct calls, but that is the only way they are used.
      if (name == "getcallerpc"
          && (this->args_ == NULL || this->args_->size() == 0))
        {
          Expression* arg = Expression::make_integer_ul(0, uint32_type, loc);
          Expression* call =
            Runtime::make_call(gogo, Runtime::BUILTIN_RETURN_ADDRESS, loc,
                               1, arg);
          // The builtin functions return void*, but the Go functions return uintptr.
          return Expression::make_cast(uintptr_type, call, loc);
        }
      else if (name == "getcallersp"
               && (this->args_ == NULL || this->args_->size() == 0))

        {
          Expression* call =
            Runtime::make_call(gogo, Runtime::BUILTIN_DWARF_CFA, loc, 0);
          // The builtin functions return void*, but the Go functions return uintptr.
          return Expression::make_cast(uintptr_type, call, loc);
        }
    }
  else if (package == "math/bits")
    {
      if (is_method)
	return NULL;

      if ((name == "ReverseBytes16" || name == "ReverseBytes32"
           || name == "ReverseBytes64" || name == "ReverseBytes")
          && this->args_ != NULL && this->args_->size() == 1)
        {
          Runtime::Function code;
          if (name == "ReverseBytes16")
            code = Runtime::BUILTIN_BSWAP16;
          else if (name == "ReverseBytes32")
            code = Runtime::BUILTIN_BSWAP32;
          else if (name == "ReverseBytes64")
            code = Runtime::BUILTIN_BSWAP64;
          else if (name == "ReverseBytes")
            code = (int_size == 8 ? Runtime::BUILTIN_BSWAP64 : Runtime::BUILTIN_BSWAP32);
          else
            go_unreachable();
          Expression* arg = this->args_->front();
          Expression* call = Runtime::make_call(gogo, code, loc, 1, arg);
          if (name == "ReverseBytes")
            return Expression::make_cast(uint_type, call, loc);
          return call;
        }
      else if ((name == "TrailingZeros8" || name == "TrailingZeros16")
               && this->args_ != NULL && this->args_->size() == 1)
        {
          // GCC does not have a ctz8 or ctz16 intrinsic. We do
          // ctz32(0x100 | arg) or ctz32(0x10000 | arg).
          Expression* arg = this->args_->front();
          arg = Expression::make_cast(uint32_type, arg, loc);
          unsigned long mask = (name == "TrailingZeros8" ? 0x100 : 0x10000);
          Expression* c = Expression::make_integer_ul(mask, uint32_type, loc);
          arg = Expression::make_binary(OPERATOR_OR, arg, c, loc);
          Expression* call = Runtime::make_call(gogo, Runtime::BUILTIN_CTZ,
						loc, 1, arg);
          return Expression::make_cast(int_type, call, loc);
        }
      else if ((name == "TrailingZeros32"
                || (name == "TrailingZeros" && int_size == 4))
               && this->args_ != NULL && this->args_->size() == 1)
        {
          Expression* arg = this->args_->front();
          if (!arg->is_multi_eval_safe())
            {
              Temporary_statement* ts = Statement::make_temporary(uint32_type, arg, loc);
              inserter->insert(ts);
              arg = Expression::make_temporary_reference(ts, loc);
            }
          // arg == 0 ? 32 : __builtin_ctz(arg)
          Expression* zero = Expression::make_integer_ul(0, uint32_type, loc);
          Expression* cmp = Expression::make_binary(OPERATOR_EQEQ, arg, zero, loc);
          Expression* c32 = Expression::make_integer_ul(32, int_type, loc);
          Expression* call = Runtime::make_call(gogo, Runtime::BUILTIN_CTZ,
						loc, 1, arg->copy());
          call = Expression::make_cast(int_type, call, loc);
          return Expression::make_conditional(cmp, c32, call, loc);
        }
      else if ((name == "TrailingZeros64"
                || (name == "TrailingZeros" && int_size == 8))
               && this->args_ != NULL && this->args_->size() == 1)
        {
          Expression* arg = this->args_->front();
          if (!arg->is_multi_eval_safe())
            {
              Temporary_statement* ts = Statement::make_temporary(uint64_type, arg, loc);
              inserter->insert(ts);
              arg = Expression::make_temporary_reference(ts, loc);
            }
          // arg == 0 ? 64 : __builtin_ctzll(arg)
          Expression* zero = Expression::make_integer_ul(0, uint64_type, loc);
          Expression* cmp = Expression::make_binary(OPERATOR_EQEQ, arg, zero, loc);
          Expression* c64 = Expression::make_integer_ul(64, int_type, loc);
          Expression* call = Runtime::make_call(gogo, Runtime::BUILTIN_CTZLL,
						loc, 1, arg->copy());
          call = Expression::make_cast(int_type, call, loc);
          return Expression::make_conditional(cmp, c64, call, loc);
        }
      else if ((name == "LeadingZeros8" || name == "LeadingZeros16"
                || name == "Len8" || name == "Len16")
               && this->args_ != NULL && this->args_->size() == 1)
        {
          // GCC does not have a clz8 ir clz16 intrinsic. We do
          // clz32(arg<<24 | 0xffffff) or clz32(arg<<16 | 0xffff).
          Expression* arg = this->args_->front();
          arg = Expression::make_cast(uint32_type, arg, loc);
          unsigned long shift =
            ((name == "LeadingZeros8" || name == "Len8") ? 24 : 16);
          Expression* c = Expression::make_integer_ul(shift, uint32_type, loc);
          arg = Expression::make_binary(OPERATOR_LSHIFT, arg, c, loc);
          unsigned long mask =
            ((name == "LeadingZeros8" || name == "Len8") ? 0xffffff : 0xffff);
          c = Expression::make_integer_ul(mask, uint32_type, loc);
          arg = Expression::make_binary(OPERATOR_OR, arg, c, loc);
          Expression* call = Runtime::make_call(gogo, Runtime::BUILTIN_CLZ,
						loc, 1, arg);
          call = Expression::make_cast(int_type, call, loc);
          // len = width - clz
          if (name == "Len8")
            {
              c = Expression::make_integer_ul(8, int_type, loc);
              return Expression::make_binary(OPERATOR_MINUS, c, call, loc);
            }
          else if (name == "Len16")
            {
              c = Expression::make_integer_ul(16, int_type, loc);
              return Expression::make_binary(OPERATOR_MINUS, c, call, loc);
            }
          return call;
        }
      else if ((name == "LeadingZeros32" || name == "Len32"
                || ((name == "LeadingZeros" || name == "Len") && int_size == 4))
               && this->args_ != NULL && this->args_->size() == 1)
        {
          Expression* arg = this->args_->front();
          if (!arg->is_multi_eval_safe())
            {
              Temporary_statement* ts = Statement::make_temporary(uint32_type, arg, loc);
              inserter->insert(ts);
              arg = Expression::make_temporary_reference(ts, loc);
            }
          // arg == 0 ? 32 : __builtin_clz(arg)
          Expression* zero = Expression::make_integer_ul(0, uint32_type, loc);
          Expression* cmp = Expression::make_binary(OPERATOR_EQEQ, arg, zero, loc);
          Expression* c32 = Expression::make_integer_ul(32, int_type, loc);
          Expression* call = Runtime::make_call(gogo, Runtime::BUILTIN_CLZ,
						loc, 1, arg->copy());
          call = Expression::make_cast(int_type, call, loc);
          Expression* cond = Expression::make_conditional(cmp, c32, call, loc);
          // len = 32 - clz
          if (name == "Len32" || name == "Len")
            return Expression::make_binary(OPERATOR_MINUS, c32->copy(), cond, loc);
          return cond;
        }
      else if ((name == "LeadingZeros64" || name == "Len64"
                || ((name == "LeadingZeros" || name == "Len") && int_size == 8))
               && this->args_ != NULL && this->args_->size() == 1)
        {
          Expression* arg = this->args_->front();
          if (!arg->is_multi_eval_safe())
            {
              Temporary_statement* ts = Statement::make_temporary(uint64_type, arg, loc);
              inserter->insert(ts);
              arg = Expression::make_temporary_reference(ts, loc);
            }
          // arg == 0 ? 64 : __builtin_clzll(arg)
          Expression* zero = Expression::make_integer_ul(0, uint64_type, loc);
          Expression* cmp = Expression::make_binary(OPERATOR_EQEQ, arg, zero, loc);
          Expression* c64 = Expression::make_integer_ul(64, int_type, loc);
          Expression* call = Runtime::make_call(gogo, Runtime::BUILTIN_CLZLL,
						loc, 1, arg->copy());
          call = Expression::make_cast(int_type, call, loc);
          Expression* cond = Expression::make_conditional(cmp, c64, call, loc);
          // len = 64 - clz
          if (name == "Len64" || name == "Len")
            return Expression::make_binary(OPERATOR_MINUS, c64->copy(), cond, loc);
          return cond;
        }
      else if ((name == "OnesCount8" || name == "OnesCount16"
           || name == "OnesCount32" || name == "OnesCount64"
           || name == "OnesCount")
          && this->args_ != NULL && this->args_->size() == 1)
        {
          Runtime::Function code;
          if (name == "OnesCount64")
            code = Runtime::BUILTIN_POPCOUNTLL;
          else if (name == "OnesCount")
            code = (int_size == 8 ? Runtime::BUILTIN_POPCOUNTLL : Runtime::BUILTIN_POPCOUNT);
          else
            code = Runtime::BUILTIN_POPCOUNT;
          Expression* arg = this->args_->front();
          Expression* call = Runtime::make_call(gogo, code, loc, 1, arg);
          return Expression::make_cast(int_type, call, loc);
        }
    }
  else if (package == "runtime/internal/atomic")
    {
      int memorder = __ATOMIC_SEQ_CST;

      if (is_method)
	{
	  Function_type* ftype = (no->is_function()
				  ? no->func_value()->type()
				  : no->func_declaration_value()->type());
	  Type* rtype = ftype->receiver()->type()->deref();
	  go_assert(rtype->named_type() != NULL);
	  const std::string& rname(rtype->named_type()->name());
	  if (rname == "Int32")
	    {
	      if (name == "Load")
		name = "LoadInt32";
	      else if (name == "Store")
		name = "Storeint32";
	      else if (name == "CompareAndSwap")
		name = "Casint32";
	      else if (name == "Swap")
		name = "Xchgint32";
	      else if (name == "Add")
		name = "Xaddint32";
	      else
		go_unreachable();
	    }
	  else if (rname == "Int64")
	    {
	      if (name == "Load")
		name = "LoadInt64";
	      else if (name == "Store")
		name = "Storeint64";
	      else if (name == "CompareAndSwap")
		name = "Casint64";
	      else if (name == "Swap")
		name = "Xchgint64";
	      else if (name == "Add")
		name = "Xaddint64";
	      else
		go_unreachable();
	    }
	  else if (rname == "Uint8")
	    {
	      if (name == "Load")
		name = "Load8";
	      else if (name == "Store")
		name = "Store8";
	      else if (name == "And")
		name = "And8";
	      else if (name == "Or")
		name = "Or8";
	      else
		go_unreachable();
	    }
	  else if (rname == "Uint32")
	    {
	      if (name == "Load")
		name = "Load";
	      else if (name == "LoadAcquire")
		name = "LoadAcq";
	      else if (name == "Store")
		name = "Store";
	      else if (name == "CompareAndSwap")
		name = "Cas";
	      else if (name == "CompareAndSwapRelease")
		name = "CasRel";
	      else if (name == "Swap")
		name = "Xchg";
	      else if (name == "And")
		name = "And";
	      else if (name == "Or")
		name = "Or";
	      else if (name == "Add")
		name = "Xadd";
	      else
		go_unreachable();
	    }
	  else if (rname == "Uint64")
	    {
	      if (name == "Load")
		name = "Load64";
	      else if (name == "Store")
		name = "Store64";
	      else if (name == "CompareAndSwap")
		name = "Cas64";
	      else if (name == "Swap")
		name = "Xchgt64";
	      else if (name == "Add")
		name = "Xadd64";
	      else
		go_unreachable();
	    }
	  else if (rname == "Uintptr")
	    {
	      if (name == "Load")
		name = "Loaduintptr";
	      else if (name == "LoadAcquire")
		name = "Loadacquintptr";
	      else if (name == "Store")
		name = "Storeuintptr";
	      else if (name == "StoreRelease")
		name = "StoreReluintptr";
	      else if (name == "CompareAndSwap")
		name = "Casuintptr";
	      else if (name == "Swap")
		name = "Xchguintptr";
	      else if (name == "Add")
		name = "Xadduintptr";
	      else
		go_unreachable();
	    }
	  else if (rname == "Float64")
	    {
	      // Needs unsafe type conversion.  Don't intrinsify for now.
	      return NULL;
	    }
	  else if (rname == "UnsafePointer")
	    {
	      if (name == "Load")
		name = "Loadp";
	      else if (name == "StoreNoWB")
		name = "StorepoWB";
	      else if (name == "CompareAndSwapNoWB")
		name = "Casp1";
	      else
		go_unreachable();
	    }
	  else
	    go_unreachable();
	}

      if ((name == "Load" || name == "Load64" || name == "Loadint64" || name == "Loadp"
           || name == "Loaduint" || name == "Loaduintptr" || name == "LoadAcq"
           || name == "Loadint32" || name == "Load8")
          && this->args_ != NULL && this->args_->size() == 1)
        {
          if (int_size < 8 && (name == "Load64" || name == "Loadint64"))
            // On 32-bit architectures we need to check alignment.
            // Not intrinsify for now.
            return NULL;

          Runtime::Function code;
          Type* res_type;
          if (name == "Load")
            {
              code = Runtime::ATOMIC_LOAD_4;
              res_type = uint32_type;
            }
          else if (name == "Load64")
            {
              code = Runtime::ATOMIC_LOAD_8;
              res_type = uint64_type;
            }
          else if (name == "Loadint32")
            {
              code = Runtime::ATOMIC_LOAD_4;
              res_type = int32_type;
            }
          else if (name == "Loadint64")
            {
              code = Runtime::ATOMIC_LOAD_8;
              res_type = int64_type;
            }
          else if (name == "Loaduint")
            {
              code = (int_size == 8
                      ? Runtime::ATOMIC_LOAD_8
                      : Runtime::ATOMIC_LOAD_4);
              res_type = uint_type;
            }
          else if (name == "Loaduintptr")
            {
              code = (ptr_size == 8
                      ? Runtime::ATOMIC_LOAD_8
                      : Runtime::ATOMIC_LOAD_4);
              res_type = uintptr_type;
            }
          else if (name == "Loadp")
            {
              code = (ptr_size == 8
                      ? Runtime::ATOMIC_LOAD_8
                      : Runtime::ATOMIC_LOAD_4);
              res_type = pointer_type;
            }
          else if (name == "LoadAcq")
            {
              code = Runtime::ATOMIC_LOAD_4;
              res_type = uint32_type;
              memorder = __ATOMIC_ACQUIRE;
            }
	  else if (name == "Load8")
	    {
	      code = Runtime::ATOMIC_LOAD_1;
	      res_type = uint8_type;
	    }
          else
            go_unreachable();
          Expression* a1 = this->args_->front();
          Expression* a2 = Expression::make_integer_ul(memorder, int32_type, loc);
          Expression* call = Runtime::make_call(gogo, code, loc, 2, a1, a2);
          return Expression::make_unsafe_cast(res_type, call, loc);
        }

      if ((name == "Store" || name == "Store64" || name == "StorepNoWB"
           || name == "Storeuintptr" || name == "StoreRel"
           || name == "Storeint32" || name == "Storeint64")
          && this->args_ != NULL && this->args_->size() == 2)
        {
          if (int_size < 8 && (name == "Store64" || name == "Storeint64"))
            return NULL;

          Runtime::Function code;
          Expression* a1 = this->args_->at(0);
          Expression* a2 = this->args_->at(1);
          if (name == "Store")
            code = Runtime::ATOMIC_STORE_4;
          else if (name == "Store64")
            code = Runtime::ATOMIC_STORE_8;
          else if (name == "Storeint32")
            code = Runtime::ATOMIC_STORE_4;
          else if (name == "Storeint64")
            code = Runtime::ATOMIC_STORE_8;
          else if (name == "Storeuintptr")
            code = (ptr_size == 8 ? Runtime::ATOMIC_STORE_8 : Runtime::ATOMIC_STORE_4);
          else if (name == "StorepNoWB")
            {
              code = (ptr_size == 8 ? Runtime::ATOMIC_STORE_8 : Runtime::ATOMIC_STORE_4);
              a2 = Expression::make_unsafe_cast(uintptr_type, a2, loc);
              a2 = Expression::make_cast(uint64_type, a2, loc);
            }
          else if (name == "StoreRel")
            {
              code = Runtime::ATOMIC_STORE_4;
              memorder = __ATOMIC_RELEASE;
            }
	  else if (name == "Store8")
	    code = Runtime::ATOMIC_STORE_1;
          else
            go_unreachable();
          Expression* a3 = Expression::make_integer_ul(memorder, int32_type, loc);
          return Runtime::make_call(gogo, code, loc, 3, a1, a2, a3);
        }

      if ((name == "Xchg" || name == "Xchg64" || name == "Xchguintptr"
           || name == "Xchgint32" || name == "Xchgint64")
          && this->args_ != NULL && this->args_->size() == 2)
        {
          if (int_size < 8 && (name == "Xchg64" || name == "Xchgint64"))
            return NULL;

          Runtime::Function code;
          Type* res_type;
          if (name == "Xchg")
            {
              code = Runtime::ATOMIC_EXCHANGE_4;
              res_type = uint32_type;
            }
          else if (name == "Xchg64")
            {
              code = Runtime::ATOMIC_EXCHANGE_8;
              res_type = uint64_type;
            }
          else if (name == "Xchgint32")
            {
              code = Runtime::ATOMIC_EXCHANGE_4;
              res_type = int32_type;
            }
          else if (name == "Xchgint64")
            {
              code = Runtime::ATOMIC_EXCHANGE_8;
              res_type = int64_type;
            }
          else if (name == "Xchguintptr")
            {
              code = (ptr_size == 8
                      ? Runtime::ATOMIC_EXCHANGE_8
                      : Runtime::ATOMIC_EXCHANGE_4);
              res_type = uintptr_type;
            }
          else
            go_unreachable();
          Expression* a1 = this->args_->at(0);
          Expression* a2 = this->args_->at(1);
          Expression* a3 = Expression::make_integer_ul(memorder, int32_type, loc);
          Expression* call = Runtime::make_call(gogo, code, loc, 3, a1, a2, a3);
          return Expression::make_cast(res_type, call, loc);
        }

      if ((name == "Cas" || name == "Cas64" || name == "Casuintptr"
           || name == "Casp1" || name == "CasRel"
           || name == "Casint32" || name == "Casint64")
          && this->args_ != NULL && this->args_->size() == 3)
        {
          if (int_size < 8 && (name == "Cas64" || name == "Casint64"))
            return NULL;

          Runtime::Function code;
          Expression* a1 = this->args_->at(0);

          // Builtin cas takes a pointer to the old value.
          // Store it in a temporary and take the address.
          Expression* a2 = this->args_->at(1);
          Temporary_statement* ts = Statement::make_temporary(NULL, a2, loc);
          inserter->insert(ts);
          a2 = Expression::make_temporary_reference(ts, loc);
          a2 = Expression::make_unary(OPERATOR_AND, a2, loc);

          Expression* a3 = this->args_->at(2);
          if (name == "Cas")
            code = Runtime::ATOMIC_COMPARE_EXCHANGE_4;
          else if (name == "Cas64")
            code = Runtime::ATOMIC_COMPARE_EXCHANGE_8;
          else if (name == "Casint32")
            code = Runtime::ATOMIC_COMPARE_EXCHANGE_4;
          else if (name == "Casint64")
            code = Runtime::ATOMIC_COMPARE_EXCHANGE_8;
          else if (name == "Casuintptr")
            code = (ptr_size == 8
                    ? Runtime::ATOMIC_COMPARE_EXCHANGE_8
                    : Runtime::ATOMIC_COMPARE_EXCHANGE_4);
          else if (name == "Casp1")
            {
              code = (ptr_size == 8
                      ? Runtime::ATOMIC_COMPARE_EXCHANGE_8
                      : Runtime::ATOMIC_COMPARE_EXCHANGE_4);
              a3 = Expression::make_unsafe_cast(uintptr_type, a3, loc);
              a3 = Expression::make_cast(uint64_type, a3, loc);
            }
          else if (name == "CasRel")
            {
              code = Runtime::ATOMIC_COMPARE_EXCHANGE_4;
              memorder = __ATOMIC_RELEASE;
            }
          else
            go_unreachable();
          Expression* a4 = Expression::make_boolean(false, loc);
          Expression* a5 = Expression::make_integer_ul(memorder, int32_type, loc);
          Expression* a6 = Expression::make_integer_ul(__ATOMIC_RELAXED, int32_type, loc);
          return Runtime::make_call(gogo, code, loc, 6, a1, a2, a3, a4, a5, a6);
        }

      if ((name == "Xadd" || name == "Xadd64" || name == "Xaddint64"
           || name == "Xadduintptr" || name == "Xaddint32")
          && this->args_ != NULL && this->args_->size() == 2)
        {
          if (int_size < 8 && (name == "Xadd64" || name == "Xaddint64"))
            return NULL;

          Runtime::Function code;
          Type* res_type;
          if (name == "Xadd")
            {
              code = Runtime::ATOMIC_ADD_FETCH_4;
              res_type = uint32_type;
            }
          else if (name == "Xadd64")
            {
              code = Runtime::ATOMIC_ADD_FETCH_8;
              res_type = uint64_type;
            }
          else if (name == "Xaddint32")
            {
              code = Runtime::ATOMIC_ADD_FETCH_4;
              res_type = int32_type;
            }
          else if (name == "Xaddint64")
            {
              code = Runtime::ATOMIC_ADD_FETCH_8;
              res_type = int64_type;
            }
          else if (name == "Xadduintptr")
            {
              code = (ptr_size == 8
                      ? Runtime::ATOMIC_ADD_FETCH_8
                      : Runtime::ATOMIC_ADD_FETCH_4);
              res_type = uintptr_type;
            }
          else
            go_unreachable();
          Expression* a1 = this->args_->at(0);
          Expression* a2 = this->args_->at(1);
          Expression* a3 = Expression::make_integer_ul(memorder, int32_type, loc);
          Expression* call = Runtime::make_call(gogo, code, loc, 3, a1, a2, a3);
          return Expression::make_cast(res_type, call, loc);
        }

      if ((name == "And8" || name == "Or8")
          && this->args_ != NULL && this->args_->size() == 2)
        {
          Runtime::Function code;
          if (name == "And8")
            code = Runtime::ATOMIC_AND_FETCH_1;
          else if (name == "Or8")
            code = Runtime::ATOMIC_OR_FETCH_1;
          else
            go_unreachable();
          Expression* a1 = this->args_->at(0);
          Expression* a2 = this->args_->at(1);
          Expression* a3 = Expression::make_integer_ul(memorder, int32_type, loc);
          return Runtime::make_call(gogo, code, loc, 3, a1, a2, a3);
        }
    }
  else if (package == "internal/abi"
	   || package == "bootstrap/internal/abi") // for bootstrapping gc
    {
      if (is_method)
	return NULL;

      if ((name == "FuncPCABI0" || name == "FuncPCABIInternal")
	  && this->args_ != NULL
	  && this->args_->size() == 1)
	{
	  // We expect to see a conversion from the expression to "any".
	  Expression* expr = this->args_->front();
	  Type_conversion_expression* tce = expr->conversion_expression();
	  if (tce != NULL)
	    expr = tce->expr();
	  Func_expression* fe = expr->func_expression();
	  Interface_field_reference_expression* interface_method =
	    expr->interface_field_reference_expression();
	  if (fe != NULL)
	    {
	      Named_object* no = fe->named_object();
	      Expression* ref = Expression::make_func_code_reference(no, loc);
	      Type* uintptr_type = Type::lookup_integer_type("uintptr");
	      return Expression::make_cast(uintptr_type, ref, loc);
	    }
	  else if (interface_method != NULL)
	    return interface_method->get_function();
	  else
	    {
	      expr = this->args_->front();
	      go_assert(expr->type()->interface_type() != NULL
			&& expr->type()->interface_type()->is_empty());
	      expr = Expression::make_interface_info(expr,
						     INTERFACE_INFO_OBJECT,
						     loc);
	      // Trust that this is a function type, which means that
	      // it is a direct iface type and we can use EXPR
	      // directly.  The backend representation of this
	      // function is a pointer to a struct whose first field
	      // is the actual function to call.
	      Type* pvoid = Type::make_pointer_type(Type::make_void_type());
	      Type* pfntype = Type::make_pointer_type(pvoid);
	      Expression* ref = make_unsafe_cast(pfntype, expr, loc);
	      return Expression::make_dereference(ref, NIL_CHECK_NOT_NEEDED,
						  loc);
	    }
	}
    }

  return NULL;
}

// Make implicit type conversions explicit.

void
Call_expression::do_add_conversions()
{
  // Skip call that requires a thunk. We generate conversions inside the thunk.
  if (this->is_concurrent_ || this->is_deferred_)
    return;

  if (this->args_ == NULL || this->args_->empty())
    return;

  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    {
      go_assert(saw_errors());
      return;
    }
  if (fntype->parameters() == NULL || fntype->parameters()->empty())
    return;

  Location loc = this->location();
  Expression_list::iterator pa = this->args_->begin();
  Typed_identifier_list::const_iterator pp = fntype->parameters()->begin();
  bool is_interface_method =
    this->fn_->interface_field_reference_expression() != NULL;
  size_t argcount = this->args_->size();
  if (!is_interface_method && fntype->is_method())
    {
      // Skip the receiver argument, which cannot be interface.
      pa++;
      argcount--;
    }
  if (argcount != fntype->parameters()->size())
    {
      go_assert(saw_errors());
      return;
    }
  for (; pa != this->args_->end(); ++pa, ++pp)
    {
      Type* pt = pp->type();
      if (!Type::are_identical(pt, (*pa)->type(), 0, NULL)
          && pt->interface_type() != NULL)
        *pa = Expression::make_cast(pt, *pa, loc);
    }
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

// Return the temporary that holds the result for a call with multiple
// results.

Temporary_statement*
Call_expression::results() const
{
  if (this->call_temp_ == NULL)
    {
      go_assert(saw_errors());
      return NULL;
    }
  return this->call_temp_;
}

// Set the number of results expected from a call expression.

void
Call_expression::set_expected_result_count(size_t count)
{
  go_assert(this->expected_result_count_ == 0);
  go_assert(!this->types_are_determined_);
  this->expected_result_count_ = count;
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

// Whether or not this call contains errors, either in the call or the
// arguments to the call.

bool
Call_expression::is_erroneous_call()
{
  if (this->is_error_expression() || this->fn()->is_error_expression())
    return true;

  if (this->args() == NULL)
    return false;
  for (Expression_list::iterator pa = this->args()->begin();
       pa != this->args()->end();
       ++pa)
    {
      if ((*pa)->type()->is_error_type() || (*pa)->is_error_expression())
        return true;
    }
  return false;
}

// Get the type.

Type*
Call_expression::do_type()
{
  if (this->is_error_expression())
    return Type::make_error_type();
  if (this->lowered_ != NULL)
    return this->lowered_->type();
  go_assert(this->type_ != NULL);
  return this->type_;
}

// Determine types for a call expression.  We can use the function
// parameter types to set the types of the arguments.  We simplify
// some of the call cases here, storing the result in the lowered_
// field.

void
Call_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  if (!this->determining_types())
    return;

  if (this->lowered_== NULL)
    {
      Expression* builtin = this->lower_builtin(gogo);
      if (builtin != this)
	this->lowered_ = builtin;
    }

  if (this->lowered_ != NULL)
    {
      this->lowered_->determine_type(gogo, context);
      return;
    }

  this->fn_->determine_type_no_context(gogo);

  // Simplify a type conversion.

  if (this->fn_->is_type_expression()
      && this->args_ != NULL
      && this->args_->size() == 1
      && (this->expected_result_count_ == 0
	  || this->expected_result_count_ == 1))
    {
      this->lowered_ = Expression::make_cast(this->fn_->type(),
					     this->args_->front(),
					     this->location());
      this->lowered_->determine_type(gogo, context);
      return;
    }

  // Get the type of the function we are calling.

  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    {
      // We report the error here so that we can reasonably return an
      // error type in do_type.
      if (!this->fn_->type()->is_error())
	this->report_error(_("expected function"));
      else
	this->set_is_error();
      if (this->args_ != NULL)
	{
	  for (Expression_list::iterator p = this->args_->begin();
	       p != this->args_->end();
	       ++p)
	    (*p)->determine_type_no_context(gogo);
	}
      return;
    }

  // Simplify f(g()) where g() returns multiple results.

  this->simplify_multiple_results(gogo);

  // Set the type of this expression.

  go_assert(this->type_ == NULL);
  const Typed_identifier_list* results = fntype->results();
  if (results == NULL || results->empty())
    this->type_ = Type::make_void_type();
  else if (results->size() == 1)
    {
      // If this is a call to a generated equality function, we
      // determine the type based on the context.  See the comment in
      // Binary_expression::lower_array_comparison.
      if (this->is_equal_function_
	  && !context->may_be_abstract
	  && context->type != NULL
	  && context->type->is_boolean_type())
	this->type_ = context->type;
      else
	this->type_ = results->begin()->type();
    }
  else
    this->type_ = Type::make_call_multiple_result_type();

  // Determine the types of the arguments.

  if (this->args_ == NULL)
    {
      if (fntype->is_varargs())
	{
	  if (!this->rewrite_varargs())
	    this->set_is_error();
	}
      return;
    }

  const Typed_identifier_list* parameters = fntype->parameters();
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
	      (*pa)->determine_type(gogo, &subcontext);
	      continue;
	    }
	}

      if ((this->is_varargs_ || this->varargs_are_lowered_)
	  && fntype->is_varargs()
	  && pa + 1 == this->args_->end()
	  && parameters != NULL
	  && pt + 1 == parameters->end())
	{
	  Type_context subcontext(pt->type(), false);
	  (*pa)->determine_type(gogo, &subcontext);
	  continue;
	}

      if (!this->is_varargs_
	  && fntype->is_varargs()
	  && parameters != NULL
	  && pt + 1 == parameters->end())
	{
	  go_assert(pt->type()->is_slice_type());
	  Type_context subcontext(pt->type()->array_type()->element_type(),
				  false);
	  (*pa)->determine_type(gogo, &subcontext);
	  continue;
	}

      if (parameters != NULL && pt != parameters->end())
	{
	  Type_context subcontext(pt->type(), false);
	  (*pa)->determine_type(gogo, &subcontext);
	  if (!fntype->is_varargs() || pt + 1 != parameters->end())
	    ++pt;
	}
      else
	(*pa)->determine_type_no_context(gogo);
    }

  if (fntype->is_varargs())
    {
      if (!this->rewrite_varargs())
	this->set_is_error();
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

// Simplify f(g()) where g() returns multiple results.  Called by
// do_determine_types of both Call_expression and
// Builtin_call_expression.

void
Call_expression::simplify_multiple_results(Gogo* gogo)
{
  if (this->args_ == NULL || this->args_->size() != 1)
    return;

  Call_expression* call = this->args_->front()->call_expression();
  if (call == NULL || call->is_builtin())
    return;

  call->determine_type_no_context(gogo);
  size_t rc = call->result_count();
  Function_type* fntype = this->get_function_type();
  if (rc > 1
      && ((fntype->parameters() != NULL
	   && (fntype->parameters()->size() == rc
	       || (fntype->is_varargs()
		   && fntype->parameters()->size() - 1 <= rc)))
	  || fntype->is_builtin()))
    {
      if (this->is_varargs_)
	{
	  go_error_at(call->location(),
		      "multiple-value argument in single-value context");
	  this->set_is_error();
	}

      call->set_is_multi_value_arg();
      Expression_list* args = new Expression_list;
      for (size_t i = 0; i < rc; ++i)
	args->push_back(Expression::make_call_result(call, i));
      // We can't create a new Call_expression here because this
      // one may be referred to by Call_result expressions.
      this->args_ = args;
    }
}

// Lower a call to a varargs function by rewriting the value(s) passed
// to the varargs argument into a slice.  Called during the
// determine_types pass.

bool
Call_expression::rewrite_varargs()
{
  if (this->varargs_are_lowered_)
    return true;
  this->varargs_are_lowered_ = true;

  Function_type* fntype = this->get_function_type();

  const Typed_identifier_list* parameters = fntype->parameters();
  go_assert(parameters != NULL && !parameters->empty());
  size_t param_count = parameters->size();

  Type* varargs_type = parameters->back().type();
  go_assert(varargs_type->is_slice_type());

  size_t arg_count = this->args_ == NULL ? 0 : this->args_->size();
  if (arg_count < param_count - 1)
    {
      if (!this->is_error_expression())
	this->report_error(_("not enough arguments"));
      return false;
    }

  bool ret = true;
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
      size_t i = 1;
      for (pa = old_args->begin(); pa != old_args->end(); ++pa, ++i)
	{
	  if (i == param_count)
	    break;
	  new_args->push_back(*pa);
	}

      // We have reached the varargs parameter.

      if (pa == old_args->end())
	push_empty_arg = true;
      else if (pa + 1 == old_args->end() && this->is_varargs_)
	new_args->push_back(*pa);
      else if (this->is_varargs_)
	{
	  if (!this->is_error_expression())
	    {
	      if ((*pa)->type()->is_slice_type())
		this->report_error(_("too many arguments"));
	      else
		{
		  go_error_at(this->location(),
			      "invalid use of %<...%> with non-slice");
		  this->set_is_error();
		}
	    }
	  return false;
	}
      else
	{
	  Type* element_type = varargs_type->array_type()->element_type();
	  Expression_list* vals = new Expression_list;
	  for (; pa != old_args->end(); ++pa, ++i)
	    {
	      Type* patype = (*pa)->type();
	      Location paloc = (*pa)->location();
	      if (!this->check_argument_type(i, element_type, patype, paloc))
		{
		  ret = false;
		  continue;
		}
	      vals->push_back(*pa);
	    }
	  Expression* val =
	    Expression::make_slice_composite_literal(varargs_type, vals,
						     this->location());
	  new_args->push_back(val);
	}
    }

  if (push_empty_arg)
    new_args->push_back(Expression::make_nil(this->location()));

  // We can't create a new Call_expression here because this
  // one may be referred to by Call_result expressions.
  this->args_ = new_args;

  return ret;
}

// Check types for parameter I.

bool
Call_expression::check_argument_type(int i, const Type* parameter_type,
				     const Type* argument_type,
				     Location argument_location)
{
  std::string reason;
  if (!Type::are_assignable(parameter_type, argument_type, &reason))
    {
      if (reason.empty())
	go_error_at(argument_location, "argument %d has incompatible type", i);
      else
	go_error_at(argument_location,
		    "argument %d has incompatible type (%s)",
		    i, reason.c_str());
      this->set_is_error();
      return false;
    }
  return true;
}

// Check types.

void
Call_expression::do_check_types(Gogo*)
{
  if (this->is_error_expression()
      || this->fn_->is_error_expression()
      || this->fn_->type()->is_error())
    return;
  if (this->lowered_ != NULL)
    return;

  Function_type* fntype = this->get_function_type();
  go_assert(fntype != NULL);

  if (this->expected_result_count_ != 0
      && this->expected_result_count_ != this->result_count())
    {
      if (this->issue_error())
	this->report_error(_("function result count mismatch"));
      this->set_is_error();
      return;
    }

  if (this->is_varargs_ && !fntype->is_varargs())
    {
      go_error_at(this->location(),
		  "invalid use of %<...%> calling non-variadic function");
      this->set_is_error();
      return;
    }

  bool is_method = fntype->is_method();
  if (is_method)
    {
      go_assert(this->args_ != NULL && !this->args_->empty());
      Type* rtype = fntype->receiver()->type();
      Expression* first_arg = this->args_->front();
      // We dereference the values since receivers are always passed
      // as pointers.
      std::string reason;
      if (!Type::are_assignable(rtype->deref(), first_arg->type()->deref(),
				&reason))
	{
	  if (reason.empty())
	    this->report_error(_("incompatible type for receiver"));
	  else
	    {
	      go_error_at(this->location(),
                          "incompatible type for receiver (%s)",
                          reason.c_str());
	      this->set_is_error();
	    }
	}
    }

  const Typed_identifier_list* parameters = fntype->parameters();
  if (this->args_ == NULL || this->args_->empty())
    {
      if (parameters != NULL && !parameters->empty())
	this->report_error(_("not enough arguments"));
    }
  else if (parameters == NULL)
    {
      if (!is_method || this->args_->size() > 1)
	this->report_error(_("too many arguments"));
    }
  else if (this->args_->size() == 1
	   && this->args_->front()->call_expression() != NULL
	   && this->args_->front()->call_expression()->result_count() > 1)
    {
      // This is F(G()) when G returns more than one result.  If the
      // results can be matched to parameters, it would have been
      // rewritten in determine_types.  If we get here we know there
      // is a mismatch.
      if (this->args_->front()->call_expression()->result_count()
	  < parameters->size())
	this->report_error(_("not enough arguments"));
      else
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
				    (*pa)->location());
	}
      if (pa != this->args_->end())
	this->report_error(_("too many arguments"));
    }
}

Expression*
Call_expression::do_copy()
{
  Call_expression* call =
    Expression::make_call(this->fn_->copy(),
			  (this->args_ == NULL
			   ? NULL
			   : this->args_->copy()),
			  this->is_varargs_, this->location());

  if (this->varargs_are_lowered_)
    call->set_varargs_are_lowered();
  if (this->is_deferred_)
    call->set_is_deferred();
  if (this->is_concurrent_)
    call->set_is_concurrent();
  return call;
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
    Expression** first_arg_ptr,
    Location location)
{
  Expression* object = interface_method->get_underlying_object();
  Type* unsafe_ptr_type = Type::make_pointer_type(Type::make_void_type());
  *first_arg_ptr =
      Expression::make_unsafe_cast(unsafe_ptr_type, object, location);
  return interface_method->get_function();
}

// Build the call expression.

Bexpression*
Call_expression::do_get_backend(Translate_context* context)
{
  Location location = this->location();

  if (this->call_ != NULL)
    {
      // If the call returns multiple results, make a new reference to
      // the temporary.
      if (this->call_temp_ != NULL)
	{
	  Expression* ref =
	    Expression::make_temporary_reference(this->call_temp_, location);
	  return ref->get_backend(context);
	}

      return this->call_;
    }

  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    return context->backend()->error_expression();

  if (this->fn_->is_error_expression())
    return context->backend()->error_expression();

  Gogo* gogo = context->gogo();

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

  Expression* first_arg = NULL;
  if (!is_interface_method && fntype->is_method())
    {
      first_arg = this->args_->front();
      if (first_arg->type()->points_to() == NULL
          && first_arg->type()->is_direct_iface_type())
        first_arg = Expression::unpack_direct_iface(first_arg,
                                                    first_arg->location());
    }

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
      fn_args[0] = first_arg->get_backend(context);
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
          fn_args[i] = first_arg->get_backend(context);
	  ++pe;
	  ++i;
	}
      for (; pe != this->args_->end(); ++pe, ++pp, ++i)
	{
	  go_assert(pp != params->end());
          Expression* arg =
              Expression::convert_for_assignment(gogo, pp->type(), *pe,
                                                 location);
          fn_args[i] = arg->get_backend(context);
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
      fn = Expression::make_dereference(fn, NIL_CHECK_NOT_NEEDED, location);
    }
  else
    {
      Expression* arg0;
      fn = this->interface_method_function(interface_method, &arg0,
                                           location);
      fn_args[0] = arg0->get_backend(context);
    }

  Bexpression* bclosure = NULL;
  if (has_closure_arg)
    bclosure = closure->get_backend(context);
  else
    go_assert(closure == NULL);

  Bexpression* bfn = fn->get_backend(context);

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

  Bfunction* bfunction = NULL;
  if (context->function())
    bfunction = context->function()->func_value()->get_decl();
  Bexpression* call = gogo->backend()->call_expression(bfunction, bfn,
                                                       fn_args, bclosure,
                                                       location);

  if (this->call_temp_ != NULL)
    {
      // This case occurs when the call returns multiple results.

      Expression* ref = Expression::make_temporary_reference(this->call_temp_,
							     location);
      Bexpression* bref = ref->get_backend(context);
      Bstatement* bassn = gogo->backend()->assignment_statement(bfunction,
								bref, call,
								location);

      ref = Expression::make_temporary_reference(this->call_temp_, location);
      this->call_ = ref->get_backend(context);

      return gogo->backend()->compound_expression(bassn, this->call_,
						  location);
    }

  this->call_ = call;
  return this->call_;
}

// The cost of inlining a call expression.

int
Call_expression::do_inlining_cost() const
{
  Func_expression* fn = this->fn_->func_expression();

  // FIXME: We don't yet support all kinds of calls.
  if (fn != NULL && fn->closure() != NULL)
    return 0x100000;
  if (this->fn_->interface_field_reference_expression())
    return 0x100000;
  if (this->get_function_type()->is_method())
    return 0x100000;

  return 5;
}

// Export a call expression.

void
Call_expression::do_export(Export_function_body* efb) const
{
  bool simple_call = (this->fn_->func_expression() != NULL);
  if (!simple_call)
    efb->write_c_string("(");
  this->fn_->export_expression(efb);
  if (!simple_call)
    efb->write_c_string(")");
  this->export_arguments(efb);
}

// Export call expression arguments.

void
Call_expression::export_arguments(Export_function_body* efb) const
{
  efb->write_c_string("(");
  if (this->args_ != NULL && !this->args_->empty())
    {
      Expression_list::const_iterator pa = this->args_->begin();
      (*pa)->export_expression(efb);
      for (pa++; pa != this->args_->end(); pa++)
	{
	  efb->write_c_string(", ");
	  (*pa)->export_expression(efb);
	}
      if (this->is_varargs_)
	efb->write_c_string("...");
    }
  efb->write_c_string(")");
}

// Dump ast representation for a call expression.

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

// Class Call_result_expression.

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
Call_result_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->call_->determine_type_no_context(gogo);
}

// Return the backend representation.  We just refer to the temporary set by the
// call expression.  We don't do this at lowering time because it makes it
// hard to evaluate the call at the right time.

Bexpression*
Call_result_expression::do_get_backend(Translate_context* context)
{
  Call_expression* ce = this->call_->call_expression();
  if (ce == NULL)
    {
      go_assert(this->call_->is_error_expression());
      return context->backend()->error_expression();
    }
  Temporary_statement* ts = ce->results();
  if (ts == NULL)
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }
  Expression* ref = Expression::make_temporary_reference(ts, this->location());
  ref = Expression::make_field_reference(ref, this->index_, this->location());
  return ref->get_backend(context);
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

// Report whether EXPR is a map index expression.  This is called when
// types are determined but before lowering.

bool
Index_expression::is_map_index(Expression* expr)
{
  if (expr->map_index_expression() != NULL)
    return true;
  if (expr->index_expression() != NULL)
    return expr->index_expression()->left_->type()->map_type() != NULL;
  return false;
}

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

void
Index_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->left_->determine_type_no_context(gogo);

  Type_context end_context;
  Type* type = this->left_->type();
  if (type->array_type() != NULL
      || (type->points_to() != NULL
	  && type->points_to()->array_type() != NULL
	  && !type->points_to()->is_slice_type())
      || type->is_string_type())
    {
      Type_context index_context(Type::lookup_integer_type("int"), false);
      this->start_->determine_type(gogo, &index_context);
      end_context = index_context;
    }
  else if (type->map_type() != NULL)
    {
      Type_context key_context(type->map_type()->key_type(), false);
      this->start_->determine_type(gogo, &key_context);
    }
  else if (type->is_error())
    this->set_is_error();
  else
    {
      if (this->cap_ != NULL)
	this->report_error(_("invalid 3-index slice of object "
			     "that is not a slice"));
      else if (this->end_ != NULL)
	this->report_error(_("attempt to slice object that is not "
			     "array, slice, or string"));
      else
	this->report_error(_("attempt to index object that is not "
			     "array, slice, string, or map"));

      this->start_->determine_type_no_context(gogo);
    }

  if (this->end_ != NULL)
    this->end_->determine_type(gogo, &end_context);
  if (this->cap_ != NULL)
    this->cap_->determine_type(gogo, &end_context);
}

Type*
Index_expression::do_type()
{
  if (this->is_error_expression())
    return Type::make_error_type();

  Type* type = this->left_->type();
  if (this->end_ != NULL)
    {
      // A slice of an array is a slice type.  A slice of a slice or
      // string type is that same type.
      Array_type* at = type->deref()->array_type();
      if (at != NULL && !at->is_slice_type())
	return Type::make_array_type(at->element_type(), NULL);
      return type;
    }
  if (type->deref()->array_type() != NULL)
    return type->deref()->array_type()->element_type();
  else if (type->is_string_type())
    return Type::lookup_integer_type("byte");
  else if (type->map_type() != NULL)
    return type->map_type()->val_type();
  else
    return Type::make_error_type();
}

void
Index_expression::do_check_types(Gogo*)
{
  if (this->is_error_expression())
    return;

  Type* ltype = this->left_->type();
  if (ltype->is_error())
    {
      go_assert(saw_errors());
      return;
    }

  if (this->left_->is_type_expression())
    {
      this->report_error(_("attempt to index type expression"));
      return;
    }

  if (ltype->array_type() != NULL)
    {
      if (!Array_index_expression::check_indexes(this->left_, this->start_,
						 this->end_, this->cap_,
						 this->location()))
	this->set_is_error();
    }
  else if (ltype->points_to() != NULL
	   && ltype->points_to()->array_type() != NULL
	   && !ltype->points_to()->is_slice_type())
    {
      Expression* deref = Expression::make_dereference(this->left_,
						       NIL_CHECK_DEFAULT,
						       this->location());
      if (!Array_index_expression::check_indexes(deref, this->start_,
						 this->end_, this->cap_,
						 this->location()))
	this->set_is_error();
      delete deref;
    }
  else if (ltype->is_string_type())
    {
      if (this->cap_ != NULL)
	this->report_error(_("invalid 3-index slice of string"));
      else
	{
	  if (!String_index_expression::check_indexes(this->left_,
						      this->start_,
						      this->end_,
						      this->location()))
	    this->set_is_error();
	}
    }
  else if (ltype->map_type() != NULL)
    {
      if (this->end_ != NULL || this->cap_ != NULL)
	this->report_error(_("invalid slice of map"));
      else
	{
	  Map_type* mt = ltype->map_type();
	  std::string reason;
	  if (!Type::are_assignable(mt->key_type(), this->start_->type(),
				    &reason))
	    {
	      if (reason.empty())
		this->report_error(_("incompatible type for map index"));
	      else
		{
		  go_error_at(this->location(),
			      "incompatible type for map index (%s)",
			      reason.c_str());
		  this->set_is_error();
		}
	    }
	}
    }
  else
    go_unreachable();
}

bool
Index_expression::do_is_addressable() const
{
  if (this->is_error_expression())
    return true;

  Type* type = this->left_->type();
  if (type->is_error())
    return true;

  // A slice index is addressable, and an index of an addressable
  // array is addressable.

  bool is_pointer = false;
  if (type->points_to() != NULL
      && type->points_to()->array_type() != NULL
      && !type->points_to()->is_slice_type())
    {
      type = type->points_to();
      is_pointer = true;
    }

  if (type->array_type() == NULL)
    return false;

  if (this->end_ != NULL)
    return false;
  if (type->is_slice_type())
    return true;
  return is_pointer || this->left_->is_addressable();
}

// We need to do a nil check of any pointer dereference.

void
Index_expression::do_issue_nil_check()
{
  this->left_->issue_nil_check();
  this->needs_nil_check_ = true;
}

// Lower an index expression.  This converts the generic index
// expression into an array index, a string index, or a map index.

Expression*
Index_expression::do_lower(Gogo*, Named_object*, Statement_inserter*)
{
  if (this->is_error_expression())
    return Expression::make_error(this->location());

  Location location = this->location();
  Expression* left = this->left_;
  Expression* start = this->start_;
  Expression* end = this->end_;
  Expression* cap = this->cap_;

  Type* type = left->type();
  if (type->is_error())
    {
      go_assert(saw_errors());
      return Expression::make_error(location);
    }
  else if (type->array_type() != NULL)
    return Expression::make_array_index(left, start, end, cap, location);
  else if (type->points_to() != NULL
	   && type->points_to()->array_type() != NULL
	   && !type->points_to()->is_slice_type())
    {
      Expression* deref =
          Expression::make_dereference(left, NIL_CHECK_DEFAULT, location);

      // For an ordinary index into the array, the pointer will be
      // dereferenced.  For a slice it will not--the resulting slice
      // will simply reuse the pointer, which is incorrect if that
      // pointer is nil.  We may also be in a context that requires a
      // nil check.
      if (end != NULL || cap != NULL || this->needs_nil_check_)
	deref->issue_nil_check();

      return Expression::make_array_index(deref, start, end, cap, location);
    }
  else if (type->is_string_type())
    {
      go_assert(cap == NULL);
      return Expression::make_string_index(left, start, end, location);
    }
  else if (type->map_type() != NULL)
    {
      go_assert(end == NULL && cap == NULL);
      return Expression::make_map_index(left, start, location);
    }
  else
    go_unreachable();
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

// Class Array_index_expression.

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
Array_index_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->array_->determine_type_no_context(gogo);

  Type_context index_context(Type::lookup_integer_type("int"), false);
  this->start_->determine_type(gogo, &index_context);
  if (this->end_ != NULL)
    this->end_->determine_type(gogo, &index_context);
  if (this->cap_ != NULL)
    this->cap_->determine_type(gogo, &index_context);
}

// Check types of an array index.

void
Array_index_expression::do_check_types(Gogo*)
{
  if (!Array_index_expression::check_indexes(this->array_, this->start_,
					     this->end_, this->cap_,
					     this->location()))
    this->set_is_error();
}

// A static function to check array index types.  This is also called
// by Index_expression::do_check_types.  It reports whether type
// checking succeeded.

bool
Array_index_expression::check_indexes(Expression* array, Expression* start,
				      Expression* end, Expression* cap,
				      Location loc)
{
  bool ret = true;

  Numeric_constant nc;
  unsigned long v;
  if (start->type()->integer_type() == NULL
      && !start->type()->is_error()
      && (!start->type()->is_abstract()
	  || !start->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    {
      go_error_at(loc, "index must be integer");
      ret = false;
    }

  if (end != NULL
      && end->type()->integer_type() == NULL
      && !end->type()->is_error()
      && !end->is_nil_expression()
      && !end->is_error_expression()
      && (!end->type()->is_abstract()
	  || !end->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    {
      go_error_at(loc, "slice end must be integer");
      ret = false;
    }

  if (cap != NULL
      && cap->type()->integer_type() == NULL
      && !cap->type()->is_error()
      && !cap->is_nil_expression()
      && !cap->is_error_expression()
      && (!cap->type()->is_abstract()
	  || !cap->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    {
      go_error_at(loc, "slice capacity must be integer");
      ret = false;
    }

  Array_type* array_type = array->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(array->type()->is_error());
      return false;
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
  if (start->numeric_constant_value(&inc) && inc.to_int(&ival))
    {
      ival_valid = true;
      if (mpz_sgn(ival) < 0
	  || mpz_sizeinbase(ival, 2) >= int_bits
	  || (lval_valid
	      && (end == NULL
		  ? mpz_cmp(ival, lval) >= 0
		  : mpz_cmp(ival, lval) > 0)))
	{
	  go_error_at(start->location(), "array index out of bounds");
	  ret = false;
	}
    }

  if (end != NULL && !end->is_nil_expression())
    {
      Numeric_constant enc;
      mpz_t eval;
      bool eval_valid = false;
      if (end->numeric_constant_value(&enc) && enc.to_int(&eval))
	{
	  eval_valid = true;
	  if (mpz_sgn(eval) < 0
	      || mpz_sizeinbase(eval, 2) >= int_bits
	      || (lval_valid && mpz_cmp(eval, lval) > 0))
	    {
	      go_error_at(end->location(), "array index out of bounds");
	      ret = false;
	    }
	  else if (ival_valid && mpz_cmp(ival, eval) > 0)
	    {
	      go_error_at(loc, "inverted slice range");
	      ret = false;
	    }
	}

      Numeric_constant cnc;
      mpz_t cval;
      if (cap != NULL
          && cap->numeric_constant_value(&cnc) && cnc.to_int(&cval))
        {
          if (mpz_sgn(cval) < 0
              || mpz_sizeinbase(cval, 2) >= int_bits
              || (lval_valid && mpz_cmp(cval, lval) > 0))
            {
              go_error_at(cap->location(), "array index out of bounds");
	      ret = false;
            }
	  else if (ival_valid && mpz_cmp(ival, cval) > 0)
	    {
	      go_error_at(cap->location(),
                          "invalid slice index: capacity less than start");
	      ret = false;
	    }
          else if (eval_valid && mpz_cmp(eval, cval) > 0)
            {
              go_error_at(cap->location(),
                          "invalid slice index: capacity less than length");
	      ret = false;
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
  if (end != NULL && !array_type->is_slice_type())
    {
      if (!array->is_addressable())
	{
	  go_error_at(loc, "slice of unaddressable value");
	  ret = false;
	}
      else
	{
	  // Set the array address taken but not escape. The escape
	  // analysis will make it escape to heap when needed.
	  array->address_taken(false);
	}
    }

  return ret;
}

// The subexpressions of an array index must be evaluated in order.
// If this is indexing into an array, rather than a slice, then only
// the index should be evaluated.  Since this is called for values on
// the left hand side of an assigment, evaluating the array, meaning
// copying the array, will cause a different array to be modified.

bool
Array_index_expression::do_must_eval_subexpressions_in_order(
    int* skip) const
{
  *skip = this->array_->type()->is_slice_type() ? 0 : 1;
  return true;
}

// Flatten array indexing: add temporary variables and bounds checks.

Expression*
Array_index_expression::do_flatten(Gogo* gogo, Named_object*,
                                   Statement_inserter* inserter)
{
  if (this->is_flattened_)
    return this;
  this->is_flattened_ = true;

  Location loc = this->location();

  if (this->is_error_expression())
    return Expression::make_error(loc);

  Expression* array = this->array_;
  Expression* start = this->start_;
  Expression* end = this->end_;
  Expression* cap = this->cap_;
  if (array->is_error_expression()
      || array->type()->is_error_type()
      || start->is_error_expression()
      || start->type()->is_error_type()
      || (end != NULL
          && (end->is_error_expression() || end->type()->is_error_type()))
      || (cap != NULL
          && (cap->is_error_expression() || cap->type()->is_error_type())))
    {
      go_assert(saw_errors());
      return Expression::make_error(loc);
    }

  Array_type* array_type = this->array_->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(saw_errors());
      return Expression::make_error(loc);
    }

  Temporary_statement* temp;
  if (array_type->is_slice_type() && !array->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, array, loc);
      inserter->insert(temp);
      this->array_ = Expression::make_temporary_reference(temp, loc);
      array = this->array_;
    }
  if (!start->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, start, loc);
      inserter->insert(temp);
      this->start_ = Expression::make_temporary_reference(temp, loc);
      start = this->start_;
    }
  if (end != NULL
      && !end->is_nil_expression()
      && !end->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, end, loc);
      inserter->insert(temp);
      this->end_ = Expression::make_temporary_reference(temp, loc);
      end = this->end_;
    }
  if (cap != NULL && !cap->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, cap, loc);
      inserter->insert(temp);
      this->cap_ = Expression::make_temporary_reference(temp, loc);
      cap = this->cap_;
    }

  if (!this->needs_bounds_check_)
    return this;

  Expression* len;
  if (!array_type->is_slice_type())
    {
      len = array_type->get_length(gogo, this->array_);
      go_assert(len->is_constant());
    }
  else
    {
      len = array_type->get_length(gogo, this->array_->copy());
      temp = Statement::make_temporary(NULL, len, loc);
      temp->determine_types(gogo);
      inserter->insert(temp);
      len = Expression::make_temporary_reference(temp, loc);
    }

  Expression* scap = NULL;
  if (array_type->is_slice_type())
    {
      scap = array_type->get_capacity(gogo, this->array_->copy());
      temp = Statement::make_temporary(NULL, scap, loc);
      temp->determine_types(gogo);
      inserter->insert(temp);
      scap = Expression::make_temporary_reference(temp, loc);
    }

  // The order of bounds checks here matches the order used by the gc
  // compiler, as tested by issue30116[u].go.

  if (cap != NULL)
    {
      if (array_type->is_slice_type())
	Expression::check_bounds(gogo, cap, OPERATOR_LE, scap,
				 Runtime::PANIC_SLICE3_ACAP,
				 Runtime::PANIC_SLICE3_ACAP_U,
				 Runtime::PANIC_EXTEND_SLICE3_ACAP,
				 Runtime::PANIC_EXTEND_SLICE3_ACAP_U,
				 inserter, loc);
      else
	Expression::check_bounds(gogo, cap, OPERATOR_LE, len,
				 Runtime::PANIC_SLICE3_ALEN,
				 Runtime::PANIC_SLICE3_ALEN_U,
				 Runtime::PANIC_EXTEND_SLICE3_ALEN,
				 Runtime::PANIC_EXTEND_SLICE3_ALEN_U,
				 inserter, loc);

      Expression* start_bound = cap;
      if (end != NULL && !end->is_nil_expression())
	{
	  Expression::check_bounds(gogo, end, OPERATOR_LE, cap,
				   Runtime::PANIC_SLICE3_B,
				   Runtime::PANIC_SLICE3_B_U,
				   Runtime::PANIC_EXTEND_SLICE3_B,
				   Runtime::PANIC_EXTEND_SLICE3_B_U,
				   inserter, loc);
	  start_bound = end;
	}

      Expression::check_bounds(gogo, start, OPERATOR_LE, start_bound,
			       Runtime::PANIC_SLICE3_C,
			       Runtime::PANIC_SLICE3_C_U,
			       Runtime::PANIC_EXTEND_SLICE3_C,
			       Runtime::PANIC_EXTEND_SLICE3_C_U,
			       inserter, loc);
    }
  else if (end != NULL && !end->is_nil_expression())
    {
      if (array_type->is_slice_type())
	Expression::check_bounds(gogo, end, OPERATOR_LE, scap,
				 Runtime::PANIC_SLICE_ACAP,
				 Runtime::PANIC_SLICE_ACAP_U,
				 Runtime::PANIC_EXTEND_SLICE_ACAP,
				 Runtime::PANIC_EXTEND_SLICE_ACAP_U,
				 inserter, loc);
      else
	Expression::check_bounds(gogo, end, OPERATOR_LE, len,
				 Runtime::PANIC_SLICE_ALEN,
				 Runtime::PANIC_SLICE_ALEN_U,
				 Runtime::PANIC_EXTEND_SLICE_ALEN,
				 Runtime::PANIC_EXTEND_SLICE_ALEN_U,
				 inserter, loc);

      Expression::check_bounds(gogo, start, OPERATOR_LE, end,
			       Runtime::PANIC_SLICE_B,
			       Runtime::PANIC_SLICE_B_U,
			       Runtime::PANIC_EXTEND_SLICE_B,
			       Runtime::PANIC_EXTEND_SLICE_B_U,
			       inserter, loc);
    }
  else if (end != NULL)
    {
      Expression* start_bound;
      if (array_type->is_slice_type())
	start_bound = scap;
      else
	start_bound = len;
      Expression::check_bounds(gogo, start, OPERATOR_LE, start_bound,
			       Runtime::PANIC_SLICE_B,
			       Runtime::PANIC_SLICE_B_U,
			       Runtime::PANIC_EXTEND_SLICE_B,
			       Runtime::PANIC_EXTEND_SLICE_B_U,
			       inserter, loc);
    }
  else
    Expression::check_bounds(gogo, start, OPERATOR_LT, len,
			     Runtime::PANIC_INDEX,
			     Runtime::PANIC_INDEX_U,
			     Runtime::PANIC_EXTEND_INDEX,
			     Runtime::PANIC_EXTEND_INDEX_U,
			     inserter, loc);

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

void
Array_index_expression::do_address_taken(bool escapes)
{
  // In &x[0], if x is a slice, then x's address is not taken.
  if (!this->array_->type()->is_slice_type())
    this->array_->address_taken(escapes);
}

// Get the backend representation for an array index.

Bexpression*
Array_index_expression::do_get_backend(Translate_context* context)
{
  Array_type* array_type = this->array_->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->array_->type()->is_error());
      return context->backend()->error_expression();
    }
  go_assert(!array_type->is_slice_type()
	    || this->array_->is_multi_eval_safe());

  Location loc = this->location();
  Gogo* gogo = context->gogo();

  Type* int_type = Type::lookup_integer_type("int");
  Btype* int_btype = int_type->get_backend(gogo);

  // Convert the length and capacity to "int".  FIXME: Do we need to
  // do this?
  Bexpression* length = NULL;
  if (this->end_ == NULL || this->end_->is_nil_expression())
    {
      Expression* len = array_type->get_length(gogo, this->array_);
      length = len->get_backend(context);
      length = gogo->backend()->convert_expression(int_btype, length, loc);
    }

  Bexpression* capacity = NULL;
  if (this->end_ != NULL)
    {
      Expression* cap = array_type->get_capacity(gogo, this->array_);
      capacity = cap->get_backend(context);
      capacity = gogo->backend()->convert_expression(int_btype, capacity, loc);
    }

  Bexpression* cap_arg = capacity;
  if (this->cap_ != NULL)
    {
      cap_arg = this->cap_->get_backend(context);
      cap_arg = gogo->backend()->convert_expression(int_btype, cap_arg, loc);
    }

  if (length == NULL)
    length = cap_arg;

  if (this->start_->type()->integer_type() == NULL
      && !Type::are_convertible(int_type, this->start_->type(), NULL))
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

  Bexpression* start = this->start_->get_backend(context);
  start = gogo->backend()->convert_expression(int_btype, start, loc);

  Bfunction* bfn = context->function()->func_value()->get_decl();
  if (this->end_ == NULL)
    {
      // Simple array indexing.
      Bexpression* ret;
      if (!array_type->is_slice_type())
	{
	  Bexpression* array = this->array_->get_backend(context);
	  ret = gogo->backend()->array_index_expression(array, start, loc);
	}
      else
	{
	  Expression* valptr = array_type->get_value_pointer(gogo,
							     this->array_);
	  Bexpression* ptr = valptr->get_backend(context);
          ptr = gogo->backend()->pointer_offset_expression(ptr, start, loc);

	  Type* ele_type = this->array_->type()->array_type()->element_type();
	  Btype* ele_btype = ele_type->get_backend(gogo);
	  ret = gogo->backend()->indirect_expression(ele_btype, ptr, false,
						     loc);
	}
      return ret;
    }

  // Slice expression.

  Bexpression* end;
  if (this->end_->is_nil_expression())
    end = length;
  else
    {
      end = this->end_->get_backend(context);
      end = gogo->backend()->convert_expression(int_btype, end, loc);
    }

  Bexpression* result_length =
    gogo->backend()->binary_expression(OPERATOR_MINUS, end, start, loc);

  Bexpression* result_capacity =
    gogo->backend()->binary_expression(OPERATOR_MINUS, cap_arg, start, loc);

  // If the new capacity is zero, don't change val.  Otherwise we can
  // get a pointer to the next object in memory, keeping it live
  // unnecessarily.  When the capacity is zero, the actual pointer
  // value doesn't matter.
  Bexpression* zero =
    Expression::make_integer_ul(0, int_type, loc)->get_backend(context);
  Bexpression* cond =
    gogo->backend()->binary_expression(OPERATOR_EQEQ, result_capacity, zero,
				       loc);
  Bexpression* offset = gogo->backend()->conditional_expression(bfn, int_btype,
								cond, zero,
								start, loc);
  Expression* valptr = array_type->get_value_pointer(gogo, this->array_);
  Bexpression* val = valptr->get_backend(context);
  val = gogo->backend()->pointer_offset_expression(val, offset, loc);

  Btype* struct_btype = this->type()->get_backend(gogo);
  std::vector<Bexpression*> init;
  init.push_back(val);
  init.push_back(result_length);
  init.push_back(result_capacity);

  return gogo->backend()->constructor_expression(struct_btype, init, loc);
}

// Export an array index expression.

void
Array_index_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("(");
  this->array_->export_expression(efb);
  efb->write_c_string(")[");

  Type* old_context = efb->type_context();
  efb->set_type_context(Type::lookup_integer_type("int"));

  this->start_->export_expression(efb);
  if (this->end_ == NULL)
    go_assert(this->cap_ == NULL);
  else
    {
      efb->write_c_string(":");
      if (!this->end_->is_nil_expression())
	this->end_->export_expression(efb);
      if (this->cap_ != NULL)
	{
	  efb->write_c_string(":");
	  this->cap_->export_expression(efb);
	}
    }

  efb->set_type_context(old_context);

  efb->write_c_string("]");
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

// Class String_index_expression.

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
String_index_expression::do_flatten(Gogo* gogo, Named_object*,
                                    Statement_inserter* inserter)
{
  if (this->is_flattened_)
    return this;
  this->is_flattened_ = true;

  Location loc = this->location();

  if (this->is_error_expression())
    return Expression::make_error(loc);

  Expression* string = this->string_;
  Expression* start = this->start_;
  Expression* end = this->end_;
  if (string->is_error_expression()
      || string->type()->is_error_type()
      || start->is_error_expression()
      || start->type()->is_error_type()
      || (end != NULL
          && (end->is_error_expression() || end->type()->is_error_type())))
    {
      go_assert(saw_errors());
      return Expression::make_error(loc);
    }

  Temporary_statement* temp;
  if (!string->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, string, loc);
      inserter->insert(temp);
      this->string_ = Expression::make_temporary_reference(temp, loc);
      string = this->string_;
    }
  if (!start->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, start, loc);
      inserter->insert(temp);
      this->start_ = Expression::make_temporary_reference(temp, loc);
      start = this->start_;
    }
  if (end != NULL
      && !end->is_nil_expression()
      && !end->is_multi_eval_safe())
    {
      temp = Statement::make_temporary(NULL, end, loc);
      inserter->insert(temp);
      this->end_ = Expression::make_temporary_reference(temp, loc);
      end = this->end_;
    }

  Expression* len = Expression::make_string_info(string->copy(),
						 STRING_INFO_LENGTH, loc);
  temp = Statement::make_temporary(NULL, len, loc);
  temp->determine_types(gogo);
  inserter->insert(temp);
  len = Expression::make_temporary_reference(temp, loc);

  // The order of bounds checks here matches the order used by the gc
  // compiler, as tested by issue30116[u].go.

  if (end != NULL && !end->is_nil_expression())
    {
      Expression::check_bounds(gogo, end, OPERATOR_LE, len,
			       Runtime::PANIC_SLICE_ALEN,
			       Runtime::PANIC_SLICE_ALEN_U,
			       Runtime::PANIC_EXTEND_SLICE_ALEN,
			       Runtime::PANIC_EXTEND_SLICE_ALEN_U,
			       inserter, loc);
      Expression::check_bounds(gogo, start, OPERATOR_LE, end,
			       Runtime::PANIC_SLICE_B,
			       Runtime::PANIC_SLICE_B_U,
			       Runtime::PANIC_EXTEND_SLICE_B,
			       Runtime::PANIC_EXTEND_SLICE_B_U,
			       inserter, loc);
    }
  else if (end != NULL)
    Expression::check_bounds(gogo, start, OPERATOR_LE, len,
			     Runtime::PANIC_SLICE_B,
			     Runtime::PANIC_SLICE_B_U,
			     Runtime::PANIC_EXTEND_SLICE_B,
			     Runtime::PANIC_EXTEND_SLICE_B_U,
			     inserter, loc);
  else
    Expression::check_bounds(gogo, start, OPERATOR_LT, len,
			     Runtime::PANIC_INDEX,
			     Runtime::PANIC_INDEX_U,
			     Runtime::PANIC_EXTEND_INDEX,
			     Runtime::PANIC_EXTEND_INDEX_U,
			     inserter, loc);

  return this;
}

// Return the type of a string index.

Type*
String_index_expression::do_type()
{
  if (this->end_ == NULL)
    return Type::lookup_integer_type("byte");
  else
    return this->string_->type();
}

// Determine the type of a string index.

void
String_index_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->string_->determine_type_no_context(gogo);

  Type_context index_context(Type::lookup_integer_type("int"), false);
  this->start_->determine_type(gogo, &index_context);
  if (this->end_ != NULL)
    this->end_->determine_type(gogo, &index_context);
}

// Check types of a string index.

void
String_index_expression::do_check_types(Gogo*)
{
  if (!String_index_expression::check_indexes(this->string_, this->start_,
					      this->end_, this->location()))
    this->set_is_error();
}

// A static function to check string index types.  This is also called
// by Index_expression::do_check_types.  It reports whether type
// checking succeeded.

bool
String_index_expression::check_indexes(Expression* string, Expression* start,
				       Expression* end, Location loc)
{
  bool ret = true;

  Numeric_constant nc;
  unsigned long v;
  if (start->type()->integer_type() == NULL
      && !start->type()->is_error()
      && (!start->type()->is_abstract()
	  || !start->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    {
      go_error_at(loc, "index must be integer");
      ret = false;
    }

  if (end != NULL
      && end->type()->integer_type() == NULL
      && !end->type()->is_error()
      && !end->is_nil_expression()
      && !end->is_error_expression()
      && (!end->type()->is_abstract()
	  || !end->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    {
      go_error_at(loc, "slice end must be integer");
      ret = false;
    }

  std::string sval;
  bool sval_valid = string->string_constant_value(&sval);

  Numeric_constant inc;
  mpz_t ival;
  bool ival_valid = false;
  if (start->numeric_constant_value(&inc) && inc.to_int(&ival))
    {
      ival_valid = true;
      if (mpz_sgn(ival) < 0
	  || (sval_valid
	      && (end == NULL
		  ? mpz_cmp_ui(ival, sval.length()) >= 0
		  : mpz_cmp_ui(ival, sval.length()) > 0)))
	{
	  go_error_at(start->location(), "string index out of bounds");
	  ret = false;
	}
    }

  if (end != NULL && !end->is_nil_expression())
    {
      Numeric_constant enc;
      mpz_t eval;
      if (end->numeric_constant_value(&enc) && enc.to_int(&eval))
	{
	  if (mpz_sgn(eval) < 0
	      || (sval_valid && mpz_cmp_ui(eval, sval.length()) > 0))
	    {
	      go_error_at(end->location(), "string index out of bounds");
	      ret = false;
	    }
	  else if (ival_valid && mpz_cmp(ival, eval) > 0)
	    {
	      go_error_at(loc, "inverted slice range");
	      ret = false;
	    }
	  mpz_clear(eval);
	}
    }
  if (ival_valid)
    mpz_clear(ival);

  return ret;
}

// Get the backend representation for a string index.

Bexpression*
String_index_expression::do_get_backend(Translate_context* context)
{
  Location loc = this->location();
  Gogo* gogo = context->gogo();

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
      return context->backend()->error_expression();
    }

  go_assert(this->string_->is_multi_eval_safe());
  go_assert(this->start_->is_multi_eval_safe());

  Expression* start = Expression::make_cast(int_type, this->start_, loc);
  Bfunction* bfn = context->function()->func_value()->get_decl();

  Expression* length =
    Expression::make_string_info(this->string_, STRING_INFO_LENGTH, loc);
  Expression* bytes =
    Expression::make_string_info(this->string_, STRING_INFO_DATA, loc);

  Bexpression* bstart = start->get_backend(context);
  Bexpression* ptr = bytes->get_backend(context);

  if (this->end_ == NULL)
    {
      ptr = gogo->backend()->pointer_offset_expression(ptr, bstart, loc);
      Btype* ubtype = Type::lookup_integer_type("uint8")->get_backend(gogo);
      return gogo->backend()->indirect_expression(ubtype, ptr, false, loc);
    }

  Expression* end = NULL;
  if (this->end_->is_nil_expression())
    end = length;
  else
    {
      go_assert(this->end_->is_multi_eval_safe());
      end = Expression::make_cast(int_type, this->end_, loc);
    }

  end = end->copy();
  Bexpression* bend = end->get_backend(context);
  Bexpression* new_length =
    gogo->backend()->binary_expression(OPERATOR_MINUS, bend, bstart, loc);

  // If the new length is zero, don't change pointer.  Otherwise we can
  // get a pointer to the next object in memory, keeping it live
  // unnecessarily.  When the length is zero, the actual pointer
  // value doesn't matter.
  Btype* int_btype = int_type->get_backend(gogo);
  Bexpression* zero =
    Expression::make_integer_ul(0, int_type, loc)->get_backend(context);
  Bexpression* cond =
    gogo->backend()->binary_expression(OPERATOR_EQEQ, new_length, zero,
                                       loc);
  Bexpression* offset =
    gogo->backend()->conditional_expression(bfn, int_btype, cond, zero,
                                            bstart, loc);

  ptr = gogo->backend()->pointer_offset_expression(ptr, offset, loc);

  Btype* str_btype = this->type()->get_backend(gogo);
  std::vector<Bexpression*> init;
  init.push_back(ptr);
  init.push_back(new_length);
  return gogo->backend()->constructor_expression(str_btype, init, loc);
}

// Export a string index expression.

void
String_index_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("(");
  this->string_->export_expression(efb);
  efb->write_c_string(")[");

  Type* old_context = efb->type_context();
  efb->set_type_context(Type::lookup_integer_type("int"));

  this->start_->export_expression(efb);
  if (this->end_ != NULL)
    {
      efb->write_c_string(":");
      if (!this->end_->is_nil_expression())
	this->end_->export_expression(efb);
    }

  efb->set_type_context(old_context);

  efb->write_c_string("]");
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
  Map_type* mt = this->map_->type()->map_type();
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

void
Map_index_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->map_->determine_type_no_context(gogo);
  Map_type* mt = this->map_->type()->map_type();
  go_assert(mt != NULL);
  Type_context index_context(mt->key_type(), false);
  this->index_->determine_type(gogo, &index_context);
  if (this->value_pointer_ != NULL)
    this->value_pointer_->determine_type_no_context(gogo);
}

void
Map_index_expression::do_check_types(Gogo*)
{
  // Types should have been checked before this was created, so this
  // will probably never be called.  Check it anyhow to be sure.
  Map_type* mt = this->map_->type()->map_type();
  go_assert(mt != NULL);
  if (!Type::are_assignable(mt->key_type(), this->index_->type(), NULL))
    this->report_error(_("incompatible type for map index"));
}

// We need to pass in a pointer to the key, so flatten the index into a
// temporary variable if it isn't already.  The value pointer will be
// dereferenced and checked for nil, so flatten into a temporary to avoid
// recomputation.

Expression*
Map_index_expression::do_flatten(Gogo* gogo, Named_object*,
				 Statement_inserter* inserter)
{
  Location loc = this->location();
  Map_type* mt = this->get_map_type();
  if (this->index()->is_error_expression()
      || this->index()->type()->is_error_type()
      || mt->is_error_type())
    {
      go_assert(saw_errors());
      return Expression::make_error(loc);
    }

  // Avoid copy for string([]byte) conversions used in map keys.
  // mapaccess doesn't keep the reference, so this is safe.
  Type_conversion_expression* ce = this->index_->conversion_expression();
  if (ce != NULL && ce->type()->is_string_type()
      && ce->expr()->type()->is_slice_type())
    ce->set_no_copy(true);

  if (!Type::are_identical(mt->key_type(), this->index_->type(),
			   Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			   NULL))
    {
      if (this->index_->type()->interface_type() != NULL
	  && !this->index_->is_multi_eval_safe())
	{
	  Temporary_statement* temp =
	    Statement::make_temporary(NULL, this->index_, loc);
	  inserter->insert(temp);
	  this->index_ = Expression::make_temporary_reference(temp, loc);
	}
      this->index_ = Expression::convert_for_assignment(gogo, mt->key_type(),
							this->index_, loc);
    }

  if (!this->index_->is_multi_eval_safe())
    {
      Temporary_statement* temp = Statement::make_temporary(NULL, this->index_,
                                                            loc);
      inserter->insert(temp);
      this->index_ = Expression::make_temporary_reference(temp, loc);
    }

  if (this->value_pointer_ == NULL)
    this->get_value_pointer(gogo);
  if (this->value_pointer_->is_error_expression()
      || this->value_pointer_->type()->is_error_type())
    return Expression::make_error(loc);
  if (!this->value_pointer_->is_multi_eval_safe())
    {
      Temporary_statement* temp =
	Statement::make_temporary(NULL, this->value_pointer_, loc);
      inserter->insert(temp);
      this->value_pointer_ = Expression::make_temporary_reference(temp, loc);
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
  return mt->val_type();
}

// Add explicit type conversions.

void
Map_index_expression::do_add_conversions()
{
  Map_type* mt = this->get_map_type();
  if (mt == NULL)
    return;
  Type* lt = mt->key_type();
  Type* rt = this->index_->type();
  if (!Type::are_identical(lt, rt, 0, NULL)
      && lt->interface_type() != NULL)
    this->index_ = Expression::make_cast(lt, this->index_, this->location());
}

// Get the backend representation for a map index.

Bexpression*
Map_index_expression::do_get_backend(Translate_context* context)
{
  Map_type* type = this->get_map_type();
  if (type == NULL)
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

  go_assert(this->value_pointer_ != NULL
            && this->value_pointer_->is_multi_eval_safe());

  Expression* val = Expression::make_dereference(this->value_pointer_,
                                                 NIL_CHECK_NOT_NEEDED,
                                                 this->location());
  return val->get_backend(context);
}

// Get an expression for the map index.  This returns an expression
// that evaluates to a pointer to a value.  If the key is not in the
// map, the pointer will point to a zero value.

Expression*
Map_index_expression::get_value_pointer(Gogo* gogo)
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

      Expression* index_ptr = Expression::make_unary(OPERATOR_AND,
						     this->index_,
                                                     loc);

      Expression* type_expr = Expression::make_type_descriptor(type, loc);
      Expression* zero = type->fat_zero_value(gogo);
      Expression* map_index;
      if (zero == NULL)
        {
          Runtime::Function code;
          Expression* key;
          switch (type->algorithm(gogo))
            {
              case Map_type::MAP_ALG_FAST32:
              case Map_type::MAP_ALG_FAST32PTR:
                {
                  Type* uint32_type = Type::lookup_integer_type("uint32");
                  Type* uint32_ptr_type = Type::make_pointer_type(uint32_type);
                  key = Expression::make_unsafe_cast(uint32_ptr_type, index_ptr,
                                                     loc);
                  key = Expression::make_dereference(key, NIL_CHECK_NOT_NEEDED,
                                                     loc);
                  code = Runtime::MAPACCESS1_FAST32;
                  break;
                }
              case Map_type::MAP_ALG_FAST64:
              case Map_type::MAP_ALG_FAST64PTR:
                {
                  Type* uint64_type = Type::lookup_integer_type("uint64");
                  Type* uint64_ptr_type = Type::make_pointer_type(uint64_type);
                  key = Expression::make_unsafe_cast(uint64_ptr_type, index_ptr,
                                                     loc);
                  key = Expression::make_dereference(key, NIL_CHECK_NOT_NEEDED,
                                                     loc);
                  code = Runtime::MAPACCESS1_FAST64;
                  break;
                }
              case Map_type::MAP_ALG_FASTSTR:
                key = this->index_;
                code = Runtime::MAPACCESS1_FASTSTR;
                break;
              default:
                key = index_ptr;
                code = Runtime::MAPACCESS1;
                break;
            }
          map_index = Runtime::make_call(gogo, code, loc, 3,
                                         type_expr, map_ref, key);
        }
      else
        map_index = Runtime::make_call(gogo, Runtime::MAPACCESS1_FAT, loc, 4,
                                       type_expr, map_ref, index_ptr, zero);

      Type* val_type = type->val_type();
      this->value_pointer_ =
          Expression::make_unsafe_cast(Type::make_pointer_type(val_type),
                                       map_index, this->location());
      this->value_pointer_->determine_type_no_context(gogo);
    }

  return this->value_pointer_;
}

// Export a map index expression.

void
Map_index_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("(");
  this->map_->export_expression(efb);
  efb->write_c_string(")[");

  Type* old_context = efb->type_context();
  efb->set_type_context(this->get_map_type()->key_type());

  this->index_->export_expression(efb);

  efb->set_type_context(old_context);

  efb->write_c_string("]");
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
				     Statement_inserter* inserter)
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

  // References from functions generated by the compiler don't count.
  if (function != NULL && function->func_value()->is_type_specific_function())
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
  Named_type* nt = this->expr_->type()->unalias()->named_type();
  if (nt == NULL || nt->named_object()->package() == NULL)
    s.append(gogo->pkgpath());
  else
    s.append(nt->named_object()->package()->pkgpath());
  s.push_back('.');
  if (nt != NULL)
    s.append(Gogo::unpack_hidden_name(nt->name()));
  s.push_back('.');
  s.append(Gogo::unpack_hidden_name(field->field_name()));
  s.push_back('"');

  // We can't use a string here, because internally a string holds a
  // pointer to the actual bytes; when the linker garbage collects the
  // string, it won't garbage collect the bytes.  So we use a
  // [...]byte.

  Expression* length_expr = Expression::make_integer_ul(s.length(), NULL, loc);

  Type* byte_type = Type::lookup_integer_type("byte");
  Array_type* array_type = Type::make_array_type(byte_type, length_expr);
  array_type->set_is_array_incomparable();

  Expression_list* bytes = new Expression_list();
  for (std::string::const_iterator p = s.begin(); p != s.end(); p++)
    {
      unsigned char c = static_cast<unsigned char>(*p);
      bytes->push_back(Expression::make_integer_ul(c, NULL, loc));
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

  Expression* call = Runtime::make_call(gogo, Runtime::FIELDTRACK, loc, 1, e);
  call->determine_type_no_context(gogo);
  gogo->lower_expression(function, inserter, &call);
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

// Get the backend representation for a field reference.

Bexpression*
Field_reference_expression::do_get_backend(Translate_context* context)
{
  Bexpression* bstruct = this->expr_->get_backend(context);
  return context->gogo()->backend()->struct_field_expression(bstruct,
							     this->field_index_,
							     this->location());
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
    ref = Expression::make_dereference(ref, NIL_CHECK_DEFAULT, loc);

  Expression* mtable =
      Expression::make_interface_info(ref, INTERFACE_INFO_METHODS, loc);
  Struct_type* mtable_type = mtable->type()->points_to()->struct_type();

  std::string name = Gogo::unpack_hidden_name(this->name_);
  unsigned int index;
  const Struct_field* field = mtable_type->find_local_field(name, &index);
  go_assert(field != NULL);

  mtable = Expression::make_dereference(mtable, NIL_CHECK_NOT_NEEDED, loc);
  return Expression::make_field_reference(mtable, index, loc);
}

// Return an expression for the first argument to pass to the interface
// function.

Expression*
Interface_field_reference_expression::get_underlying_object()
{
  Expression* expr = this->expr_;
  if (expr->type()->points_to() != NULL)
    expr = Expression::make_dereference(expr, NIL_CHECK_DEFAULT,
                                        this->location());
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
Interface_field_reference_expression::do_flatten(Gogo*, Named_object*,
						 Statement_inserter* inserter)
{
  if (this->expr_->is_error_expression()
      || this->expr_->type()->is_error_type())
    {
      go_assert(saw_errors());
      return Expression::make_error(this->location());
    }

  if (!this->expr_->is_multi_eval_safe())
    {
      Temporary_statement* temp =
	Statement::make_temporary(NULL, this->expr_, this->location());
      inserter->insert(temp);
      this->expr_ = Expression::make_temporary_reference(temp, this->location());
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
Interface_field_reference_expression::do_determine_type(Gogo* gogo,
							const Type_context*)
{
  this->expr_->determine_type_no_context(gogo);
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
	  go_error_at(this->location(), "method %qs not in interface",
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
    return Named_object::make_erroneous_name(gogo->thunk_name());

  Function_type* orig_fntype = method_id->type()->function_type();
  if (orig_fntype == NULL)
    return Named_object::make_erroneous_name(gogo->thunk_name());

  Struct_field_list* sfl = new Struct_field_list();
  // The type here is wrong--it should be the C function type.  But it
  // doesn't really matter.
  Type* vt = Type::make_pointer_type(Type::make_void_type());
  sfl->push_back(Struct_field(Typed_identifier("fn", vt, loc)));
  sfl->push_back(Struct_field(Typed_identifier("val", type, loc)));
  Struct_type* st = Type::make_struct_type(sfl, loc);
  st->set_is_struct_incomparable();
  Type* closure_type = Type::make_pointer_type(st);

  Function_type* new_fntype = orig_fntype->copy_with_names();

  std::string thunk_name = gogo->thunk_name();
  Named_object* new_no = gogo->start_function(thunk_name, new_fntype,
					      false, loc);

  Variable* cvar = new Variable(closure_type, NULL, false, false, false, loc);
  cvar->set_is_used();
  cvar->set_is_closure();
  Named_object* cp = Named_object::make_variable("$closure" + thunk_name,
						 NULL, cvar);
  new_no->func_value()->set_closure_var(cp);

  gogo->start_block(loc);

  // Field 0 of the closure is the function code pointer, field 1 is
  // the value on which to invoke the method.
  Expression* arg = Expression::make_var_reference(cp, loc);
  arg = Expression::make_dereference(arg, NIL_CHECK_NOT_NEEDED, loc);
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

  Statement* s = Statement::make_return_from_call(new_no, call, loc);
  s->determine_types(gogo);
  gogo->add_statement(s);
  Block* b = gogo->finish_block(loc);
  gogo->add_block(b, loc);

  // This is called after lowering.
  gogo->lower_block(new_no, b);

  gogo->finish_function(loc);

  ins.first->second->push_back(std::make_pair(name, new_no));
  return new_no;
}

// Lookup a thunk to call method NAME on TYPE.

Named_object*
Interface_field_reference_expression::lookup_thunk(Interface_type* type,
						   const std::string& name)
{
  Interface_method_thunks::const_iterator p =
    Interface_field_reference_expression::interface_method_thunks.find(type);
  if (p == Interface_field_reference_expression::interface_method_thunks.end())
    return NULL;
  for (Method_thunks::const_iterator pm = p->second->begin();
       pm != p->second->end();
       ++pm)
    if (pm->first == name)
      return pm->second;
  return NULL;
}

// Get the backend representation for a method value.

Bexpression*
Interface_field_reference_expression::do_get_backend(Translate_context* context)
{
  Interface_type* type = this->expr_->type()->interface_type();
  if (type == NULL)
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

  Named_object* thunk =
    Interface_field_reference_expression::lookup_thunk(type, this->name_);

  // The thunk should have been created during the
  // create_function_descriptors pass.
  if (thunk == NULL || thunk->is_erroneous())
    {
      go_assert(saw_errors());
      return context->backend()->error_expression();
    }

  // FIXME: We should lower this earlier, but we can't it lower it in
  // the lowering pass because at that point we don't know whether we
  // need to create the thunk or not.  If the expression is called, we
  // don't need the thunk.

  Location loc = this->location();

  Struct_field_list* fields = new Struct_field_list();
  fields->push_back(Struct_field(Typed_identifier("fn",
						  thunk->func_value()->type(),
						  loc)));
  fields->push_back(Struct_field(Typed_identifier("val",
						  this->expr_->type(),
						  loc)));
  Struct_type* st = Type::make_struct_type(fields, loc);
  st->set_is_struct_incomparable();

  Expression_list* vals = new Expression_list();
  vals->push_back(Expression::make_func_code_reference(thunk, loc));
  vals->push_back(this->expr_);

  Expression* expr = Expression::make_struct_composite_literal(st, vals, loc);
  Bexpression* bclosure =
    Expression::make_heap_expression(expr, loc)->get_backend(context);

  Gogo* gogo = context->gogo();
  Btype* btype = this->type()->get_backend(gogo);
  bclosure = gogo->backend()->convert_expression(btype, bclosure, loc);

  Expression* nil_check =
      Expression::make_binary(OPERATOR_EQEQ, this->expr_,
                              Expression::make_nil(loc), loc);
  Bexpression* bnil_check = nil_check->get_backend(context);

  Expression* crash = Runtime::make_call(gogo, Runtime::PANIC_MEM, loc, 0);
  crash->determine_type_no_context(gogo);
  Bexpression* bcrash = crash->get_backend(context);

  Bfunction* bfn = context->function()->func_value()->get_decl();
  Bexpression* bcond =
      gogo->backend()->conditional_expression(bfn, NULL,
                                              bnil_check, bcrash, NULL, loc);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  Bstatement* cond_statement =
      gogo->backend()->expression_statement(bfunction, bcond);
  return gogo->backend()->compound_expression(cond_statement, bclosure, loc);
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

// Class Allocation_expression.

int
Allocation_expression::do_traverse(Traverse* traverse)
{
  return Type::traverse(this->type_, traverse);
}

Type*
Allocation_expression::do_type()
{
  return Type::make_pointer_type(this->type_);
}

void
Allocation_expression::do_check_types(Gogo*)
{
  if (!this->type_->in_heap())
    go_error_at(this->location(), "cannot heap allocate go:notinheap type");
}

// Make a copy of an allocation expression.

Expression*
Allocation_expression::do_copy()
{
  Allocation_expression* alloc =
    new Allocation_expression(this->type_->copy_expressions(),
			      this->location());
  if (this->allocate_on_stack_)
    alloc->set_allocate_on_stack();
  if (this->no_zero_)
    alloc->set_no_zero();
  return alloc;
}

// Return the backend representation for an allocation expression.

Bexpression*
Allocation_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location loc = this->location();
  Btype* btype = this->type_->get_backend(gogo);

  if (this->allocate_on_stack_)
    {
      int64_t size;
      bool ok = this->type_->backend_type_size(gogo, &size);
      if (!ok)
        {
          go_assert(saw_errors());
          return gogo->backend()->error_expression();
        }
      Bstatement* decl;
      Named_object* fn = context->function();
      go_assert(fn != NULL);
      Bfunction* fndecl = fn->func_value()->get_or_make_decl(gogo, fn);
      Bexpression* init = (this->no_zero_
                           ? NULL
                           : gogo->backend()->zero_expression(btype));
      Bvariable* temp =
        gogo->backend()->temporary_variable(fndecl, context->bblock(), btype,
                                            init,
					    Backend::variable_address_is_taken,
					    loc, &decl);
      Bexpression* ret = gogo->backend()->var_expression(temp, loc);
      ret = gogo->backend()->address_expression(ret, loc);
      ret = gogo->backend()->compound_expression(decl, ret, loc);
      return ret;
    }

  Bexpression* space =
    gogo->allocate_memory(this->type_, loc)->get_backend(context);
  Btype* pbtype = gogo->backend()->pointer_type(btype);
  return gogo->backend()->convert_expression(pbtype, space, loc);
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

// Class Ordered_value_list.

int
Ordered_value_list::traverse_vals(Traverse* traverse)
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
	  for (std::vector<unsigned long>::const_iterator p =
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
  return TRAVERSE_CONTINUE;
}

// Class Struct_construction_expression.

// Traversal.

int
Struct_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->traverse_vals(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return whether this is a constant initializer.

bool
Struct_construction_expression::is_constant_struct() const
{
  if (this->vals() == NULL)
    return true;
  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
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

// Return whether this is a zero value.

bool
Struct_construction_expression::do_is_zero_value() const
{
  if (this->vals() == NULL)
    return true;
  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    if (*pv != NULL && !(*pv)->is_zero_value())
      return false;

  const Struct_field_list* fields = this->type_->struct_type()->fields();
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      // Interface conversion may cause a zero value being converted
      // to a non-zero value, like interface{}(0).  Be conservative.
      if (pf->type()->interface_type() != NULL)
        return false;
    }

  return true;
}

// Return whether this struct can be used as a constant initializer.

bool
Struct_construction_expression::do_is_static_initializer() const
{
  if (this->vals() == NULL)
    return true;
  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    {
      if (*pv != NULL && !(*pv)->is_static_initializer())
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
Struct_construction_expression::do_determine_type(Gogo* gogo,
						  const Type_context*)
{
  if (this->vals() == NULL)
    return;
  const Struct_field_list* fields = this->type_->struct_type()->fields();
  Expression_list::const_iterator pv = this->vals()->begin();
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++pv)
    {
      if (pv == this->vals()->end())
	return;
      if (*pv != NULL)
	{
	  Type_context subcontext(pf->type(), false);
	  (*pv)->determine_type(gogo, &subcontext);
	}
    }
  // Extra values are an error we will report elsewhere; we still want
  // to determine the type to avoid knockon errors.
  for (; pv != this->vals()->end(); ++pv)
    (*pv)->determine_type_no_context(gogo);
}

// Check types.

void
Struct_construction_expression::do_check_types(Gogo* gogo)
{
  Struct_construction_expression::check_value_types(gogo, this->type_,
						    this->vals(),
						    this->location());
}

// Check types.  This static function is also called by
// Composite_literal_expression::do_check_types.  Reports whether type
// checking succeeded.

bool
Struct_construction_expression::check_value_types(Gogo* gogo,
						  Type* type,
						  Expression_list* vals,
						  Location loc)
{
  if (vals == NULL || vals->empty())
    return true;

  Struct_type* st = type->struct_type();
  if (vals->size() > st->field_count())
    {
      go_error_at(loc, "too many expressions for struct");
      return false;
    }

  bool imported_type =
    (type->named_type() != NULL
     && type->named_type()->named_object()->package() != NULL);

  bool ret = true;
  const Struct_field_list* fields = st->fields();
  Expression_list::const_iterator pv = vals->begin();
  int i = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++pv, ++i)
    {
      if (pv == vals->end())
	{
	  go_error_at(loc, "too few expressions for struct");
	  return false;
	}

      if (*pv == NULL)
	continue;

      if (imported_type
	  && (Gogo::is_hidden_name(pf->field_name())
	      || pf->is_embedded_builtin(gogo)))
	{
	  go_error_at(loc,
		      "assignment of unexported field %qs in %qs literal",
		      Gogo::message_name(pf->field_name()).c_str(),
		      type->named_type()->message_name().c_str());
	  ret = false;
	}

      std::string reason;
      if (!Type::are_assignable(pf->type(), (*pv)->type(), &reason))
	{
	  if (reason.empty())
	    go_error_at((*pv)->location(),
                        "incompatible type for field %d in struct construction",
                        i + 1);
	  else
	    go_error_at((*pv)->location(),
                        ("incompatible type for field %d in "
                         "struct construction (%s)"),
                        i + 1, reason.c_str());
	  ret = false;
	}
    }
  go_assert(pv == vals->end());
  return ret;
}

// Copy.

Expression*
Struct_construction_expression::do_copy()
{
  Struct_construction_expression* ret =
    new Struct_construction_expression(this->type_->copy_expressions(),
				       (this->vals() == NULL
					? NULL
					: this->vals()->copy()),
				       this->location());
  if (this->traverse_order() != NULL)
    ret->set_traverse_order(this->traverse_order());
  return ret;
}

// Make implicit type conversions explicit.

void
Struct_construction_expression::do_add_conversions()
{
  if (this->vals() == NULL)
    return;

  Location loc = this->location();
  const Struct_field_list* fields = this->type_->struct_type()->fields();
  Expression_list::iterator pv = this->vals()->begin();
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++pv)
    {
      if (pv == this->vals()->end())
        break;
      if (*pv != NULL)
        {
          Type* ft = pf->type();
          if (!Type::are_identical(ft, (*pv)->type(), 0, NULL)
              && ft->interface_type() != NULL)
           *pv = Expression::make_cast(ft, *pv, loc);
        }
    }
}

// Return the backend representation for constructing a struct.

Bexpression*
Struct_construction_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  Btype* btype = this->type_->get_backend(gogo);
  if (this->vals() == NULL)
    return gogo->backend()->zero_expression(btype);

  const Struct_field_list* fields = this->type_->struct_type()->fields();
  Expression_list::const_iterator pv = this->vals()->begin();
  std::vector<Bexpression*> init;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      Btype* fbtype = pf->type()->get_backend(gogo);
      if (pv == this->vals()->end())
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
          init.push_back(val->get_backend(context));
	  ++pv;
	}
    }
  if (this->type_->struct_type()->has_padding())
    {
      // Feed an extra value if there is a padding field.
      Btype *fbtype = Type::lookup_integer_type("uint8")->get_backend(gogo);
      init.push_back(gogo->backend()->zero_expression(fbtype));
    }
  return gogo->backend()->constructor_expression(btype, init, this->location());
}

// Export a struct construction.

void
Struct_construction_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("$convert(");
  efb->write_type(this->type_);
  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    {
      efb->write_c_string(", ");
      if (*pv != NULL)
	(*pv)->export_expression(efb);
    }
  efb->write_c_string(")");
}

// Dump ast representation of a struct construction expression.

void
Struct_construction_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << "{";
  ast_dump_context->dump_expression_list(this->vals());
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

// Class Array_construction_expression.

// Traversal.

int
Array_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->traverse_vals(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return whether this is a constant initializer.

bool
Array_construction_expression::is_constant_array() const
{
  if (this->vals() == NULL)
    return true;

  // There are no constant constructors for interfaces.
  if (this->type_->array_type()->element_type()->interface_type() != NULL)
    return false;

  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
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

// Return whether this is a zero value.

bool
Array_construction_expression::do_is_zero_value() const
{
  if (this->vals() == NULL)
    return true;

  // Interface conversion may cause a zero value being converted
  // to a non-zero value, like interface{}(0).  Be conservative.
  if (this->type_->array_type()->element_type()->interface_type() != NULL)
    return false;

  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    if (*pv != NULL && !(*pv)->is_zero_value())
      return false;

  return true;
}

// Return whether this can be used a constant initializer.

bool
Array_construction_expression::do_is_static_initializer() const
{
  if (this->vals() == NULL)
    return true;

  // There are no constant constructors for interfaces.
  if (this->type_->array_type()->element_type()->interface_type() != NULL)
    return false;

  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    {
      if (*pv != NULL && !(*pv)->is_static_initializer())
	return false;
    }
  return true;
}

// Final type determination.

void
Array_construction_expression::do_determine_type(Gogo* gogo,
						 const Type_context*)
{
  if (this->is_error_expression())
    {
      go_assert(saw_errors());
      return;
    }

  if (this->vals() == NULL)
    return;
  Array_type* at = this->type_->array_type();
  if (at == NULL || at->is_error() || at->element_type()->is_error())
    {
      go_assert(saw_errors());
      this->set_is_error();
      return;
    }
  Type_context subcontext(at->element_type(), false);
  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    {
      if (*pv != NULL)
	(*pv)->determine_type(gogo, &subcontext);
    }
}

// Check types.

void
Array_construction_expression::do_check_types(Gogo*)
{
  if (this->is_error_expression())
    {
      go_assert(saw_errors());
      return;
    }

  if (this->vals() == NULL)
    return;

  Array_type* at = this->type_->array_type();
  if (at == NULL || at->is_error() || at->element_type()->is_error())
    {
      go_assert(saw_errors());
      this->set_is_error();
      return;
    }
  int i = 0;
  Type* element_type = at->element_type();
  for (Expression_list::const_iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv, ++i)
    {
      if (*pv != NULL
	  && !Type::are_assignable(element_type, (*pv)->type(), NULL))
	{
	  go_error_at((*pv)->location(),
                      "incompatible type for element %d in composite literal",
                      i + 1);
	  this->set_is_error();
	}
    }
}

// Make implicit type conversions explicit.

void
Array_construction_expression::do_add_conversions()
{
  if (this->is_error_expression())
    {
      go_assert(saw_errors());
      return;
    }

  if (this->vals() == NULL)
    return;

  Type* et = this->type_->array_type()->element_type();
  if (et->interface_type() == NULL)
    return;

  Location loc = this->location();
  for (Expression_list::iterator pv = this->vals()->begin();
       pv != this->vals()->end();
       ++pv)
    if (!Type::are_identical(et, (*pv)->type(), 0, NULL))
      *pv = Expression::make_cast(et, *pv, loc);
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
  if (this->vals() != NULL)
    {
      size_t i = 0;
      std::vector<unsigned long>::const_iterator pi;
      if (this->indexes_ != NULL)
	pi = this->indexes_->begin();
      for (Expression_list::const_iterator pv = this->vals()->begin();
	   pv != this->vals()->end();
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
	      vals.push_back(val_expr->get_backend(context));
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
Array_construction_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("$convert(");
  efb->write_type(this->type_);
  if (this->vals() != NULL)
    {
      std::vector<unsigned long>::const_iterator pi;
      if (this->indexes_ != NULL)
	pi = this->indexes_->begin();
      for (Expression_list::const_iterator pv = this->vals()->begin();
	   pv != this->vals()->end();
	   ++pv)
	{
	  efb->write_c_string(", ");

	  if (this->indexes_ != NULL)
	    {
	      char buf[100];
	      snprintf(buf, sizeof buf, "%lu", *pi);
	      efb->write_c_string(buf);
	      efb->write_c_string(":");
	    }

	  if (*pv != NULL)
	    (*pv)->export_expression(efb);

	  if (this->indexes_ != NULL)
	    ++pi;
	}
    }
  efb->write_c_string(")");
}

// Dump ast representation of an array construction expression.

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
  this->dump_slice_storage_expression(ast_dump_context);
  ast_dump_context->ostream() << "{" ;
  if (this->indexes_ == NULL)
    ast_dump_context->dump_expression_list(this->vals());
  else
    {
      Expression_list::const_iterator pv = this->vals()->begin();
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

// Class Fixed_array_construction_expression.

Fixed_array_construction_expression::Fixed_array_construction_expression(
    Type* type, const std::vector<unsigned long>* indexes,
    Expression_list* vals, Location location)
  : Array_construction_expression(EXPRESSION_FIXED_ARRAY_CONSTRUCTION,
				  type, indexes, vals, location)
{ go_assert(type->array_type() != NULL && !type->is_slice_type()); }


// Copy.

Expression*
Fixed_array_construction_expression::do_copy()
{
  Type* t = this->type()->copy_expressions();
  return new Fixed_array_construction_expression(t, this->indexes(),
						 (this->vals() == NULL
						  ? NULL
						  : this->vals()->copy()),
						 this->location());
}

// Return the backend representation for constructing a fixed array.

Bexpression*
Fixed_array_construction_expression::do_get_backend(Translate_context* context)
{
  Type* type = this->type();
  Btype* btype = type->get_backend(context->gogo());
  return this->get_constructor(context, btype);
}

Expression*
Expression::make_array_composite_literal(Type* type, Expression_list* vals,
                                         Location location)
{
  go_assert(type->array_type() != NULL && !type->is_slice_type());
  return new Fixed_array_construction_expression(type, NULL, vals, location);
}

// Class Slice_construction_expression.

Slice_construction_expression::Slice_construction_expression(
  Type* type, const std::vector<unsigned long>* indexes,
  Expression_list* vals, Location location)
  : Array_construction_expression(EXPRESSION_SLICE_CONSTRUCTION,
				  type, indexes, vals, location),
    valtype_(NULL), array_val_(NULL), slice_storage_(NULL),
    storage_escapes_(true)
{
  go_assert(type->is_slice_type());

  unsigned long lenval;
  Expression* length;
  if (vals == NULL || vals->empty())
    lenval = 0;
  else
    {
      if (this->indexes() == NULL)
	lenval = vals->size();
      else
	lenval = indexes->back() + 1;
    }
  Type* int_type = Type::lookup_integer_type("int");
  length = Expression::make_integer_ul(lenval, int_type, location);
  Type* element_type = type->array_type()->element_type();
  Array_type* array_type = Type::make_array_type(element_type, length);
  array_type->set_is_array_incomparable();
  this->valtype_ = array_type;
}

// Traversal.

int
Slice_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->Array_construction_expression::do_traverse(traverse)
      == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Type::traverse(this->valtype_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->array_val_ != NULL
      && Expression::traverse(&this->array_val_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->slice_storage_ != NULL
      && Expression::traverse(&this->slice_storage_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Helper routine to create fixed array value underlying the slice literal.
// May be called during flattening, or later during do_get_backend().

Expression*
Slice_construction_expression::create_array_val()
{
  Array_type* array_type = this->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->type()->is_error());
      return NULL;
    }

  Location loc = this->location();
  go_assert(this->valtype_ != NULL);

  Expression_list* vals = this->vals();
  return new Fixed_array_construction_expression(
      this->valtype_, this->indexes(), vals, loc);
}

// If we're previous established that the slice storage does not
// escape, then create a separate array temp val here for it. We
// need to do this as part of flattening so as to be able to insert
// the new temp statement.

Expression*
Slice_construction_expression::do_flatten(Gogo*, Named_object*,
                                          Statement_inserter* inserter)
{
  if (this->type()->array_type() == NULL)
    {
      go_assert(saw_errors());
      return Expression::make_error(this->location());
    }

  // Create a stack-allocated storage temp if storage won't escape
  if (!this->storage_escapes_
      && this->slice_storage_ == NULL
      && this->element_count() > 0)
    {
      Location loc = this->location();
      this->array_val_ = this->create_array_val();
      go_assert(this->array_val_ != NULL);
      Temporary_statement* temp =
          Statement::make_temporary(this->valtype_, this->array_val_, loc);
      inserter->insert(temp);
      this->slice_storage_ = Expression::make_temporary_reference(temp, loc);
    }
  return this;
}

// When dumping a slice construction expression that has an explicit
// storeage temp, emit the temp here (if we don't do this the storage
// temp appears unused in the AST dump).

void
Slice_construction_expression::
dump_slice_storage_expression(Ast_dump_context* ast_dump_context) const
{
  if (this->slice_storage_ == NULL)
    return;
  ast_dump_context->ostream() << "storage=" ;
  ast_dump_context->dump_expression(this->slice_storage_);
}

// Copy.

Expression*
Slice_construction_expression::do_copy()
{
  return new Slice_construction_expression(this->type()->copy_expressions(),
					   this->indexes(),
					   (this->vals() == NULL
					    ? NULL
					    : this->vals()->copy()),
					   this->location());
}

// Return the backend representation for constructing a slice.

Bexpression*
Slice_construction_expression::do_get_backend(Translate_context* context)
{
  if (this->array_val_ == NULL)
    this->array_val_ = this->create_array_val();
  if (this->array_val_ == NULL)
    {
      go_assert(this->type()->is_error());
      return context->backend()->error_expression();
    }

  Location loc = this->location();

  bool is_static_initializer = this->array_val_->is_static_initializer();

  // We have to copy the initial values into heap memory if we are in
  // a function or if the values are not constants.
  bool copy_to_heap = context->function() != NULL || !is_static_initializer;

  Expression* space;

  if (this->slice_storage_ != NULL)
    {
      go_assert(!this->storage_escapes_);
      space = Expression::make_unary(OPERATOR_AND, this->slice_storage_, loc);
    }
  else if (!copy_to_heap)
    {
      // The initializer will only run once.
      space = Expression::make_unary(OPERATOR_AND, this->array_val_, loc);
      space->unary_expression()->set_is_slice_init();
    }
  else
    {
      go_assert(this->storage_escapes_ || this->element_count() == 0);
      space = Expression::make_heap_expression(this->array_val_, loc);
    }
  Array_type* at = this->valtype_->array_type();
  Type* et = at->element_type();
  space = Expression::make_unsafe_cast(Type::make_pointer_type(et),
				       space, loc);

  // Build a constructor for the slice.
  Expression* len = at->length();
  Expression* slice_val =
    Expression::make_slice_value(this->type(), space, len, len, loc);
  return slice_val->get_backend(context);
}

// Make a slice composite literal.  This is used by the type
// descriptor code.

Slice_construction_expression*
Expression::make_slice_composite_literal(Type* type, Expression_list* vals,
					 Location location)
{
  go_assert(type->is_slice_type());
  return new Slice_construction_expression(type, NULL, vals, location);
}

// Class Map_construction_expression.

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

void
Map_construction_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  Map_type* mt = this->type_->map_type();
  go_assert(mt != NULL);
  Type_context key_context(mt->key_type(), false);
  Type_context val_context(mt->val_type(), false);
  if (this->vals_ != NULL)
    {
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv)
	{
	  (*pv)->determine_type(gogo, &key_context);
	  ++pv;
	  (*pv)->determine_type(gogo, &val_context);
	}
    }
}

void
Map_construction_expression::do_check_types(Gogo*)
{
  // Types should have been checked before this was created, so this
  // will probably never be called.  Check it anyhow to be sure.

  if (this->vals_ == NULL)
    return;

  Map_type* mt = this->type_->map_type();
  go_assert(mt != NULL);
  Type* key_type = mt->key_type();
  Type* val_type = mt->val_type();
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (!Type::are_assignable(key_type, (*pv)->type(), NULL))
	{
	  go_error_at((*pv)->location(),
		      "incompatible type for key in map composite literal");
	  this->set_is_error();
	}
      ++pv;
      if (!Type::are_assignable(val_type, (*pv)->type(), NULL))
	{
	  go_error_at((*pv)->location(),
		      "incompatible type for value in map composite literal");
	  this->set_is_error();
	}
    }
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
          Expression* key = *pv;
          if (key->is_error_expression() || key->type()->is_error_type())
            {
              go_assert(saw_errors());
              return Expression::make_error(loc);
            }
	  if (key->type()->interface_type() != NULL
	      && !key->is_multi_eval_safe())
	    {
	      Temporary_statement* temp =
		Statement::make_temporary(NULL, key, loc);
	      inserter->insert(temp);
	      key = Expression::make_temporary_reference(temp, loc);
	    }
	  key = Expression::convert_for_assignment(gogo, key_type, key, loc);

          ++pv;
          Expression* val = *pv;
          if (val->is_error_expression() || val->type()->is_error_type())
            {
              go_assert(saw_errors());
              return Expression::make_error(loc);
            }
	  if (val->type()->interface_type() != NULL
	      && !val->is_multi_eval_safe())
	    {
	      Temporary_statement* temp =
		Statement::make_temporary(NULL, val, loc);
	      inserter->insert(temp);
	      val = Expression::make_temporary_reference(temp, loc);
	    }
	  val = Expression::convert_for_assignment(gogo, val_type, val, loc);

          key_value_pair->push_back(key);
          key_value_pair->push_back(val);
          value_pairs->push_back(
              Expression::make_struct_composite_literal(this->element_type_,
                                                        key_value_pair, loc));
        }

      Expression* element_count = Expression::make_integer_ul(i, NULL, loc);
      Array_type* ctor_type =
          Type::make_array_type(this->element_type_, element_count);
      ctor_type->set_is_array_incomparable();
      Expression* constructor =
          new Fixed_array_construction_expression(ctor_type, NULL,
                                                  value_pairs, loc);

      this->constructor_temp_ =
          Statement::make_temporary(NULL, constructor, loc);
      constructor->issue_nil_check();
      this->constructor_temp_->set_is_address_taken();
      this->constructor_temp_->determine_types(gogo);
      inserter->insert(this->constructor_temp_);
    }

  return this;
}

// Copy.

Expression*
Map_construction_expression::do_copy()
{
  return new Map_construction_expression(this->type_->copy_expressions(),
					 (this->vals_ == NULL
					  ? NULL
					  : this->vals_->copy()),
					 this->location());
}

// Make implicit type conversions explicit.

void
Map_construction_expression::do_add_conversions()
{
  if (this->vals_ == NULL || this->vals_->empty())
    return;

  Map_type* mt = this->type_->map_type();
  Type* kt = mt->key_type();
  Type* vt = mt->val_type();
  bool key_is_interface = (kt->interface_type() != NULL);
  bool val_is_interface = (vt->interface_type() != NULL);
  if (!key_is_interface && !val_is_interface)
    return;

  Location loc = this->location();
  for (Expression_list::iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (key_is_interface &&
          !Type::are_identical(kt, (*pv)->type(), 0, NULL))
        *pv = Expression::make_cast(kt, *pv, loc);
      ++pv;
      if (val_is_interface &&
          !Type::are_identical(vt, (*pv)->type(), 0, NULL))
        *pv = Expression::make_cast(vt, *pv, loc);
    }
}

// Return the backend representation for constructing a map.

Bexpression*
Map_construction_expression::do_get_backend(Translate_context* context)
{
  if (this->is_error_expression())
    return context->backend()->error_expression();
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
  Expression* descriptor = Expression::make_type_descriptor(mt, loc);

  Type* uintptr_t = Type::lookup_integer_type("uintptr");
  Expression* count = Expression::make_integer_ul(i, uintptr_t, loc);

  Expression* entry_size =
      Expression::make_type_info(this->element_type_, TYPE_INFO_SIZE);

  unsigned int field_index;
  const Struct_field* valfield =
      this->element_type_->find_local_field("__val", &field_index);
  Expression* val_offset =
      Expression::make_struct_field_offset(this->element_type_, valfield);

  Gogo* gogo = context->gogo();
  Expression* map_ctor =
    Runtime::make_call(gogo, Runtime::CONSTRUCT_MAP, loc, 5,
		       descriptor, count, entry_size, val_offset, ventries);
  map_ctor->determine_type_no_context(gogo);
  return map_ctor->get_backend(context);
}

// Export an array construction.

void
Map_construction_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("$convert(");
  efb->write_type(this->type_);
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      efb->write_c_string(", ");
      (*pv)->export_expression(efb);
    }
  efb->write_c_string(")");
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

// A composite literal key.  This is seen during parsing, but is not
// resolved to a named_object in case this is a composite literal of
// struct type.

class Composite_literal_key_expression : public Parser_expression
{
 public:
  Composite_literal_key_expression(const std::string& name, Location location)
    : Parser_expression(EXPRESSION_COMPOSITE_LITERAL_KEY, location),
      name_(name), expr_(NULL)
  { }

  const std::string&
  name() const
  { return this->name_; }

 protected:
  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*);

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*);

  Expression*
  do_copy()
  {
    return new Composite_literal_key_expression(this->name_, this->location());
  }

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The name.
  std::string name_;
  // After we look up the name, a corresponding expression.
  Expression* expr_;
};

// Determine the type of a composite literal key.  We will never get
// here for keys in composite literals of struct types, because they
// will be handled by Composite_literal_expression::do_determine_type.
// So if we get here, this must be a regular name reference after all.

void
Composite_literal_key_expression::do_determine_type(
    Gogo* gogo,
    const Type_context* context)
{
  if (this->expr_ != NULL)
    {
      // Already resolved.
      return;
    }

  Named_object* no = gogo->lookup(this->name_, NULL);
  if (no == NULL)
    {
      // Gogo::lookup doesn't look in the global namespace, and names
      // used in composite literal keys aren't seen by
      // Gogo::define_global_names, so we have to look in the global
      // namespace ourselves.
      no = gogo->lookup_global(Gogo::unpack_hidden_name(this->name_).c_str());
      if (no == NULL)
	{
	  go_error_at(this->location(), "reference to undefined name %qs",
		      Gogo::message_name(this->name_).c_str());
	  this->set_is_error();
	  return;
	}
    }

  this->expr_ = Expression::make_unknown_reference(no, this->location());
  this->expr_->determine_type(gogo, context);
}

Type*
Composite_literal_key_expression::do_type()
{
  if (this->is_error_expression())
    return Type::make_error_type();
  go_assert(this->expr_ != NULL);
  return this->expr_->type();
}

Expression*
Composite_literal_key_expression::do_lower(Gogo*, Named_object*,
					   Statement_inserter*)
{
  if (this->is_error_expression())
    return Expression::make_error(this->location());
  go_assert(this->expr_ != NULL);
  return this->expr_;
}

// Dump a composite literal key.

void
Composite_literal_key_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "_UnknownName_(" << this->name_ << ")";
}

// Make a composite literal key.

Expression*
Expression::make_composite_literal_key(const std::string& name,
				       Location location)
{
  return new Composite_literal_key_expression(name, location);
}

// Class Composite_literal_expression.

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

      for (int depth = 0; depth < this->depth_; ++depth)
        {
	  type = type->deref();
          if (type->array_type() != NULL)
            type = type->array_type()->element_type();
          else if (type->map_type() != NULL)
            {
              if (this->key_path_[depth])
                type = type->map_type()->key_type();
              else
                type = type->map_type()->val_type();
            }
          else
            {
              // This error will be reported during lowering.
              return TRAVERSE_CONTINUE;
            }
        }
      type = type->deref();

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

void
Composite_literal_expression::do_determine_type(Gogo* gogo,
						const Type_context*)
{
  // Resolve the type if this composite literal is within another
  // composite literal and has omitted the type.  The DEPTH_ field
  // tells us how deeply this one is embedded.
  Type* type = this->type_;
  for (int depth = 0; depth < this->depth_; ++depth)
    {
      type = type->deref();
      if (type->array_type() != NULL)
	type = type->array_type()->element_type();
      else if (type->map_type() != NULL)
	{
	  if (this->key_path_[depth])
	    type = type->map_type()->key_type();
	  else
	    type = type->map_type()->val_type();
	}
      else
	{
	  if (!type->is_error())
	    this->report_error(_("may only omit types within composite "
				 "literals of slice, array, or map type"));
	  else
	    {
	      go_assert(saw_errors());
	      this->set_is_error();
	    }
	  return;
	}
    }

  this->type_ = type;
  this->depth_ = 0;

  type = type->deref();

  if (this->vals_ == NULL || this->vals_->empty())
    {
      // No value types to determine.
      if (type->array_type() != NULL
	  && type->array_type()->length() != NULL
	  && type->array_type()->length()->is_nil_expression())
	this->resolve_array_length(type);
      return;
    }

  if (type->struct_type() != NULL && this->has_keys_)
    {
      // Rewrite the value list by removing the struct field keys.  We
      // do this now rather than during lowering because handling
      // struct keys is painful.  We don't need to do this for
      // slice/array/map literals, because it's easy to determine
      // their types with or without the keys.
      if (!this->resolve_struct_keys(gogo, type))
	{
	  this->set_is_error();
	  delete this->vals_;
	  this->vals_ = NULL;
	  this->has_keys_ = false;
	  return;
	}
    }

  if (type->struct_type() != NULL)
    {
      const Struct_field_list* fields = type->struct_type()->fields();
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
	      (*pv)->determine_type(gogo, &subcontext);
	    }
	}
      // Extra values are an error we will report in the check_types
      // pass; we still want to determine the type to avoid knockon
      // errors.
      for (; pv != this->vals_->end(); ++pv)
	(*pv)->determine_type_no_context(gogo);
    }
  else if (type->array_type() != NULL)
    {
      Array_type* at = type->array_type();
      Type_context intcontext(Type::lookup_integer_type("int"), false);
      Type_context subcontext(at->element_type(), false);
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv)
	{
	  if (this->has_keys_)
	    {
	      if (*pv != NULL)
		(*pv)->determine_type(gogo, &intcontext);
	      ++pv;
	      if (pv == this->vals_->end())
		break;
	    }

	  if (*pv != NULL)
	    (*pv)->determine_type(gogo, &subcontext);
	}

      if (at->length() != NULL && at->length()->is_nil_expression())
	this->resolve_array_length(type);
    }
  else if (type->map_type() != NULL)
    {
      if (!this->has_keys_)
	{
	  this->report_error(_("map composite literal must have keys"));
	  return;
	}

      Map_type* mt = type->map_type();
      Type_context key_context(mt->key_type(), false);
      Type_context val_context(mt->val_type(), false);
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv)
	{
	  if (*pv != NULL)
	    (*pv)->determine_type(gogo, &key_context);
	  ++pv;
	  if (*pv != NULL)
	    (*pv)->determine_type(gogo, &val_context);
	}
    }
  else
    {
      // We'll report this as an error in the check_types pass.
      // Determine types to avoid knockon errors.
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv)
	{
	  if (*pv != NULL)
	    (*pv)->determine_type_no_context(gogo);
	}
    }
}

Type*
Composite_literal_expression::do_type()
{
  if (this->is_error_expression())
    return Type::make_error_type();
  go_assert(this->depth_ == 0);
  return this->type_;
}

// Resolve the field keys of a struct composite literal.

bool
Composite_literal_expression::resolve_struct_keys(Gogo* gogo, Type* type)
{
  Struct_type* st = type->struct_type();
  size_t field_count = st->field_count();
  std::vector<Expression*> vals(field_count);
  std::vector<unsigned long>* traverse_order = new(std::vector<unsigned long>);
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
	  go_error_at(val->location(),
		      "mixture of field and value initializers");
	  return false;
	}

      bool bad_key = false;
      std::string name;
      const Named_object* no = NULL;
      switch (name_expr->classification())
	{
	case EXPRESSION_COMPOSITE_LITERAL_KEY:
	  name =
	    static_cast<Composite_literal_key_expression*>(name_expr)->name();
	  break;

	case EXPRESSION_UNKNOWN_REFERENCE:
	  name = name_expr->unknown_expression()->name();
	  if (type->named_type() != NULL)
	    {
	      // If the named object found for this field name comes from a
	      // different package than the struct it is a part of, do not count
	      // this incorrect lookup as a usage of the object's package.
	      no = name_expr->unknown_expression()->named_object();
	      if (no->package() != NULL
		  && (no->package()
		      != type->named_type()->named_object()->package()))
		no->package()->forget_usage(name_expr);
	    }
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

	case EXPRESSION_ENCLOSED_VAR_REFERENCE:
	  no = name_expr->enclosed_var_expression()->variable();
	  break;

	case EXPRESSION_FUNC_REFERENCE:
	  no = name_expr->func_expression()->named_object();
	  break;

	default:
	  bad_key = true;
	  break;
	}
      if (bad_key)
	{
	  go_error_at(name_expr->location(), "expected struct field name");
	  return false;
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
	  go_error_at(name_expr->location(), "unknown field %qs in %qs",
		      Gogo::message_name(name).c_str(),
		      (type->named_type() != NULL
		       ? type->named_type()->message_name().c_str()
		       : "unnamed struct"));
	  return false;
	}
      if (vals[index] != NULL)
	{
	  go_error_at(name_expr->location(),
		      "duplicate value for field %qs in %qs",
		      Gogo::message_name(name).c_str(),
		      (type->named_type() != NULL
		       ? type->named_type()->message_name().c_str()
		       : "unnamed struct"));
	  return false;
	}

      vals[index] = val;
      traverse_order->push_back(static_cast<unsigned long>(index));
    }

  if (!this->all_are_names_)
    {
      // This is a weird case like bug462 in the testsuite.
      if (external_expr == NULL)
	go_error_at(this->location(), "unknown field in %qs literal",
                    (type->named_type() != NULL
                     ? type->named_type()->message_name().c_str()
                     : "unnamed struct"));
      else
	go_error_at(external_expr->location(), "unknown field %qs in %qs",
                    external_no->message_name().c_str(),
                    (type->named_type() != NULL
                     ? type->named_type()->message_name().c_str()
                     : "unnamed struct"));
      return false;
    }

  Expression_list* list = new Expression_list;
  list->reserve(field_count);
  for (size_t i = 0; i < field_count; ++i)
    list->push_back(vals[i]);

  this->vals_ = list;
  this->traverse_order_ = traverse_order;
  this->has_keys_ = false;

  return true;
}

// Handle [...]{...}

void
Composite_literal_expression::resolve_array_length(Type* type)
{
  size_t size;
  if (this->vals_ == NULL || this->vals_->empty())
    size = 0;
  else if (!this->has_keys_)
    size = this->vals_->size();
  else
    {
      unsigned long index = 0;
      size = 0;
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   pv += 2)
	{
	  Expression* index_expr = *pv;
	  if (index_expr != NULL)
	    {
	      Numeric_constant nc;
	      unsigned long iv;
	      if (!index_expr->numeric_constant_value(&nc)
		  || nc.to_unsigned_long(&iv) != Numeric_constant::NC_UL_VALID)
		{
		  // We will report an error when lowering.
		  break;
		}
	      index = iv;
	    }

	  if (index >= size)
	    size = index + 1;

	  ++index;
	}
    }

  Expression* elen = Expression::make_integer_ul(size, NULL, this->location());
  this->type_ = Type::make_array_type(type->array_type()->element_type(),
				      elen);
}

void
Composite_literal_expression::do_check_types(Gogo* gogo)
{
  if (this->is_error_expression() || this->type_->is_error())
    {
      go_assert(saw_errors());
      return;
    }
  go_assert(this->depth_ == 0);

  Type* type = this->type_->deref();
  if (type->struct_type() != NULL)
    {
      go_assert(!this->has_keys_);
      if (!Struct_construction_expression::check_value_types(gogo, type,
							     this->vals_,
							     this->location()))
	this->set_is_error();
    }
  else if (type->array_type() != NULL)
    {
      Type* element_type = type->array_type()->element_type();
      if (element_type->is_error())
	{
	  go_assert(saw_errors());
	  return;
	}
      if (this->vals_ == NULL || this->vals_->empty())
	return;
      int i = 0;
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv, ++i)
	{
	  if (this->has_keys_)
	    {
	      // We check the key type in the lowering pass.
	      ++pv;
	      if (pv == this->vals_->end())
		break;
	    }
	  if (*pv != NULL
	      && !Type::are_assignable(element_type, (*pv)->type(), NULL))
	    {
	      go_error_at((*pv)->location(),
			  ("incompatible type for element %d "
			   "in composite literal"),
			  i + 1);
	      this->set_is_error();
	    }
	}
    }
  else if (type->map_type() != NULL)
    {
      if (this->vals_ == NULL || this->vals_->empty())
	return;
      if (!this->has_keys_)
	{
	  this->report_error(_("map composite literal must have keys"));
	  return;
	}
      int i = 0;
      Map_type* mt = type->map_type();
      Type* key_type = mt->key_type();
      Type* val_type = mt->val_type();
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv, ++i)
	{
	  if (*pv != NULL
	      && !Type::are_assignable(key_type, (*pv)->type(), NULL))
	    {
	      go_error_at((*pv)->location(),
			  ("incompatible type for element %d key "
			   "in map construction"),
			  i + 1);
	      this->set_is_error();
	    }
	  ++pv;
	  if (!Type::are_assignable(val_type, (*pv)->type(), NULL))
	    {
	      go_error_at((*pv)->location(),
			  ("incompatible type for element %d value "
			   "in map construction"),
			  i + 1);
	      this->set_is_error();
	    }
	}
    }
  else
    {
      this->report_error(_("expected struct, slice, array, or map type "
			   "for composite literal"));
    }
}

// Lower a generic composite literal into a specific version based on
// the type.

Expression*
Composite_literal_expression::do_lower(Gogo* gogo, Named_object* function,
				       Statement_inserter* inserter)
{
  if (this->is_error_expression() || this->type_->is_error())
    return Expression::make_error(this->location());
  go_assert(this->depth_ == 0);

  Type* type = this->type_;
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
    ret = this->lower_struct(type);
  else if (type->array_type() != NULL)
    ret = this->lower_array(type);
  else if (type->map_type() != NULL)
    ret = this->lower_map(gogo, function, inserter, type);
  else
    go_unreachable();

  if (is_pointer)
    ret = Expression::make_heap_expression(ret, this->location());

  return ret;
}

// Lower a struct composite literal.

Expression*
Composite_literal_expression::lower_struct(Type* type)
{
  go_assert(!this->has_keys_);
  Struct_construction_expression* ret =
    new Struct_construction_expression(type, this->vals_, this->location());
  ret->set_traverse_order(this->traverse_order_);
  return ret;
}

// Index/value/traversal-order triple.

struct IVT_triple {
  unsigned long index;
  unsigned long traversal_order;
  Expression* expr;
  IVT_triple(unsigned long i, unsigned long to, Expression *e)
      : index(i), traversal_order(to), expr(e) { }
  bool operator<(const IVT_triple& other) const
  { return this->index < other.index; }
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
	  if (std::find(indexes->begin(), indexes->end(), index)
	      != indexes->end())
	    {
	      go_error_at(val->location(),
			  "duplicate value for index %lu", index);
	      return Expression::make_error(location);
	    }
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
	      go_error_at(index_expr->location(),
                          "index expression is not integer constant");
	      return Expression::make_error(location);
	    }

	  switch (nc.to_unsigned_long(&index))
	    {
	    case Numeric_constant::NC_UL_VALID:
	      break;
	    case Numeric_constant::NC_UL_NOTINT:
	      go_error_at(index_expr->location(),
                          "index expression is not integer constant");
	      return Expression::make_error(location);
	    case Numeric_constant::NC_UL_NEGATIVE:
	      go_error_at(index_expr->location(),
                          "index expression is negative");
	      return Expression::make_error(location);
	    case Numeric_constant::NC_UL_BIG:
	      go_error_at(index_expr->location(), "index value overflow");
	      return Expression::make_error(location);
	    default:
	      go_unreachable();
	    }

	  Named_type* ntype = Type::lookup_integer_type("int");
	  Integer_type* inttype = ntype->integer_type();
	  if (sizeof(index) >= static_cast<size_t>(inttype->bits() / 8)
	      && index >> (inttype->bits() - 1) != 0)
	    {
	      go_error_at(index_expr->location(), "index value overflow");
	      return Expression::make_error(location);
	    }

	  if (std::find(indexes->begin(), indexes->end(), index)
	      != indexes->end())
	    {
	      go_error_at(index_expr->location(),
                          "duplicate value for index %lu",
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

  std::vector<unsigned long>* traverse_order = NULL;
  if (indexes_out_of_order)
    {
      typedef std::vector<IVT_triple> V;

      V v;
      v.reserve(indexes->size());
      std::vector<unsigned long>::const_iterator pi = indexes->begin();
      unsigned long torder = 0;
      for (Expression_list::const_iterator pe = vals->begin();
	   pe != vals->end();
	   ++pe, ++pi, ++torder)
	v.push_back(IVT_triple(*pi, torder, *pe));

      std::sort(v.begin(), v.end());

      delete indexes;
      delete vals;

      indexes = new std::vector<unsigned long>();
      indexes->reserve(v.size());
      vals = new Expression_list();
      vals->reserve(v.size());
      traverse_order = new std::vector<unsigned long>();
      traverse_order->reserve(v.size());

      for (V::const_iterator pv = v.begin(); pv != v.end(); ++pv)
	{
	  indexes->push_back(pv->index);
	  vals->push_back(pv->expr);
	  traverse_order->push_back(pv->traversal_order);
	}
    }

  Expression* ret = this->make_array(type, indexes, vals);
  Array_construction_expression* ace = ret->array_literal();
  if (ace != NULL && traverse_order != NULL)
    ace->set_traverse_order(traverse_order);
  return ret;
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

  if (at->length() != NULL
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
		  go_error_at(location,
                              "too many elements in composite literal");
		  return Expression::make_error(location);
		}
	    }
	  else
	    {
	      unsigned long max = indexes->back();
	      if (max >= val)
		{
		  go_error_at(location,
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
  Unordered_map(unsigned int, std::vector<Expression*>) st;
  Unordered_map(unsigned int, std::vector<Expression*>) nt;
  bool saw_false = false;
  bool saw_true = false;
  if (this->vals_ != NULL)
    {
      go_assert(this->has_keys_);

      for (Expression_list::iterator p = this->vals_->begin();
	   p != this->vals_->end();
	   p += 2)
	{
	  if (*p == NULL)
	    {
	      ++p;
	      go_error_at((*p)->location(),
                          ("map composite literal must "
                           "have keys for every value"));
	      return Expression::make_error(location);
	    }
	  // Make sure we have lowered the key; it may not have been
	  // lowered in order to handle keys for struct composite
	  // literals.  Lower it now to get the right error message.
	  if ((*p)->unknown_expression() != NULL)
	    {
	      gogo->lower_expression(function, inserter, &*p);
	      go_assert((*p)->is_error_expression());
	      return Expression::make_error(location);
	    }
	  // Check if there are duplicate constant keys.
	  if (!(*p)->is_constant())
	    continue;
	  std::string sval;
	  Numeric_constant nval;
	  bool bval;
	  if ((*p)->string_constant_value(&sval)) // Check string keys.
	    {
	      unsigned int h = Gogo::hash_string(sval, 0);
	      // Search the index h in the hash map.
	      Unordered_map(unsigned int, std::vector<Expression*>)::iterator mit;
	      mit = st.find(h);
	      if (mit == st.end())
		{
		  // No duplicate since h is a new index.
		  // Create a new vector indexed by h and add it to the hash map.
		  std::vector<Expression*> l;
		  l.push_back(*p);
		  std::pair<unsigned int, std::vector<Expression*> > val(h, l);
		  st.insert(val);
		}
	      else
		{
		  // Do further check since index h already exists.
		  for (std::vector<Expression*>::iterator lit =
			   mit->second.begin();
		       lit != mit->second.end();
		       lit++)
		    {
		      std::string s;
		      bool ok = (*lit)->string_constant_value(&s);
		      go_assert(ok);
		      if (s == sval)
			{
			  go_error_at((*p)->location(), ("duplicate key "
				      "in map literal"));
			  return Expression::make_error(location);
			}
		    }
		  // Add this new string key to the vector indexed by h.
		  mit->second.push_back(*p);
		}
	    }
	  else if ((*p)->numeric_constant_value(&nval))
	    {
	      // Check numeric keys.
	      unsigned int h = nval.hash(0);
	      Unordered_map(unsigned int, std::vector<Expression*>)::iterator mit;
	      mit = nt.find(h);
	      if (mit == nt.end())
		{
		  // No duplicate since h is a new code.
		  // Create a new vector indexed by h and add it to the hash map.
		  std::vector<Expression*> l;
		  l.push_back(*p);
		  std::pair<unsigned int, std::vector<Expression*> > val(h, l);
		  nt.insert(val);
		}
	      else
		{
		  // Do further check since h already exists.
		  for (std::vector<Expression*>::iterator lit =
			   mit->second.begin();
		       lit != mit->second.end();
		       lit++)
		    {
		      Numeric_constant rval;
		      bool ok = (*lit)->numeric_constant_value(&rval);
		      go_assert(ok);
		      if (nval.equals(rval))
			{
			  go_error_at((*p)->location(),
				      "duplicate key in map literal");
			  return Expression::make_error(location);
			}
		    }
		  // Add this new numeric key to the vector indexed by h.
		  mit->second.push_back(*p);
		}
	    }
	  else if ((*p)->boolean_constant_value(&bval))
	    {
	      if ((bval && saw_true) || (!bval && saw_false))
		{
		  go_error_at((*p)->location(),
			      "duplicate key in map literal");
		  return Expression::make_error(location);
		}
	      if (bval)
		saw_true = true;
	      else
		saw_false = true;
	    }
	}
    }

  return new Map_construction_expression(type, this->vals_, location);
}

// Copy.

Expression*
Composite_literal_expression::do_copy()
{
  Composite_literal_expression* ret =
    new Composite_literal_expression(this->type_->copy_expressions(),
				     this->depth_, this->has_keys_,
				     (this->vals_ == NULL
				      ? NULL
				      : this->vals_->copy()),
				     this->all_are_names_,
				     this->location());
  ret->key_path_ = this->key_path_;
  return ret;
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
    case EXPRESSION_ENCLOSED_VAR_REFERENCE:
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

// Return true if multiple evaluations are OK.

bool
Expression::is_multi_eval_safe()
{
  switch (this->classification_)
    {
    case EXPRESSION_VAR_REFERENCE:
      {
	// A variable is a simple reference if not stored in the heap.
	const Named_object* no = this->var_expression()->named_object();
	if (no->is_variable())
	  return !no->var_value()->is_in_heap();
	else if (no->is_result_variable())
	  return !no->result_var_value()->is_in_heap();
	else
	  go_unreachable();
      }

    case EXPRESSION_TEMPORARY_REFERENCE:
      return true;

    default:
      break;
    }

  if (!this->is_constant())
    return false;

  // Only numeric and boolean constants are really multi-evaluation
  // safe.  We don't want multiple copies of string constants.
  Type* type = this->type();
  return type->is_numeric_type() || type->is_boolean_type();
}

const Named_object*
Expression::named_constant() const
{
  if (this->classification() != EXPRESSION_CONST_REFERENCE)
    return NULL;
  const Const_expression* ce = static_cast<const Const_expression*>(this);
  return ce->named_object();
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
  if (this->expr_->is_error_expression()
      || this->expr_->type()->is_error_type())
    {
      go_assert(saw_errors());
      return Expression::make_error(this->location());
    }

  if (!this->expr_->is_multi_eval_safe())
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
		go_error_at(this->location(),
                            ("impossible type assertion: "
                             "type does not implement interface (%s)"),
                            reason.c_str());
	    }
	  this->set_is_error();
	}
    }
}

// Copy.

Expression*
Type_guard_expression::do_copy()
{
  return new Type_guard_expression(this->expr_->copy(),
				   this->type_->copy_expressions(),
				   this->location());
}

// Return the backend representation for a type guard expression.

Bexpression*
Type_guard_expression::do_get_backend(Translate_context* context)
{
  Expression* conversion;
  if (this->type_->interface_type() != NULL)
    conversion = Expression::convert_interface_to_interface(context->gogo(),
							    this->type_,
							    this->expr_,
							    true,
							    this->location());
  else
    conversion = Expression::convert_for_assignment(context->gogo(),
						    this->type_,
						    this->expr_,
						    this->location());

  Gogo* gogo = context->gogo();
  Btype* bt = this->type_->get_backend(gogo);
  Bexpression* bexpr = conversion->get_backend(context);
  return gogo->backend()->convert_expression(bt, bexpr, this->location());
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

// Return the type of the expression stored on the heap.

Type*
Heap_expression::do_type()
{ return Type::make_pointer_type(this->expr_->type()); }

// Return the backend representation for allocating an expression on the heap.

Bexpression*
Heap_expression::do_get_backend(Translate_context* context)
{
  Type* etype = this->expr_->type();
  if (this->expr_->is_error_expression() || etype->is_error())
    return context->backend()->error_expression();

  Location loc = this->location();
  Gogo* gogo = context->gogo();
  Btype* btype = this->type()->get_backend(gogo);

  Expression* alloc = Expression::make_allocation(etype, loc);
  if (this->allocate_on_stack_)
    alloc->allocation_expression()->set_allocate_on_stack();
  Bexpression* space = alloc->get_backend(context);

  Bstatement* decl;
  Named_object* fn = context->function();
  go_assert(fn != NULL);
  Bfunction* fndecl = fn->func_value()->get_or_make_decl(gogo, fn);
  Bvariable* space_temp =
    gogo->backend()->temporary_variable(fndecl, context->bblock(), btype,
					space,
					Backend::variable_address_is_taken,
					loc, &decl);
  Btype* expr_btype = etype->get_backend(gogo);

  Bexpression* bexpr = this->expr_->get_backend(context);

  // If this assignment needs a write barrier, call typedmemmove.  We
  // don't do this in the write barrier pass because in some cases
  // backend conversion can introduce new Heap_expression values.
  Bstatement* assn;
  if (!etype->has_pointer() || this->allocate_on_stack_)
    {
      space = gogo->backend()->var_expression(space_temp, loc);
      Bexpression* ref =
	gogo->backend()->indirect_expression(expr_btype, space, true, loc);
      assn = gogo->backend()->assignment_statement(fndecl, ref, bexpr, loc);
    }
  else
    {
      Bstatement* edecl;
      Bvariable* btemp =
	gogo->backend()->temporary_variable(fndecl, context->bblock(),
					    expr_btype, bexpr,
					    Backend::variable_address_is_taken,
					    loc, &edecl);
      Bexpression* btempref = gogo->backend()->var_expression(btemp,
							      loc);
      space = gogo->backend()->var_expression(space_temp, loc);
      Type* etype_ptr = Type::make_pointer_type(etype);
      Expression* elhs = Expression::make_backend(space, etype_ptr, loc);
      Expression* erhs;
      Expression* call;
      if (etype->is_direct_iface_type())
        {
          // Single pointer.
          Type* uintptr_type = Type::lookup_integer_type("uintptr");
          erhs = Expression::make_backend(btempref, etype, loc);
          erhs = Expression::unpack_direct_iface(erhs, loc);
          erhs = Expression::make_unsafe_cast(uintptr_type, erhs, loc);
          call = Runtime::make_call(gogo, Runtime::GCWRITEBARRIER, loc, 2,
                                    elhs, erhs);
        }
      else
        {
          Expression* td = Expression::make_type_descriptor(etype, loc);
          Bexpression* addr =
            gogo->backend()->address_expression(btempref, loc);
          erhs = Expression::make_backend(addr, etype_ptr, loc);
          call = Runtime::make_call(gogo, Runtime::TYPEDMEMMOVE, loc, 3,
                                    td, elhs, erhs);
        }
      Statement* cs = Statement::make_statement(call, false);

      space = gogo->backend()->var_expression(space_temp, loc);
      Bexpression* ref =
        gogo->backend()->indirect_expression(expr_btype, space, true, loc);
      Expression* eref = Expression::make_backend(ref, etype, loc);
      btempref = gogo->backend()->var_expression(btemp, loc);
      erhs = Expression::make_backend(btempref, etype, loc);
      Statement* as = Statement::make_assignment(eref, erhs, loc);

      as = gogo->check_write_barrier(context->block(), as, cs);
      Bstatement* s = as->get_backend(context);

      assn = gogo->backend()->compound_statement(edecl, s);
    }
  decl = gogo->backend()->compound_statement(decl, assn);
  space = gogo->backend()->var_expression(space_temp, loc);
  return gogo->backend()->compound_expression(decl, space, loc);
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
  if (this->is_error_expression())
    return Type::make_error_type();
  Channel_type* channel_type = this->channel_->type()->channel_type();
  if (channel_type == NULL)
    {
      this->report_error(_("expected channel"));
      return Type::make_error_type();
    }
  return channel_type->element_type();
}

// Check types for a receive expression.

void
Receive_expression::do_check_types(Gogo*)
{
  Type* type = this->channel_->type();
  if (type->is_error())
    {
      go_assert(saw_errors());
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
  else if (this->channel_->is_error_expression())
   {
     go_assert(saw_errors());
     return Expression::make_error(this->location());
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

// Get the backend representation for a receive expression.

Bexpression*
Receive_expression::do_get_backend(Translate_context* context)
{
  Location loc = this->location();

  Channel_type* channel_type = this->channel_->type()->channel_type();
  if (channel_type == NULL)
    {
      go_assert(this->channel_->type()->is_error());
      return context->backend()->error_expression();
    }

  Gogo* gogo = context->gogo();
  Expression* recv_ref =
    Expression::make_temporary_reference(this->temp_receiver_, loc);
  Expression* recv_addr =
    Expression::make_temporary_reference(this->temp_receiver_, loc);
  recv_addr = Expression::make_unary(OPERATOR_AND, recv_addr, loc);
  Expression* recv = Runtime::make_call(gogo, Runtime::CHANRECV1, loc, 2,
					this->channel_, recv_addr);
  Expression* ret = Expression::make_compound(recv, recv_ref, loc);
  ret->determine_type_no_context(gogo);
  return ret->get_backend(context);
}

// Export a receive expression.

void
Receive_expression::do_export(Export_function_body* efb) const
{
  efb->write_c_string("<-");
  this->channel_->export_expression(efb);
}

// Dump ast representation for a receive expression.

void
Receive_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << " <- " ;
  ast_dump_context->dump_expression(channel_);
}

// Import a receive expression.

Expression*
Receive_expression::do_import(Import_expression* imp, Location loc)
{
  imp->require_c_string("<-");
  Expression* expr = Expression::import_expression(imp, loc);
  return Expression::make_receive(expr, loc);
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
  int
  do_traverse(Traverse*);

  Type*
  do_type()
  { return Type::make_type_descriptor_ptr_type(); }

  bool
  do_is_static_initializer() const
  { return true; }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context* context)
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

int
Type_descriptor_expression::do_traverse(Traverse* traverse)
{
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

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

// An expression which evaluates to a pointer to the Garbage Collection symbol
// of a type.

class GC_symbol_expression : public Expression
{
 public:
  GC_symbol_expression(Type* type)
    : Expression(EXPRESSION_GC_SYMBOL, Linemap::predeclared_location()),
      type_(type)
  {}

 protected:
  Type*
  do_type()
  { return Type::make_pointer_type(Type::lookup_integer_type("uint8")); }

  bool
  do_is_static_initializer() const
  { return true; }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context* context)
  { return this->type_->gc_symbol_pointer(context->gogo()); }

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type which this gc symbol describes.
  Type* type_;
};

// Dump ast representation for a gc symbol expression.

void
GC_symbol_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "gcdata(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ")";
}

// Make a gc symbol expression.

Expression*
Expression::make_gc_symbol(Type* type)
{
  return new GC_symbol_expression(type);
}

// An expression that evaluates to a pointer to a symbol holding the
// ptrmask data of a type.

class Ptrmask_symbol_expression : public Expression
{
 public:
  Ptrmask_symbol_expression(Type* type)
    : Expression(EXPRESSION_PTRMASK_SYMBOL, Linemap::predeclared_location()),
      type_(type)
  {}

 protected:
  Type*
  do_type()
  { return Type::make_pointer_type(Type::lookup_integer_type("uint8")); }

  bool
  do_is_static_initializer() const
  { return true; }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type that this ptrmask symbol describes.
  Type* type_;
};

// Return the ptrmask variable.

Bexpression*
Ptrmask_symbol_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  // If this type does not need a gcprog, then we can use the standard
  // GC symbol.
  int64_t ptrsize, ptrdata;
  if (!this->type_->needs_gcprog(gogo, &ptrsize, &ptrdata))
    return this->type_->gc_symbol_pointer(gogo);

  // Otherwise we have to build a ptrmask variable, and return a
  // pointer to it.

  Bvariable* bvar = this->type_->gc_ptrmask_var(gogo, ptrsize, ptrdata);
  Location bloc = Linemap::predeclared_location();
  Bexpression* bref = gogo->backend()->var_expression(bvar, bloc);
  Bexpression* baddr = gogo->backend()->address_expression(bref, bloc);

  Type* uint8_type = Type::lookup_integer_type("uint8");
  Type* pointer_uint8_type = Type::make_pointer_type(uint8_type);
  Btype* ubtype = pointer_uint8_type->get_backend(gogo);
  return gogo->backend()->convert_expression(ubtype, baddr, bloc);
}

// Dump AST for a ptrmask symbol expression.

void
Ptrmask_symbol_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "ptrmask(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ")";
}

// Make a ptrmask symbol expression.

Expression*
Expression::make_ptrmask_symbol(Type* type)
{
  return new Ptrmask_symbol_expression(type);
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
  do_is_static_initializer() const
  { return true; }

  Type*
  do_type();

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context* context);

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
    case TYPE_INFO_BACKEND_PTRDATA:
    case TYPE_INFO_DESCRIPTOR_PTRDATA:
      return Type::lookup_integer_type("uintptr");
    case TYPE_INFO_ALIGNMENT:
    case TYPE_INFO_FIELD_ALIGNMENT:
      return Type::lookup_integer_type("uint8");
    default:
      go_unreachable();
    }
}

// Return the backend representation for type information.

Bexpression*
Type_info_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  bool ok = true;
  int64_t val;
  switch (this->type_info_)
    {
    case TYPE_INFO_SIZE:
      ok = this->type_->backend_type_size(gogo, &val);
      break;
    case TYPE_INFO_ALIGNMENT:
      ok = this->type_->backend_type_align(gogo, &val);
      break;
    case TYPE_INFO_FIELD_ALIGNMENT:
      ok = this->type_->backend_type_field_align(gogo, &val);
      break;
    case TYPE_INFO_BACKEND_PTRDATA:
      ok = this->type_->backend_type_ptrdata(gogo, &val);
      break;
    case TYPE_INFO_DESCRIPTOR_PTRDATA:
      ok = this->type_->descriptor_ptrdata(gogo, &val);
      break;
    default:
      go_unreachable();
    }
  if (!ok)
    {
      go_assert(saw_errors());
      return gogo->backend()->error_expression();
    }
  Expression* e = Expression::make_integer_int64(val, this->type(),
						 this->location());
  return e->get_backend(context);
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
    : this->type_info_ == TYPE_INFO_SIZE ? "size"
    : this->type_info_ == TYPE_INFO_BACKEND_PTRDATA ? "backend_ptrdata"
    : this->type_info_ == TYPE_INFO_DESCRIPTOR_PTRDATA ? "descriptor_ptrdata"
    : "unknown");
  ast_dump_context->ostream() << ")";
}

// Make a type info expression.

Expression*
Expression::make_type_info(Type* type, Type_info type_info)
{
  return new Type_info_expression(type, type_info);
}

// Slice_info_expression.

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

// Return the backend information for slice information.

Bexpression*
Slice_info_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Bexpression* bslice = this->slice_->get_backend(context);
  switch (this->slice_info_)
    {
    case SLICE_INFO_VALUE_POINTER:
    case SLICE_INFO_LENGTH:
    case SLICE_INFO_CAPACITY:
      return gogo->backend()->struct_field_expression(bslice, this->slice_info_,
						      this->location());
      break;
    default:
      go_unreachable();
    }
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

// Class Slice_value_expression.

int
Slice_value_expression::do_traverse(Traverse* traverse)
{
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->valmem_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->len_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->cap_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Determine type of a slice value.

void
Slice_value_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->valmem_->determine_type_no_context(gogo);
  this->len_->determine_type_no_context(gogo);
  this->cap_->determine_type_no_context(gogo);
}

Expression*
Slice_value_expression::do_copy()
{
  return new Slice_value_expression(this->type_->copy_expressions(),
				    this->valmem_->copy(),
				    this->len_->copy(), this->cap_->copy(),
				    this->location());
}

Bexpression*
Slice_value_expression::do_get_backend(Translate_context* context)
{
  std::vector<Bexpression*> vals(3);
  vals[0] = this->valmem_->get_backend(context);
  vals[1] = this->len_->get_backend(context);
  vals[2] = this->cap_->get_backend(context);

  Gogo* gogo = context->gogo();
  Btype* btype = this->type_->get_backend(gogo);
  return gogo->backend()->constructor_expression(btype, vals, this->location());
}

void
Slice_value_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "slicevalue(";
  ast_dump_context->ostream() << "values: ";
  this->valmem_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ", length: ";
  this->len_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ", capacity: ";
  this->cap_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ")";
}

Expression*
Expression::make_slice_value(Type* at, Expression* valmem, Expression* len,
                             Expression* cap, Location location)
{
  go_assert(at->is_slice_type());
  go_assert(valmem->is_nil_expression()
	    || (at->array_type()->element_type()
		== valmem->type()->points_to()));
  return new Slice_value_expression(at, valmem, len, cap, location);
}

// Look through the expression of a Slice_value_expression's valmem to
// find an call to makeslice.  If found, return the call expression and
// the containing temporary statement (if any).

std::pair<Call_expression*, Temporary_statement*>
Expression::find_makeslice_call(Expression* expr)
{
  Unsafe_type_conversion_expression* utce =
    expr->unsafe_conversion_expression();
  if (utce != NULL)
    expr = utce->expr();

  Slice_value_expression* sve = expr->slice_value_expression();
  if (sve == NULL)
    return std::make_pair<Call_expression*, Temporary_statement*>(NULL, NULL);
  expr = sve->valmem();

  utce = expr->unsafe_conversion_expression();
  if (utce != NULL)
    expr = utce->expr();

  Temporary_reference_expression* tre = expr->temporary_reference_expression();
  Temporary_statement* ts = (tre != NULL ? tre->statement() : NULL);
  if (ts != NULL && ts->init() != NULL && !ts->assigned()
      && !ts->is_address_taken())
    expr = ts->init();

  Call_expression* call = expr->call_expression();
  if (call == NULL)
    return std::make_pair<Call_expression*, Temporary_statement*>(NULL, NULL);

  Func_expression* fe = call->fn()->func_expression();
  if (fe != NULL
      && fe->runtime_code() == Runtime::MAKESLICE)
    return std::make_pair(call, ts);

  return std::make_pair<Call_expression*, Temporary_statement*>(NULL, NULL);
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
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return new Interface_info_expression(this->iface_->copy(),
                                         this->iface_info_, this->location());
  }

  Bexpression*
  do_get_backend(Translate_context* context);

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
        typedef Unordered_map(Interface_type*, Type*) Hashtable;
        static Hashtable result_types;

        Interface_type* itype = this->iface_->type()->interface_type();

        Hashtable::const_iterator pr = result_types.find(itype);
        if (pr != result_types.end())
          return pr->second;

        Type* pdt = Type::make_type_descriptor_ptr_type();
        if (itype->is_empty())
          {
            result_types[itype] = pdt;
            return pdt;
          }

        Location loc = this->location();
        Struct_field_list* sfl = new Struct_field_list();
        sfl->push_back(
            Struct_field(Typed_identifier("__type_descriptor", pdt, loc)));

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

	Struct_type* st = Type::make_struct_type(sfl, loc);
	st->set_is_struct_incomparable();
	Pointer_type *pt = Type::make_pointer_type(st);
        result_types[itype] = pt;
        return pt;
      }
    case INTERFACE_INFO_OBJECT:
      return Type::make_pointer_type(Type::make_void_type());
    default:
      go_unreachable();
    }
}

// Return the backend representation for interface information.

Bexpression*
Interface_info_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Bexpression* biface = this->iface_->get_backend(context);
  switch (this->iface_info_)
    {
    case INTERFACE_INFO_METHODS:
    case INTERFACE_INFO_OBJECT:
      return gogo->backend()->struct_field_expression(biface, this->iface_info_,
						      this->location());
      break;
    default:
      go_unreachable();
    }
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
  do_determine_type(Gogo*, const Type_context*);

  Expression*
  do_copy()
  {
    return new Interface_value_expression(this->type_->copy_expressions(),
                                          this->first_field_->copy(),
                                          this->obj_->copy(), this->location());
  }

  Bexpression*
  do_get_backend(Translate_context* context);

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

void
Interface_value_expression::do_determine_type(Gogo* gogo, const Type_context*)
{
  this->first_field_->determine_type_no_context(gogo);
  this->obj_->determine_type_no_context(gogo);
}

Bexpression*
Interface_value_expression::do_get_backend(Translate_context* context)
{
  std::vector<Bexpression*> vals(2);
  vals[0] = this->first_field_->get_backend(context);
  vals[1] = this->obj_->get_backend(context);

  Gogo* gogo = context->gogo();
  Btype* btype = this->type_->get_backend(gogo);
  return gogo->backend()->constructor_expression(btype, vals, this->location());
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
  do_is_static_initializer() const
  { return true; }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  {
    Interface_type* itype = this->itype_->copy_expressions()->interface_type();
    return new Interface_mtable_expression(itype,
					   this->type_->copy_expressions(),
                                           this->is_pointer_, this->location());
  }

  bool
  do_is_addressable() const
  { return true; }

  Bexpression*
  do_get_backend(Translate_context* context);

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
  Type* unsafe_ptr_type = Type::make_pointer_type(Type::make_void_type());
  for (Typed_identifier_list::const_iterator p = interface_methods->begin();
       p != interface_methods->end();
       ++p)
    {
      // We want C function pointers here, not func descriptors; model
      // using void* pointers.
      Typed_identifier method(p->name(), unsafe_ptr_type, p->location());
      sfl->push_back(Struct_field(method));
    }
  Struct_type* st = Type::make_struct_type(sfl, this->location());
  st->set_is_struct_incomparable();
  this->method_table_type_ = st;
  return this->method_table_type_;
}

Bexpression*
Interface_mtable_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location loc = Linemap::predeclared_location();
  if (this->bvar_ != NULL)
    return gogo->backend()->var_expression(this->bvar_, this->location());

  const Typed_identifier_list* interface_methods = this->itype_->methods();
  go_assert(!interface_methods->empty());

  std::string mangled_name =
    gogo->interface_method_table_name(this->itype_, this->type_,
				      this->is_pointer_);

  // Set is_public if we are converting a named type to an interface
  // type that is defined in the same package as the named type, and
  // the interface has hidden methods.  In that case the interface
  // method table will be defined by the package that defines the
  // types.
  bool is_public = false;
  if (this->type_->named_type() != NULL
      && (this->type_->named_type()->named_object()->package()
	  == this->itype_->package()))
    {
      for (Typed_identifier_list::const_iterator p = interface_methods->begin();
	   p != interface_methods->end();
	   ++p)
	{
	  if (Gogo::is_hidden_name(p->name()))
	    {
	      is_public = true;
	      break;
	    }
	}
    }

  if (is_public
      && this->type_->named_type()->named_object()->package() != NULL)
    {
      // The interface conversion table is defined elsewhere.
      Btype* btype = this->type()->get_backend(gogo);
      this->bvar_ =
          gogo->backend()->immutable_struct_reference(mangled_name, "",
                                                      btype, loc);
      return gogo->backend()->var_expression(this->bvar_, this->location());
    }

  // The first element is the type descriptor.
  Type* td_type;
  if (!this->is_pointer_)
    td_type = this->type_;
  else
    td_type = Type::make_pointer_type(this->type_);

  std::vector<Backend::Btyped_identifier> bstructfields;

  // Build an interface method table for a type: a type descriptor followed by a
  // list of function pointers, one for each interface method.  This is used for
  // interfaces.
  Expression_list* svals = new Expression_list();
  Expression* tdescriptor = Expression::make_type_descriptor(td_type, loc);
  svals->push_back(tdescriptor);

  Btype* tdesc_btype = tdescriptor->type()->get_backend(gogo);
  Backend::Btyped_identifier btd("_type", tdesc_btype, loc);
  bstructfields.push_back(btd);

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

      // See the comment in Type::method_constructor.
      bool use_direct_iface_stub = false;
      if (m->is_value_method()
	  && this->is_pointer_
	  && this->type_->is_direct_iface_type())
	use_direct_iface_stub = true;
      if (!m->is_value_method()
	  && this->is_pointer_
	  && !this->type_->in_heap())
	use_direct_iface_stub = true;
      Named_object* no = (use_direct_iface_stub
			  ? m->iface_stub_object()
			  : m->named_object());

      go_assert(no->is_function() || no->is_function_declaration());

      Function_type* fcn_type = (no->is_function()
                                 ? no->func_value()->type()
                                 : no->func_declaration_value()->type());
      Btype* fcn_btype = fcn_type->get_backend_fntype(gogo);
      Backend::Btyped_identifier bmtype(p->name(), fcn_btype, loc);
      bstructfields.push_back(bmtype);

      svals->push_back(Expression::make_func_code_reference(no, loc));
    }

  Btype *btype = gogo->backend()->struct_type(bstructfields);
  std::vector<Bexpression*> ctor_bexprs;
  for (Expression_list::const_iterator pe = svals->begin();
       pe != svals->end();
       ++pe)
    {
      ctor_bexprs.push_back((*pe)->get_backend(context));
    }
  Bexpression* ctor =
      gogo->backend()->constructor_expression(btype, ctor_bexprs, loc);

  unsigned int flags = 0;
  if (!is_public)
    flags |= Backend::variable_is_hidden;
  this->bvar_ = gogo->backend()->immutable_struct(mangled_name, "", flags,
						  btype, loc);
  gogo->backend()->immutable_struct_set_init(this->bvar_, mangled_name, flags,
					     btype, loc, ctor);
  return gogo->backend()->var_expression(this->bvar_, loc);
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
  bool
  do_is_static_initializer() const
  { return true; }

  Type*
  do_type()
  { return Type::lookup_integer_type("uintptr"); }

  void
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  Bexpression*
  do_get_backend(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of the struct.
  Struct_type* type_;
  // The field.
  const Struct_field* field_;
};

// Return the backend representation for a struct field offset.

Bexpression*
Struct_field_offset_expression::do_get_backend(Translate_context* context)
{
  const Struct_field_list* fields = this->type_->fields();
  Struct_field_list::const_iterator p;
  unsigned i = 0;
  for (p = fields->begin();
       p != fields->end();
       ++p, ++i)
    if (&*p == this->field_)
      break;
  go_assert(&*p == this->field_);

  Gogo* gogo = context->gogo();
  Btype* btype = this->type_->get_backend(gogo);

  int64_t offset = gogo->backend()->type_field_offset(btype, i);
  Type* uptr_type = Type::lookup_integer_type("uintptr");
  Expression* ret =
    Expression::make_integer_int64(offset, uptr_type,
				   Linemap::predeclared_location());
  return ret->get_backend(context);
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
  do_determine_type(Gogo*, const Type_context*)
  { }

  Expression*
  do_copy()
  { return new Label_addr_expression(this->label_, this->location()); }

  Bexpression*
  do_get_backend(Translate_context* context)
  { return this->label_->get_addr(context, this->location()); }

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

// Class Conditional_expression.

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
  if (Type::are_identical(this->then_->type(), this->else_->type(),
			  Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
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
Conditional_expression::do_determine_type(Gogo* gogo,
					  const Type_context* context)
{
  this->cond_->determine_type_no_context(gogo);
  this->then_->determine_type(gogo, context);
  this->else_->determine_type(gogo, context);
}

// Get the backend representation of a conditional expression.

Bexpression*
Conditional_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Btype* result_btype = this->type()->get_backend(gogo);
  Bexpression* cond = this->cond_->get_backend(context);
  Bexpression* then = this->then_->get_backend(context);
  Bexpression* belse = this->else_->get_backend(context);
  Bfunction* bfn = context->function()->func_value()->get_decl();
  return gogo->backend()->conditional_expression(bfn, result_btype, cond, then,
						 belse, this->location());
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

// Class Compound_expression.

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
Compound_expression::do_determine_type(Gogo* gogo, const Type_context* context)
{
  this->init_->determine_type_no_context(gogo);
  this->expr_->determine_type(gogo, context);
}

// Get the backend representation of a compound expression.

Bexpression*
Compound_expression::do_get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Bexpression* binit = this->init_->get_backend(context);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  Bstatement* init_stmt = gogo->backend()->expression_statement(bfunction,
                                                                binit);
  Bexpression* bexpr = this->expr_->get_backend(context);
  return gogo->backend()->compound_expression(init_stmt, bexpr,
					      this->location());
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

// Class Backend_expression.

int
Backend_expression::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

Expression*
Backend_expression::do_copy()
{
  return new Backend_expression(this->bexpr_, this->type_->copy_expressions(),
				this->location());
}

void
Backend_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "backend_expression<";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ">";
}

Expression*
Expression::make_backend(Bexpression* bexpr, Type* type, Location location)
{
  return new Backend_expression(bexpr, type, location);
}

// Import an expression.  This comes at the end in order to see the
// various class definitions.

Expression*
Expression::import_expression(Import_expression* imp, Location loc)
{
  Expression* expr = Expression::import_expression_without_suffix(imp, loc);
  while (true)
    {
      if (imp->match_c_string("("))
	{
	  imp->advance(1);
	  Expression_list* args = new Expression_list();
	  bool is_varargs = false;
	  while (!imp->match_c_string(")"))
	    {
	      Expression* arg = Expression::import_expression(imp, loc);
	      if (arg->is_error_expression())
		return arg;
	      args->push_back(arg);
	      if (imp->match_c_string(")"))
		break;
	      else if (imp->match_c_string("...)"))
		{
		  imp->advance(3);
		  is_varargs = true;
		  break;
		}
	      imp->require_c_string(", ");
	    }
	  imp->require_c_string(")");
	  expr = Expression::make_call(expr, args, is_varargs, loc);
          expr->call_expression()->set_varargs_are_lowered();
	}
      else if (imp->match_c_string("["))
	{
	  imp->advance(1);
	  Expression* start = Expression::import_expression(imp, loc);
	  Expression* end = NULL;
	  Expression* cap = NULL;
	  if (imp->match_c_string(":"))
	    {
	      imp->advance(1);
	      int c = imp->peek_char();
	      if (c == ':' || c == ']')
		end = Expression::make_nil(loc);
	      else
		end = Expression::import_expression(imp, loc);
	      if (imp->match_c_string(":"))
		{
		  imp->advance(1);
		  cap = Expression::import_expression(imp, loc);
		}
	    }
	  imp->require_c_string("]");
	  expr = Expression::make_index(expr, start, end, cap, loc);
	}
      else
	break;
    }

  return expr;
}

// Import an expression without considering a suffix (function
// arguments, index operations, etc.).

Expression*
Expression::import_expression_without_suffix(Import_expression* imp,
					     Location loc)
{
  int c = imp->peek_char();
  if (c == '+' || c == '-' || c == '!' || c == '^' || c == '&' || c == '*')
    return Unary_expression::do_import(imp, loc);
  else if (c == '(')
    return Binary_expression::do_import(imp, loc);
  else if (imp->match_c_string("$true")
	   || imp->match_c_string("$false")
	   || (imp->version() < EXPORT_FORMAT_V3
	       && (imp->match_c_string("true")
		   || imp->match_c_string("false"))))
    return Boolean_expression::do_import(imp, loc);
  else if (c == '"')
    return String_expression::do_import(imp, loc);
  else if (c == '-' || (c >= '0' && c <= '9'))
    {
      // This handles integers, floats and complex constants.
      return Integer_expression::do_import(imp, loc);
    }
  else if (imp->match_c_string("<-"))
    return Receive_expression::do_import(imp, loc);
  else if (imp->match_c_string("$nil")
	   || (imp->version() < EXPORT_FORMAT_V3
	       && imp->match_c_string("nil")))
    return Nil_expression::do_import(imp, loc);
  else if (imp->match_c_string("$convert")
	   || (imp->version() < EXPORT_FORMAT_V3
	       && imp->match_c_string("convert")))
    return Type_conversion_expression::do_import(imp, loc);

  Import_function_body* ifb = imp->ifb();
  if (ifb == NULL)
    {
      go_error_at(imp->location(), "import error: expected expression");
      return Expression::make_error(loc);
    }
  if (ifb->saw_error())
    return Expression::make_error(loc);

  if (ifb->match_c_string("$t"))
    return Temporary_reference_expression::do_import(ifb, loc);

  return Expression::import_identifier(ifb, loc);
}

// Import an identifier in an expression.  This is a reference to a
// variable or function.

Expression*
Expression::import_identifier(Import_function_body* ifb, Location loc)
{
  std::string id;
  Package* pkg;
  bool is_exported;
  if (!Import::read_qualified_identifier(ifb, &id, &pkg, &is_exported))
    {
      if (!ifb->saw_error())
	go_error_at(ifb->location(),
		    "import error for %qs: bad qualified identifier at %lu",
		    ifb->name().c_str(),
		    static_cast<unsigned long>(ifb->off()));
      ifb->set_saw_error();
      return Expression::make_error(loc);
    }

  Named_object* no = NULL;
  if (pkg == NULL && is_exported)
    no = ifb->block()->bindings()->lookup(id);
  if (no == NULL)
    {
      const Package* ipkg = pkg;
      if (ipkg == NULL)
	ipkg = ifb->function()->package();
      if (!is_exported)
	id = '.' + ipkg->pkgpath() + '.' + id;
      no = ipkg->bindings()->lookup(id);
    }
  if (no == NULL)
    no = ifb->gogo()->lookup_global(id.c_str());

  if (no == NULL)
    {
      if (!ifb->saw_error())
	go_error_at(ifb->location(),
		    "import error for %qs: lookup of %qs failed",
		    ifb->name().c_str(), id.c_str());
      ifb->set_saw_error();
      return Expression::make_error(loc);
    }

  if (no->is_variable() || no->is_result_variable())
    return Expression::make_var_reference(no, loc);
  else if (no->is_function() || no->is_function_declaration())
    return Expression::make_func_reference(no, NULL, loc);
  else
    {
      if (!ifb->saw_error())
	go_error_at(ifb->location(),
		    ("import error for %qs: "
		     "unexpected type of identifier %qs (%d)"),
		    ifb->name().c_str(),
		    id.c_str(), no->classification());
      ifb->set_saw_error();
      return Expression::make_error(loc);
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
      mpfr_init_set(this->u_.float_val, a.u_.float_val, MPFR_RNDN);
      break;
    case NC_COMPLEX:
      mpc_init2(this->u_.complex_val, mpc_precision);
      mpc_set(this->u_.complex_val, a.u_.complex_val, MPC_RNDNN);
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
      mpfr_init_set(this->u_.float_val, a.u_.float_val, MPFR_RNDN);
      break;
    case NC_COMPLEX:
      mpc_init2(this->u_.complex_val, mpc_precision);
      mpc_set(this->u_.complex_val, a.u_.complex_val, MPC_RNDNN);
      break;
    default:
      go_unreachable();
    }
  return *this;
}

// Check equality with another numeric constant.

bool
Numeric_constant::equals(const Numeric_constant& a) const
{
  if (this->classification_ != a.classification_)
    return false;

  if (this->type_ != NULL && a.type_ != NULL
      && !Type::are_identical(this->type_, a.type_,
			      Type::COMPARE_ALIASES, NULL))
    return false;

  switch (a.classification_)
    {
    case NC_INVALID:
      break;
    case NC_INT:
    case NC_RUNE:
      return mpz_cmp(this->u_.int_val, a.u_.int_val) == 0;
    case NC_FLOAT:
      return mpfr_cmp(this->u_.float_val, a.u_.float_val) == 0;
    case NC_COMPLEX:
      return mpc_cmp(this->u_.complex_val, a.u_.complex_val) == 0;
    default:
      go_unreachable();
    }
  return false;
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
      mpc_clear(this->u_.complex_val);
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
  int bits = 0;
  if (type != NULL
      && type->float_type() != NULL
      && !type->float_type()->is_abstract())
    bits = type->float_type()->bits();
  if (Numeric_constant::is_float_neg_zero(val, bits))
    mpfr_init_set_ui(this->u_.float_val, 0, MPFR_RNDN);
  else
    mpfr_init_set(this->u_.float_val, val, MPFR_RNDN);
}

// Set to a complex value.

void
Numeric_constant::set_complex(Type* type, const mpc_t val)
{
  this->clear();
  this->classification_ = NC_COMPLEX;
  this->type_ = type;

  // Avoid negative zero as in set_float.
  int bits = 0;
  if (type != NULL
      && type->complex_type() != NULL
      && !type->complex_type()->is_abstract())
    bits = type->complex_type()->bits() / 2;

  mpfr_t real;
  mpfr_init_set(real, mpc_realref(val), MPFR_RNDN);
  if (Numeric_constant::is_float_neg_zero(real, bits))
    mpfr_set_ui(real, 0, MPFR_RNDN);

  mpfr_t imag;
  mpfr_init_set(imag, mpc_imagref(val), MPFR_RNDN);
  if (Numeric_constant::is_float_neg_zero(imag, bits))
    mpfr_set_ui(imag, 0, MPFR_RNDN);

  mpc_init2(this->u_.complex_val, mpc_precision);
  mpc_set_fr_fr(this->u_.complex_val, real, imag, MPC_RNDNN);

  mpfr_clear(real);
  mpfr_clear(imag);
}

// Return whether VAL, at a precision of BITS, is a negative zero.
// BITS may be zero in which case it is ignored.

bool
Numeric_constant::is_float_neg_zero(const mpfr_t val, int bits)
{
  if (!mpfr_signbit(val))
    return false;
  if (mpfr_zero_p(val))
    return true;
  mpfr_exp_t min_exp;
  switch (bits)
    {
    case 0:
      return false;
    case 32:
      // In a denormalized float32 the exponent is -126, and there are
      // 24 bits of which at least the last must be 1, so the smallest
      // representable non-zero exponent is -126 - (24 - 1) == -149.
      min_exp = -149;
      break;
    case 64:
      // Minimum exponent is -1022, there are 53 bits.
      min_exp = -1074;
      break;
    default:
      go_unreachable();
    }
  return mpfr_get_exp(val) < min_exp;
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
  mpfr_init_set(*val, this->u_.float_val, MPFR_RNDN);
}

// Get a complex value.

void
Numeric_constant::get_complex(mpc_t* val) const
{
  go_assert(this->is_complex());
  mpc_init2(*val, mpc_precision);
  mpc_set(*val, this->u_.complex_val, MPC_RNDNN);
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
      if (!mpfr_zero_p(mpc_imagref(this->u_.complex_val)))
	return NC_UL_NOTINT;
      return this->mpfr_to_unsigned_long(mpc_realref(this->u_.complex_val),
					 val);
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
  mpfr_get_z(ival, fval, MPFR_RNDN);
  To_unsigned_long ret = this->mpz_to_unsigned_long(ival, val);
  mpz_clear(ival);
  return ret;
}

// Express value as memory size if possible.

bool
Numeric_constant::to_memory_size(int64_t* val) const
{
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      return this->mpz_to_memory_size(this->u_.int_val, val);
    case NC_FLOAT:
      return this->mpfr_to_memory_size(this->u_.float_val, val);
    case NC_COMPLEX:
      if (!mpfr_zero_p(mpc_imagref(this->u_.complex_val)))
	return false;
      return this->mpfr_to_memory_size(mpc_realref(this->u_.complex_val), val);
    default:
      go_unreachable();
    }
}

// Express integer as memory size if possible.

bool
Numeric_constant::mpz_to_memory_size(const mpz_t ival, int64_t* val) const
{
  if (mpz_sgn(ival) < 0)
    return false;
  if (mpz_fits_slong_p(ival))
    {
      *val = static_cast<int64_t>(mpz_get_si(ival));
      return true;
    }

  // Test >= 64, not > 64, because an int64_t can hold 63 bits of a
  // positive value.
  if (mpz_sizeinbase(ival, 2) >= 64)
    return false;

  mpz_t q, r;
  mpz_init(q);
  mpz_init(r);
  mpz_tdiv_q_2exp(q, ival, 32);
  mpz_tdiv_r_2exp(r, ival, 32);
  go_assert(mpz_fits_ulong_p(q) && mpz_fits_ulong_p(r));
  *val = ((static_cast<int64_t>(mpz_get_ui(q)) << 32)
	  + static_cast<int64_t>(mpz_get_ui(r)));
  mpz_clear(r);
  mpz_clear(q);
  return true;
}

// Express floating point value as memory size if possible.

bool
Numeric_constant::mpfr_to_memory_size(const mpfr_t fval, int64_t* val) const
{
  if (!mpfr_integer_p(fval))
    return false;
  mpz_t ival;
  mpz_init(ival);
  mpfr_get_z(ival, fval, MPFR_RNDN);
  bool ret = this->mpz_to_memory_size(ival, val);
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
      mpfr_get_z(*val, this->u_.float_val, MPFR_RNDN);
      return true;
    case NC_COMPLEX:
      if (!mpfr_zero_p(mpc_imagref(this->u_.complex_val))
	  || !mpfr_integer_p(mpc_realref(this->u_.complex_val)))
	return false;
      mpz_init(*val);
      mpfr_get_z(*val, mpc_realref(this->u_.complex_val), MPFR_RNDN);
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
      mpfr_init_set_z(*val, this->u_.int_val, MPFR_RNDN);
      return true;
    case NC_FLOAT:
      mpfr_init_set(*val, this->u_.float_val, MPFR_RNDN);
      return true;
    case NC_COMPLEX:
      if (!mpfr_zero_p(mpc_imagref(this->u_.complex_val)))
	return false;
      mpfr_init_set(*val, mpc_realref(this->u_.complex_val), MPFR_RNDN);
      return true;
    default:
      go_unreachable();
    }
}

// Convert value to complex.

bool
Numeric_constant::to_complex(mpc_t* val) const
{
  mpc_init2(*val, mpc_precision);
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpc_set_z(*val, this->u_.int_val, MPC_RNDNN);
      return true;
    case NC_FLOAT:
      mpc_set_fr(*val, this->u_.float_val, MPC_RNDNN);
      return true;
    case NC_COMPLEX:
      mpc_set(*val, this->u_.complex_val, MPC_RNDNN);
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
  if (type == NULL || type->is_error())
    ret = true;
  else if (type->integer_type() != NULL)
    ret = this->check_int_type(type->integer_type(), issue_error, loc);
  else if (type->float_type() != NULL)
    ret = this->check_float_type(type->float_type(), issue_error, loc);
  else if (type->complex_type() != NULL)
    ret = this->check_complex_type(type->complex_type(), issue_error, loc);
  else
    {
      ret = false;
      if (issue_error)
        go_assert(saw_errors());
    }
  if (ret)
    this->type_ = type;
  return ret;
}

// Check whether the constant can be expressed in an integer type.

bool
Numeric_constant::check_int_type(Integer_type* type, bool issue_error,
				 Location location)
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
            {
              go_error_at(location,
                          "floating-point constant truncated to integer");
              this->set_invalid();
            }
	  return false;
	}
      mpz_init(val);
      mpfr_get_z(val, this->u_.float_val, MPFR_RNDN);
      break;

    case NC_COMPLEX:
      if (!mpfr_integer_p(mpc_realref(this->u_.complex_val))
	  || !mpfr_zero_p(mpc_imagref(this->u_.complex_val)))
	{
	  if (issue_error)
            {
              go_error_at(location, "complex constant truncated to integer");
              this->set_invalid();
            }
	  return false;
	}
      mpz_init(val);
      mpfr_get_z(val, mpc_realref(this->u_.complex_val), MPFR_RNDN);
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
    {
      go_error_at(location, "integer constant overflow");
      this->set_invalid();
    }

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
      mpfr_init_set_z(val, this->u_.int_val, MPFR_RNDN);
      break;

    case NC_FLOAT:
      mpfr_init_set(val, this->u_.float_val, MPFR_RNDN);
      break;

    case NC_COMPLEX:
      if (!mpfr_zero_p(mpc_imagref(this->u_.complex_val)))
	{
	  if (issue_error)
            {
              this->set_invalid();
              go_error_at(location,
			  "complex constant truncated to floating-point");
            }
	  return false;
	}
      mpfr_init_set(val, mpc_realref(this->u_.complex_val), MPFR_RNDN);
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
      mpfr_exp_t exp = mpfr_get_exp(val);
      mpfr_exp_t max_exp;
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
	  mpfr_set(t, val, MPFR_RNDN);
	  mpfr_set(val, t, MPFR_RNDN);
	  mpfr_clear(t);

	  this->set_float(type, val);
	}
    }

  mpfr_clear(val);

  if (!ret && issue_error)
    {
      go_error_at(location, "floating-point constant overflow");
      this->set_invalid();
    }

  return ret;
}

// Check whether the constant can be expressed in a complex type.

bool
Numeric_constant::check_complex_type(Complex_type* type, bool issue_error,
				     Location location)
{
  if (type->is_abstract())
    return true;

  mpfr_exp_t max_exp;
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

  mpc_t val;
  mpc_init2(val, mpc_precision);
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpc_set_z(val, this->u_.int_val, MPC_RNDNN);
      break;

    case NC_FLOAT:
      mpc_set_fr(val, this->u_.float_val, MPC_RNDNN);
      break;

    case NC_COMPLEX:
      mpc_set(val, this->u_.complex_val, MPC_RNDNN);
      break;

    default:
      go_unreachable();
    }

  bool ret = true;
  if (!mpfr_nan_p(mpc_realref(val))
      && !mpfr_inf_p(mpc_realref(val))
      && !mpfr_zero_p(mpc_realref(val))
      && mpfr_get_exp(mpc_realref(val)) > max_exp)
    {
      if (issue_error)
        {
          go_error_at(location, "complex real part overflow");
          this->set_invalid();
        }
      ret = false;
    }

  if (!mpfr_nan_p(mpc_imagref(val))
      && !mpfr_inf_p(mpc_imagref(val))
      && !mpfr_zero_p(mpc_imagref(val))
      && mpfr_get_exp(mpc_imagref(val)) > max_exp)
    {
      if (issue_error)
        {
          go_error_at(location, "complex imaginary part overflow");
          this->set_invalid();
        }
      ret = false;
    }

  if (ret)
    {
      // Round the constant to the desired type.
      mpc_t t;
      switch (type->bits())
	{
	case 64:
	  mpc_init2(t, 24);
	  break;
	case 128:
	  mpc_init2(t, 53);
	  break;
	default:
	  go_unreachable();
	}
      mpc_set(t, val, MPC_RNDNN);
      mpc_set(val, t, MPC_RNDNN);
      mpc_clear(t);

      this->set_complex(type, val);
    }

  mpc_clear(val);

  return ret;
}

// Return an Expression for this value.

Expression*
Numeric_constant::expression(Location loc) const
{
  switch (this->classification_)
    {
    case NC_INT:
      return Expression::make_integer_z(&this->u_.int_val, this->type_, loc);
    case NC_RUNE:
      return Expression::make_character(&this->u_.int_val, this->type_, loc);
    case NC_FLOAT:
      return Expression::make_float(&this->u_.float_val, this->type_, loc);
    case NC_COMPLEX:
      return Expression::make_complex(&this->u_.complex_val, this->type_, loc);
    case NC_INVALID:
      go_assert(saw_errors());
      return Expression::make_error(loc);
    default:
      go_unreachable();
    }
}

// Calculate a hash code with a given seed.

unsigned int
Numeric_constant::hash(unsigned int seed) const
{
  unsigned long val;
  const unsigned int PRIME = 97;
  long e = 0;
  double f = 1.0;
  mpfr_t m;

  switch (this->classification_)
    {
    case NC_INVALID:
      return PRIME;
    case NC_INT:
    case NC_RUNE:
      val = mpz_get_ui(this->u_.int_val);
      break;
    case NC_COMPLEX:
      mpfr_init(m);
      mpc_abs(m, this->u_.complex_val, MPFR_RNDN);
      val = mpfr_get_ui(m, MPFR_RNDN);
      mpfr_clear(m);
      break;
    case NC_FLOAT:
      f = mpfr_get_d_2exp(&e, this->u_.float_val, MPFR_RNDN) * 4294967295.0;
      val = static_cast<unsigned long>(e + static_cast<long>(f));
      break;
    default:
      go_unreachable();
    }

  return (static_cast<unsigned int>(val) + seed) * PRIME;
}
