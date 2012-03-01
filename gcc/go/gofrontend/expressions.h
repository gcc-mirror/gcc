// expressions.h -- Go frontend expression handling.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_EXPRESSIONS_H
#define GO_EXPRESSIONS_H

#include <gmp.h>
#include <mpfr.h>

#include "operator.h"

class Gogo;
class Translate_context;
class Traverse;
class Statement_inserter;
class Type;
struct Type_context;
class Function_type;
class Map_type;
class Struct_type;
class Struct_field;
class Expression_list;
class Var_expression;
class Temporary_reference_expression;
class Set_and_use_temporary_expression;
class String_expression;
class Binary_expression;
class Call_expression;
class Func_expression;
class Unknown_expression;
class Index_expression;
class Map_index_expression;
class Bound_method_expression;
class Field_reference_expression;
class Interface_field_reference_expression;
class Type_guard_expression;
class Receive_expression;
class Named_object;
class Export;
class Import;
class Temporary_statement;
class Label;
class Ast_dump_context;
class String_dump;

// The base class for all expressions.

class Expression
{
 public:
  // The types of expressions.
  enum Expression_classification
  {
    EXPRESSION_ERROR,
    EXPRESSION_TYPE,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_CONST_REFERENCE,
    EXPRESSION_VAR_REFERENCE,
    EXPRESSION_TEMPORARY_REFERENCE,
    EXPRESSION_SET_AND_USE_TEMPORARY,
    EXPRESSION_SINK,
    EXPRESSION_FUNC_REFERENCE,
    EXPRESSION_UNKNOWN_REFERENCE,
    EXPRESSION_BOOLEAN,
    EXPRESSION_STRING,
    EXPRESSION_INTEGER,
    EXPRESSION_FLOAT,
    EXPRESSION_COMPLEX,
    EXPRESSION_NIL,
    EXPRESSION_IOTA,
    EXPRESSION_CALL,
    EXPRESSION_CALL_RESULT,
    EXPRESSION_BOUND_METHOD,
    EXPRESSION_INDEX,
    EXPRESSION_ARRAY_INDEX,
    EXPRESSION_STRING_INDEX,
    EXPRESSION_MAP_INDEX,
    EXPRESSION_SELECTOR,
    EXPRESSION_FIELD_REFERENCE,
    EXPRESSION_INTERFACE_FIELD_REFERENCE,
    EXPRESSION_ALLOCATION,
    EXPRESSION_TYPE_GUARD,
    EXPRESSION_CONVERSION,
    EXPRESSION_UNSAFE_CONVERSION,
    EXPRESSION_STRUCT_CONSTRUCTION,
    EXPRESSION_FIXED_ARRAY_CONSTRUCTION,
    EXPRESSION_OPEN_ARRAY_CONSTRUCTION,
    EXPRESSION_MAP_CONSTRUCTION,
    EXPRESSION_COMPOSITE_LITERAL,
    EXPRESSION_HEAP_COMPOSITE,
    EXPRESSION_RECEIVE,
    EXPRESSION_TYPE_DESCRIPTOR,
    EXPRESSION_TYPE_INFO,
    EXPRESSION_STRUCT_FIELD_OFFSET,
    EXPRESSION_MAP_DESCRIPTOR,
    EXPRESSION_LABEL_ADDR
  };

  Expression(Expression_classification, Location);

  virtual ~Expression();

  // Make an error expression.  This is used when a parse error occurs
  // to prevent cascading errors.
  static Expression*
  make_error(Location);

  // Make an expression which is really a type.  This is used during
  // parsing.
  static Expression*
  make_type(Type*, Location);

  // Make a unary expression.
  static Expression*
  make_unary(Operator, Expression*, Location);

  // Make a binary expression.
  static Expression*
  make_binary(Operator, Expression*, Expression*, Location);

  // Make a reference to a constant in an expression.
  static Expression*
  make_const_reference(Named_object*, Location);

  // Make a reference to a variable in an expression.
  static Expression*
  make_var_reference(Named_object*, Location);

  // Make a reference to a temporary variable.  Temporary variables
  // are always created by a single statement, which is what we use to
  // refer to them.
  static Temporary_reference_expression*
  make_temporary_reference(Temporary_statement*, Location);

  // Make an expressions which sets a temporary variable and then
  // evaluates to a reference to that temporary variable.  This is
  // used to set a temporary variable while retaining the order of
  // evaluation.
  static Set_and_use_temporary_expression*
  make_set_and_use_temporary(Temporary_statement*, Expression*, Location);

  // Make a sink expression--a reference to the blank identifier _.
  static Expression*
  make_sink(Location);

  // Make a reference to a function in an expression.
  static Expression*
  make_func_reference(Named_object*, Expression* closure, Location);

  // Make a reference to an unknown name.  In a correct program this
  // will always be lowered to a real const/var/func reference.
  static Unknown_expression*
  make_unknown_reference(Named_object*, Location);

  // Make a constant bool expression.
  static Expression*
  make_boolean(bool val, Location);

  // Make a constant string expression.
  static Expression*
  make_string(const std::string&, Location);

  // Make a character constant expression.  TYPE should be NULL for an
  // abstract type.
  static Expression*
  make_character(const mpz_t*, Type*, Location);

  // Make a constant integer expression.  TYPE should be NULL for an
  // abstract type.
  static Expression*
  make_integer(const mpz_t*, Type*, Location);

  // Make a constant float expression.  TYPE should be NULL for an
  // abstract type.
  static Expression*
  make_float(const mpfr_t*, Type*, Location);

  // Make a constant complex expression.  TYPE should be NULL for an
  // abstract type.
  static Expression*
  make_complex(const mpfr_t* real, const mpfr_t* imag, Type*, Location);

  // Make a nil expression.
  static Expression*
  make_nil(Location);

  // Make an iota expression.  This is used for the predeclared
  // constant iota.
  static Expression*
  make_iota();

  // Make a call expression.
  static Call_expression*
  make_call(Expression* func, Expression_list* args, bool is_varargs,
	    Location);

  // Make a reference to a specific result of a call expression which
  // returns a tuple.
  static Expression*
  make_call_result(Call_expression*, unsigned int index);

  // Make an expression which is a method bound to its first
  // parameter.
  static Bound_method_expression*
  make_bound_method(Expression* object, Named_object* method, Location);

  // Make an index or slice expression.  This is a parser expression
  // which represents LEFT[START:END].  END may be NULL, meaning an
  // index rather than a slice.  At parse time we may not know the
  // type of LEFT.  After parsing this is lowered to an array index, a
  // string index, or a map index.
  static Expression*
  make_index(Expression* left, Expression* start, Expression* end,
	     Location);

  // Make an array index expression.  END may be NULL, in which case
  // this is an lvalue.
  static Expression*
  make_array_index(Expression* array, Expression* start, Expression* end,
		   Location);

  // Make a string index expression.  END may be NULL.  This is never
  // an lvalue.
  static Expression*
  make_string_index(Expression* string, Expression* start, Expression* end,
		    Location);

  // Make a map index expression.  This is an lvalue.
  static Map_index_expression*
  make_map_index(Expression* map, Expression* val, Location);

  // Make a selector.  This is a parser expression which represents
  // LEFT.NAME.  At parse time we may not know the type of the left
  // hand side.
  static Expression*
  make_selector(Expression* left, const std::string& name, Location);

  // Make a reference to a field in a struct.
  static Field_reference_expression*
  make_field_reference(Expression*, unsigned int field_index, Location);

  // Make a reference to a field of an interface, with an associated
  // object.
  static Expression*
  make_interface_field_reference(Expression*, const std::string&,
				 Location);

  // Make an allocation expression.
  static Expression*
  make_allocation(Type*, Location);

  // Make a type guard expression.
  static Expression*
  make_type_guard(Expression*, Type*, Location);

  // Make a type cast expression.
  static Expression*
  make_cast(Type*, Expression*, Location);

  // Make an unsafe type cast expression.  This is only used when
  // passing parameter to builtin functions that are part of the Go
  // runtime.
  static Expression*
  make_unsafe_cast(Type*, Expression*, Location);

  // Make a composite literal.  The DEPTH parameter is how far down we
  // are in a list of composite literals with omitted types.
  static Expression*
  make_composite_literal(Type*, int depth, bool has_keys, Expression_list*,
			 Location);

  // Make a struct composite literal.
  static Expression*
  make_struct_composite_literal(Type*, Expression_list*, Location);

  // Make a slice composite literal.
  static Expression*
  make_slice_composite_literal(Type*, Expression_list*, Location);

  // Take a composite literal and allocate it on the heap.
  static Expression*
  make_heap_composite(Expression*, Location);

  // Make a receive expression.  VAL is NULL for a unary receive.
  static Receive_expression*
  make_receive(Expression* channel, Location);

  // Make an expression which evaluates to the address of the type
  // descriptor for TYPE.
  static Expression*
  make_type_descriptor(Type* type, Location);

  // Make an expression which evaluates to some characteristic of a
  // type.  These are only used for type descriptors, so there is no
  // location parameter.
  enum Type_info
    {
      // The size of a value of the type.
      TYPE_INFO_SIZE,
      // The required alignment of a value of the type.
      TYPE_INFO_ALIGNMENT,
      // The required alignment of a value of the type when used as a
      // field in a struct.
      TYPE_INFO_FIELD_ALIGNMENT
    };

  static Expression*
  make_type_info(Type* type, Type_info);

  // Make an expression which evaluates to the offset of a field in a
  // struct.  This is only used for type descriptors, so there is no
  // location parameter.
  static Expression*
  make_struct_field_offset(Struct_type*, const Struct_field*);

  // Make an expression which evaluates to the address of the map
  // descriptor for TYPE.
  static Expression*
  make_map_descriptor(Map_type* type, Location);

  // Make an expression which evaluates to the address of an unnamed
  // label.
  static Expression*
  make_label_addr(Label*, Location);

  // Return the expression classification.
  Expression_classification
  classification() const
  { return this->classification_; }

  // Return the location of the expression.
  Location
  location() const
  { return this->location_; }

  // Return whether this is a constant expression.
  bool
  is_constant() const
  { return this->do_is_constant(); }

  // If this is not a constant expression with integral type, return
  // false.  If it is one, return true, and set VAL to the value.  VAL
  // should already be initialized.  If this returns true, it sets
  // *PTYPE to the type of the value, or NULL for an abstract type.
  // If IOTA_IS_CONSTANT is true, then an iota expression is assumed
  // to have its final value.
  bool
  integer_constant_value(bool iota_is_constant, mpz_t val, Type** ptype) const;

  // If this is not a constant expression with floating point type,
  // return false.  If it is one, return true, and set VAL to the
  // value.  VAL should already be initialized.  If this returns true,
  // it sets *PTYPE to the type of the value, or NULL for an abstract
  // type.
  bool
  float_constant_value(mpfr_t val, Type** ptype) const;

  // If this is not a constant expression with complex type, return
  // false.  If it is one, return true, and set REAL and IMAG to the
  // value.  REAL and IMAG should already be initialized.  If this
  // return strue, it sets *PTYPE to the type of the value, or NULL
  // for an abstract type.
  bool
  complex_constant_value(mpfr_t real, mpfr_t imag, Type** ptype) const;

  // If this is not a constant expression with string type, return
  // false.  If it is one, return true, and set VAL to the value.
  bool
  string_constant_value(std::string* val) const
  { return this->do_string_constant_value(val); }

  // This is called if the value of this expression is being
  // discarded.  This issues warnings about computed values being
  // unused.
  void
  discarding_value()
  { this->do_discarding_value(); }

  // Return whether this is an error expression.
  bool
  is_error_expression() const
  { return this->classification_ == EXPRESSION_ERROR; }

  // Return whether this expression really represents a type.
  bool
  is_type_expression() const
  { return this->classification_ == EXPRESSION_TYPE; }

  // If this is a variable reference, return the Var_expression
  // structure.  Otherwise, return NULL.  This is a controlled dynamic
  // cast.
  Var_expression*
  var_expression()
  { return this->convert<Var_expression, EXPRESSION_VAR_REFERENCE>(); }

  const Var_expression*
  var_expression() const
  { return this->convert<const Var_expression, EXPRESSION_VAR_REFERENCE>(); }

  // If this is a reference to a temporary variable, return the
  // Temporary_reference_expression.  Otherwise, return NULL.
  Temporary_reference_expression*
  temporary_reference_expression()
  {
    return this->convert<Temporary_reference_expression,
			 EXPRESSION_TEMPORARY_REFERENCE>();
  }

  // If this is a set-and-use-temporary, return the
  // Set_and_use_temporary_expression.  Otherwise, return NULL.
  Set_and_use_temporary_expression*
  set_and_use_temporary_expression()
  {
    return this->convert<Set_and_use_temporary_expression,
			 EXPRESSION_SET_AND_USE_TEMPORARY>();
  }

  // Return whether this is a sink expression.
  bool
  is_sink_expression() const
  { return this->classification_ == EXPRESSION_SINK; }

  // If this is a string expression, return the String_expression
  // structure.  Otherwise, return NULL.
  String_expression*
  string_expression()
  { return this->convert<String_expression, EXPRESSION_STRING>(); }

  // Return whether this is the expression nil.
  bool
  is_nil_expression() const
  { return this->classification_ == EXPRESSION_NIL; }

  // If this is an indirection through a pointer, return the
  // expression being pointed through.  Otherwise return this.
  Expression*
  deref();

  // If this is a binary expression, return the Binary_expression
  // structure.  Otherwise return NULL.
  Binary_expression*
  binary_expression()
  { return this->convert<Binary_expression, EXPRESSION_BINARY>(); }

  // If this is a call expression, return the Call_expression
  // structure.  Otherwise, return NULL.  This is a controlled dynamic
  // cast.
  Call_expression*
  call_expression()
  { return this->convert<Call_expression, EXPRESSION_CALL>(); }

  // If this is an expression which refers to a function, return the
  // Func_expression structure.  Otherwise, return NULL.
  Func_expression*
  func_expression()
  { return this->convert<Func_expression, EXPRESSION_FUNC_REFERENCE>(); }

  const Func_expression*
  func_expression() const
  { return this->convert<const Func_expression, EXPRESSION_FUNC_REFERENCE>(); }

  // If this is an expression which refers to an unknown name, return
  // the Unknown_expression structure.  Otherwise, return NULL.
  Unknown_expression*
  unknown_expression()
  { return this->convert<Unknown_expression, EXPRESSION_UNKNOWN_REFERENCE>(); }

  const Unknown_expression*
  unknown_expression() const
  {
    return this->convert<const Unknown_expression,
			 EXPRESSION_UNKNOWN_REFERENCE>();
  }

  // If this is an index expression, return the Index_expression
  // structure.  Otherwise, return NULL.
  Index_expression*
  index_expression()
  { return this->convert<Index_expression, EXPRESSION_INDEX>(); }

  // If this is an expression which refers to indexing in a map,
  // return the Map_index_expression structure.  Otherwise, return
  // NULL.
  Map_index_expression*
  map_index_expression()
  { return this->convert<Map_index_expression, EXPRESSION_MAP_INDEX>(); }

  // If this is a bound method expression, return the
  // Bound_method_expression structure.  Otherwise, return NULL.
  Bound_method_expression*
  bound_method_expression()
  { return this->convert<Bound_method_expression, EXPRESSION_BOUND_METHOD>(); }

  // If this is a reference to a field in a struct, return the
  // Field_reference_expression structure.  Otherwise, return NULL.
  Field_reference_expression*
  field_reference_expression()
  {
    return this->convert<Field_reference_expression,
			 EXPRESSION_FIELD_REFERENCE>();
  }

  // If this is a reference to a field in an interface, return the
  // Interface_field_reference_expression structure.  Otherwise,
  // return NULL.
  Interface_field_reference_expression*
  interface_field_reference_expression()
  {
    return this->convert<Interface_field_reference_expression,
			 EXPRESSION_INTERFACE_FIELD_REFERENCE>();
  }

  // If this is a type guard expression, return the
  // Type_guard_expression structure.  Otherwise, return NULL.
  Type_guard_expression*
  type_guard_expression()
  { return this->convert<Type_guard_expression, EXPRESSION_TYPE_GUARD>(); }

  // If this is a receive expression, return the Receive_expression
  // structure.  Otherwise, return NULL.
  Receive_expression*
  receive_expression()
  { return this->convert<Receive_expression, EXPRESSION_RECEIVE>(); }

  // Return true if this is a composite literal.
  bool
  is_composite_literal() const;

  // Return true if this is a composite literal which is not constant.
  bool
  is_nonconstant_composite_literal() const;

  // Return true if this is a reference to a local variable.
  bool
  is_local_variable() const;

  // Traverse an expression.
  static int
  traverse(Expression**, Traverse*);

  // Traverse subexpressions of this expression.
  int
  traverse_subexpressions(Traverse*);

  // Lower an expression.  This is called immediately after parsing.
  // FUNCTION is the function we are in; it will be NULL for an
  // expression initializing a global variable.  INSERTER may be used
  // to insert statements before the statement or initializer
  // containing this expression; it is normally used to create
  // temporary variables.  IOTA_VALUE is the value that we should give
  // to any iota expressions.  This function must resolve expressions
  // which could not be fully parsed into their final form.  It
  // returns the same Expression or a new one.
  Expression*
  lower(Gogo* gogo, Named_object* function, Statement_inserter* inserter,
	int iota_value)
  { return this->do_lower(gogo, function, inserter, iota_value); }

  // Determine the real type of an expression with abstract integer,
  // floating point, or complex type.  TYPE_CONTEXT describes the
  // expected type.
  void
  determine_type(const Type_context*);

  // Check types in an expression.
  void
  check_types(Gogo* gogo)
  { this->do_check_types(gogo); }

  // Determine the type when there is no context.
  void
  determine_type_no_context();

  // Return the current type of the expression.  This may be changed
  // by determine_type.
  Type*
  type()
  { return this->do_type(); }

  // Return a copy of an expression.
  Expression*
  copy()
  { return this->do_copy(); }

  // Return whether the expression is addressable--something which may
  // be used as the operand of the unary & operator.
  bool
  is_addressable() const
  { return this->do_is_addressable(); }

  // Note that we are taking the address of this expression.  ESCAPES
  // is true if this address escapes the current function.
  void
  address_taken(bool escapes)
  { this->do_address_taken(escapes); }

  // Return whether this expression must be evaluated in order
  // according to the order of evaluation rules.  This is basically
  // true of all expressions with side-effects.
  bool
  must_eval_in_order() const
  { return this->do_must_eval_in_order(); }

  // Return whether subexpressions of this expression must be
  // evaluated in order.  This is true of index expressions and
  // pointer indirections.  This sets *SKIP to the number of
  // subexpressions to skip during traversing, as index expressions
  // only requiring moving the index, not the array.
  bool
  must_eval_subexpressions_in_order(int* skip) const
  {
    *skip = 0;
    return this->do_must_eval_subexpressions_in_order(skip);
  }

  // Return the tree for this expression.
  tree
  get_tree(Translate_context*);

  // Return a tree handling any conversions which must be done during
  // assignment.
  static tree
  convert_for_assignment(Translate_context*, Type* lhs_type, Type* rhs_type,
			 tree rhs_tree, Location location);

  // Return a tree converting a value of one interface type to another
  // interface type.  If FOR_TYPE_GUARD is true this is for a type
  // assertion.
  static tree
  convert_interface_to_interface(Translate_context*, Type* lhs_type,
				 Type* rhs_type, tree rhs_tree,
				 bool for_type_guard, Location);

  // Return a tree implementing the comparison LHS_TREE OP RHS_TREE.
  // TYPE is the type of both sides.
  static tree
  comparison_tree(Translate_context*, Operator op, Type* left_type,
		  tree left_tree, Type* right_type, tree right_tree,
		  Location);

  // Return a tree for the multi-precision integer VAL in TYPE.
  static tree
  integer_constant_tree(mpz_t val, tree type);

  // Return a tree for the floating point value VAL in TYPE.
  static tree
  float_constant_tree(mpfr_t val, tree type);

  // Return a tree for the complex value REAL/IMAG in TYPE.
  static tree
  complex_constant_tree(mpfr_t real, mpfr_t imag, tree type);

  // Export the expression.  This is only used for constants.  It will
  // be used for things like values of named constants and sizes of
  // arrays.
  void
  export_expression(Export* exp) const
  { this->do_export(exp); }

  // Import an expression.
  static Expression*
  import_expression(Import*);

  // Return a tree which checks that VAL, of arbitrary integer type,
  // is non-negative and is not more than the maximum value of
  // BOUND_TYPE.  If SOFAR is not NULL, it is or'red into the result.
  // The return value may be NULL if SOFAR is NULL.
  static tree
  check_bounds(tree val, tree bound_type, tree sofar, Location);

  // Dump an expression to a dump constext.
  void
  dump_expression(Ast_dump_context*) const;

 protected:
  // May be implemented by child class: traverse the expressions.
  virtual int
  do_traverse(Traverse*);

  // Return a lowered expression.
  virtual Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int)
  { return this; }

  // Return whether this is a constant expression.
  virtual bool
  do_is_constant() const
  { return false; }

  // Return whether this is a constant expression of integral type,
  // and set VAL to the value.
  virtual bool
  do_integer_constant_value(bool, mpz_t, Type**) const
  { return false; }

  // Return whether this is a constant expression of floating point
  // type, and set VAL to the value.
  virtual bool
  do_float_constant_value(mpfr_t, Type**) const
  { return false; }

  // Return whether this is a constant expression of complex type, and
  // set REAL and IMAGE to the value.
  virtual bool
  do_complex_constant_value(mpfr_t, mpfr_t, Type**) const
  { return false; }

  // Return whether this is a constant expression of string type, and
  // set VAL to the value.
  virtual bool
  do_string_constant_value(std::string*) const
  { return false; }

  // Called by the parser if the value is being discarded.
  virtual void
  do_discarding_value();

  // Child class holds type.
  virtual Type*
  do_type() = 0;

  // Child class implements determining type information.
  virtual void
  do_determine_type(const Type_context*) = 0;

  // Child class implements type checking if needed.
  virtual void
  do_check_types(Gogo*)
  { }

  // Child class implements copying.
  virtual Expression*
  do_copy() = 0;

  // Child class implements whether the expression is addressable.
  virtual bool
  do_is_addressable() const
  { return false; }

  // Child class implements taking the address of an expression.
  virtual void
  do_address_taken(bool)
  { }

  // Child class implements whether this expression must be evaluated
  // in order.
  virtual bool
  do_must_eval_in_order() const
  { return false; }

  // Child class implements whether this expressions requires that
  // subexpressions be evaluated in order.  The child implementation
  // may set *SKIP if it should be non-zero.
  virtual bool
  do_must_eval_subexpressions_in_order(int* /* skip */) const
  { return false; }

  // Child class implements conversion to tree.
  virtual tree
  do_get_tree(Translate_context*) = 0;

  // Child class implements export.
  virtual void
  do_export(Export*) const;

  // For children to call to give an error for an unused value.
  void
  unused_value_error();

  // For children to call when they detect that they are in error.
  void
  set_is_error();

  // For children to call to report an error conveniently.
  void
  report_error(const char*);

  // Child class implements dumping to a dump context.
  virtual void
  do_dump_expression(Ast_dump_context*) const = 0;

 private:
  // Convert to the desired statement classification, or return NULL.
  // This is a controlled dynamic cast.
  template<typename Expression_class,
	   Expression_classification expr_classification>
  Expression_class*
  convert()
  {
    return (this->classification_ == expr_classification
	    ? static_cast<Expression_class*>(this)
	    : NULL);
  }

  template<typename Expression_class,
	   Expression_classification expr_classification>
  const Expression_class*
  convert() const
  {
    return (this->classification_ == expr_classification
	    ? static_cast<const Expression_class*>(this)
	    : NULL);
  }

  static tree
  convert_type_to_interface(Translate_context*, Type*, Type*, tree,
			    Location);

  static tree
  get_interface_type_descriptor(Translate_context*, Type*, tree,
				Location);

  static tree
  convert_interface_to_type(Translate_context*, Type*, Type*, tree,
			    Location);

  // The expression classification.
  Expression_classification classification_;
  // The location in the input file.
  Location location_;
};

// A list of Expressions.

class Expression_list
{
 public:
  Expression_list()
    : entries_()
  { }

  // Return whether the list is empty.
  bool
  empty() const
  { return this->entries_.empty(); }

  // Return the number of entries in the list.
  size_t
  size() const
  { return this->entries_.size(); }

  // Add an entry to the end of the list.
  void
  push_back(Expression* expr)
  { this->entries_.push_back(expr); }

  void
  append(Expression_list* add)
  { this->entries_.insert(this->entries_.end(), add->begin(), add->end()); }

  // Reserve space in the list.
  void
  reserve(size_t size)
  { this->entries_.reserve(size); }

  // Traverse the expressions in the list.
  int
  traverse(Traverse*);

  // Copy the list.
  Expression_list*
  copy();

  // Return true if the list contains an error expression.
  bool
  contains_error() const;

  // Return the first and last elements.
  Expression*&
  front()
  { return this->entries_.front(); }

  Expression*
  front() const
  { return this->entries_.front(); }

  Expression*&
  back()
  { return this->entries_.back(); }

  Expression*
  back() const
  { return this->entries_.back(); }

  // Iterators.

  typedef std::vector<Expression*>::iterator iterator;
  typedef std::vector<Expression*>::const_iterator const_iterator;

  iterator
  begin()
  { return this->entries_.begin(); }

  const_iterator
  begin() const
  { return this->entries_.begin(); }

  iterator
  end()
  { return this->entries_.end(); }

  const_iterator
  end() const
  { return this->entries_.end(); }

  // Erase an entry.
  void
  erase(iterator p)
  { this->entries_.erase(p); }

 private:
  std::vector<Expression*> entries_;
};

// An abstract base class for an expression which is only used by the
// parser, and is lowered in the lowering pass.

class Parser_expression : public Expression
{
 public:
  Parser_expression(Expression_classification classification,
		    Location location)
    : Expression(classification, location)
  { }

 protected:
  virtual Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int) = 0;

  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { go_unreachable(); }

  void
  do_check_types(Gogo*)
  { go_unreachable(); }

  tree
  do_get_tree(Translate_context*)
  { go_unreachable(); }
};

// An expression which is simply a variable.

class Var_expression : public Expression
{
 public:
  Var_expression(Named_object* variable, Location location)
    : Expression(EXPRESSION_VAR_REFERENCE, location),
      variable_(variable)
  { }

  // Return the variable.
  Named_object*
  named_object() const
  { return this->variable_; }

 protected:
  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  Expression*
  do_copy()
  { return this; }

  bool
  do_is_addressable() const
  { return true; }

  void
  do_address_taken(bool);

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The variable we are referencing.
  Named_object* variable_;
};

// A reference to a temporary variable.

class Temporary_reference_expression : public Expression
{
 public:
  Temporary_reference_expression(Temporary_statement* statement,
				 Location location)
    : Expression(EXPRESSION_TEMPORARY_REFERENCE, location),
      statement_(statement), is_lvalue_(false)
  { }

  // Indicate that this reference appears on the left hand side of an
  // assignment statement.
  void
  set_is_lvalue()
  { this->is_lvalue_ = true; }

 protected:
  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return make_temporary_reference(this->statement_, this->location()); }

  bool
  do_is_addressable() const
  { return true; }

  void
  do_address_taken(bool);

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The statement where the temporary variable is defined.
  Temporary_statement* statement_;
  // Whether this reference appears on the left hand side of an
  // assignment statement.
  bool is_lvalue_;
};

// Set and use a temporary variable.

class Set_and_use_temporary_expression : public Expression
{
 public:
  Set_and_use_temporary_expression(Temporary_statement* statement,
				   Expression* expr, Location location)
    : Expression(EXPRESSION_SET_AND_USE_TEMPORARY, location),
      statement_(statement), expr_(expr)
  { }

  // Return the temporary.
  Temporary_statement*
  temporary() const
  { return this->statement_; }

  // Return the expression.
  Expression*
  expression() const
  { return this->expr_; }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->expr_, traverse); }

  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return make_set_and_use_temporary(this->statement_, this->expr_,
				      this->location());
  }

  bool
  do_is_addressable() const
  { return true; }

  void
  do_address_taken(bool);

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The statement where the temporary variable is defined.
  Temporary_statement* statement_;
  // The expression to assign to the temporary.
  Expression* expr_;
};

// A string expression.

class String_expression : public Expression
{
 public:
  String_expression(const std::string& val, Location location)
    : Expression(EXPRESSION_STRING, location),
      val_(val), type_(NULL)
  { }

  const std::string&
  val() const
  { return this->val_; }

  static Expression*
  do_import(Import*);

 protected:
  bool
  do_is_constant() const
  { return true; }

  bool
  do_string_constant_value(std::string* val) const
  {
    *val = this->val_;
    return true;
  }

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context*);

  // Write string literal to a string dump.
  static void
  export_string(String_dump* exp, const String_expression* str);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The string value.  This is immutable.
  const std::string val_;
  // The type as determined by context.
  Type* type_;
};

// A binary expression.

class Binary_expression : public Expression
{
 public:
  Binary_expression(Operator op, Expression* left, Expression* right,
		    Location location)
    : Expression(EXPRESSION_BINARY, location),
      op_(op), left_(left), right_(right)
  { }

  // Return the operator.
  Operator
  op()
  { return this->op_; }

  // Return the left hand expression.
  Expression*
  left()
  { return this->left_; }

  // Return the right hand expression.
  Expression*
  right()
  { return this->right_; }

  // Apply binary opcode OP to LEFT_VAL and RIGHT_VAL, setting VAL.
  // LEFT_TYPE is the type of LEFT_VAL, RIGHT_TYPE is the type of
  // RIGHT_VAL; LEFT_TYPE and/or RIGHT_TYPE may be NULL.  Return true
  // if this could be done, false if not.
  static bool
  eval_integer(Operator op, Type* left_type, mpz_t left_val,
	       Type* right_type, mpz_t right_val, Location,
	       mpz_t val);

  // Apply binary opcode OP to LEFT_VAL and RIGHT_VAL, setting VAL.
  // Return true if this could be done, false if not.
  static bool
  eval_float(Operator op, Type* left_type, mpfr_t left_val,
	     Type* right_type, mpfr_t right_val, mpfr_t val,
	     Location);

  // Apply binary opcode OP to LEFT_REAL/LEFT_IMAG and
  // RIGHT_REAL/RIGHT_IMAG, setting REAL/IMAG.  Return true if this
  // could be done, false if not.
  static bool
  eval_complex(Operator op, Type* left_type, mpfr_t left_real,
	       mpfr_t left_imag, Type* right_type, mpfr_t right_real,
	       mpfr_t right_imag, mpfr_t real, mpfr_t imag, Location);

  // Compare integer constants according to OP.
  static bool
  compare_integer(Operator op, mpz_t left_val, mpz_t right_val);

  // Compare floating point constants according to OP.
  static bool
  compare_float(Operator op, Type* type, mpfr_t left_val, mpfr_t right_val);

  // Compare complex constants according to OP.
  static bool
  compare_complex(Operator op, Type* type, mpfr_t left_real, mpfr_t left_imag,
		  mpfr_t right_val, mpfr_t right_imag);

  static Expression*
  do_import(Import*);

  // Report an error if OP can not be applied to TYPE.  Return whether
  // it can.  OTYPE is the type of the other operand.
  static bool
  check_operator_type(Operator op, Type* type, Type* otype, Location);

 protected:
  int
  do_traverse(Traverse* traverse);

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  bool
  do_is_constant() const
  { return this->left_->is_constant() && this->right_->is_constant(); }

  bool
  do_integer_constant_value(bool, mpz_t val, Type**) const;

  bool
  do_float_constant_value(mpfr_t val, Type**) const;

  bool
  do_complex_constant_value(mpfr_t real, mpfr_t imag, Type**) const;

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
    return Expression::make_binary(this->op_, this->left_->copy(),
				   this->right_->copy(), this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  Expression*
  lower_struct_comparison(Gogo*, Statement_inserter*);

  Expression*
  lower_array_comparison(Gogo*, Statement_inserter*);

  Expression*
  lower_compare_to_memcmp(Gogo*, Statement_inserter*);

  Expression*
  operand_address(Statement_inserter*, Expression*);

  // The binary operator to apply.
  Operator op_;
  // The left hand side operand.
  Expression* left_;
  // The right hand side operand.
  Expression* right_;
};

// A call expression.  The go statement needs to dig inside this.

class Call_expression : public Expression
{
 public:
  Call_expression(Expression* fn, Expression_list* args, bool is_varargs,
		  Location location)
    : Expression(EXPRESSION_CALL, location),
      fn_(fn), args_(args), type_(NULL), results_(NULL), tree_(NULL),
      is_varargs_(is_varargs), are_hidden_fields_ok_(false),
      varargs_are_lowered_(false), types_are_determined_(false),
      is_deferred_(false), issued_error_(false)
  { }

  // The function to call.
  Expression*
  fn() const
  { return this->fn_; }

  // The arguments.
  Expression_list*
  args()
  { return this->args_; }

  const Expression_list*
  args() const
  { return this->args_; }

  // Get the function type.
  Function_type*
  get_function_type() const;

  // Return the number of values this call will return.
  size_t
  result_count() const;

  // Return the temporary variable which holds result I.  This is only
  // valid after the expression has been lowered, and is only valid
  // for calls which return multiple results.
  Temporary_statement*
  result(size_t i) const;

  // Return whether this is a call to the predeclared function
  // recover.
  bool
  is_recover_call() const;

  // Set the argument for a call to recover.
  void
  set_recover_arg(Expression*);

  // Whether the last argument is a varargs argument (f(a...)).
  bool
  is_varargs() const
  { return this->is_varargs_; }

  // Note that varargs have already been lowered.
  void
  set_varargs_are_lowered()
  { this->varargs_are_lowered_ = true; }

  // Note that it is OK for this call to set hidden fields when
  // passing arguments.
  void
  set_hidden_fields_are_ok()
  { this->are_hidden_fields_ok_ = true; }

  // Whether this call is being deferred.
  bool
  is_deferred() const
  { return this->is_deferred_; }

  // Note that the call is being deferred.
  void
  set_is_deferred()
  { this->is_deferred_ = true; }

  // We have found an error with this call expression; return true if
  // we should report it.
  bool
  issue_error();

 protected:
  int
  do_traverse(Traverse*);

  virtual Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  void
  do_discarding_value()
  { }

  virtual Type*
  do_type();

  virtual void
  do_determine_type(const Type_context*);

  virtual void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_call(this->fn_->copy(),
				 (this->args_ == NULL
				  ? NULL
				  : this->args_->copy()),
				 this->is_varargs_, this->location());
  }

  bool
  do_must_eval_in_order() const;

  virtual tree
  do_get_tree(Translate_context*);

  virtual bool
  do_is_recover_call() const;

  virtual void
  do_set_recover_arg(Expression*);

  // Let a builtin expression change the argument list.
  void
  set_args(Expression_list* args)
  { this->args_ = args; }

  // Let a builtin expression lower varargs.
  void
  lower_varargs(Gogo*, Named_object* function, Statement_inserter* inserter,
		Type* varargs_type, size_t param_count);

  // Let a builtin expression check whether types have been
  // determined.
  bool
  determining_types();

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  bool
  check_argument_type(int, const Type*, const Type*, Location, bool);

  tree
  interface_method_function(Translate_context*,
			    Interface_field_reference_expression*,
			    tree*);

  tree
  set_results(Translate_context*, tree);

  // The function to call.
  Expression* fn_;
  // The arguments to pass.  This may be NULL if there are no
  // arguments.
  Expression_list* args_;
  // The type of the expression, to avoid recomputing it.
  Type* type_;
  // The list of temporaries which will hold the results if the
  // function returns a tuple.
  std::vector<Temporary_statement*>* results_;
  // The tree for the call, used for a call which returns a tuple.
  tree tree_;
  // True if the last argument is a varargs argument (f(a...)).
  bool is_varargs_;
  // True if this statement may pass hidden fields in the arguments.
  // This is used for generated method stubs.
  bool are_hidden_fields_ok_;
  // True if varargs have already been lowered.
  bool varargs_are_lowered_;
  // True if types have been determined.
  bool types_are_determined_;
  // True if the call is an argument to a defer statement.
  bool is_deferred_;
  // True if we reported an error about a mismatch between call
  // results and uses.  This is to avoid producing multiple errors
  // when there are multiple Call_result_expressions.
  bool issued_error_;
};

// An expression which represents a pointer to a function.

class Func_expression : public Expression
{
 public:
  Func_expression(Named_object* function, Expression* closure,
		  Location location)
    : Expression(EXPRESSION_FUNC_REFERENCE, location),
      function_(function), closure_(closure)
  { }

  // Return the object associated with the function.
  const Named_object*
  named_object() const
  { return this->function_; }

  // Return the closure for this function.  This will return NULL if
  // the function has no closure, which is the normal case.
  Expression*
  closure()
  { return this->closure_; }

  // Return a tree for this function without evaluating the closure.
  tree
  get_tree_without_closure(Gogo*);

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  {
    if (this->closure_ != NULL)
      this->closure_->determine_type_no_context();
  }

  Expression*
  do_copy()
  {
    return Expression::make_func_reference(this->function_,
					   (this->closure_ == NULL
					    ? NULL
					    : this->closure_->copy()),
					   this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The function itself.
  Named_object* function_;
  // A closure.  This is normally NULL.  For a nested function, it may
  // be a heap-allocated struct holding pointers to all the variables
  // referenced by this function and defined in enclosing functions.
  Expression* closure_;
};

// A reference to an unknown name.

class Unknown_expression : public Parser_expression
{
 public:
  Unknown_expression(Named_object* named_object, Location location)
    : Parser_expression(EXPRESSION_UNKNOWN_REFERENCE, location),
      named_object_(named_object), no_error_message_(false),
      is_composite_literal_key_(false)
  { }

  // The associated named object.
  Named_object*
  named_object() const
  { return this->named_object_; }

  // The name of the identifier which was unknown.
  const std::string&
  name() const;

  // Call this to indicate that we should not give an error if this
  // name is never defined.  This is used to avoid knock-on errors
  // during an erroneous parse.
  void
  set_no_error_message()
  { this->no_error_message_ = true; }

  // Note that this expression is being used as the key in a composite
  // literal, so it may be OK if it is not resolved.
  void
  set_is_composite_literal_key()
  { this->is_composite_literal_key_ = true; }

  // Note that this expression should no longer be treated as a
  // composite literal key.
  void
  clear_is_composite_literal_key()
  { this->is_composite_literal_key_ = false; }

 protected:
  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  Expression*
  do_copy()
  { return new Unknown_expression(this->named_object_, this->location()); }

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  // The unknown name.
  Named_object* named_object_;
  // True if we should not give errors if this is undefined.  This is
  // used if there was a parse failure.
  bool no_error_message_;
  // True if this is the key in a composite literal.
  bool is_composite_literal_key_;
};

// An index expression.  This is lowered to an array index, a string
// index, or a map index.

class Index_expression : public Parser_expression
{
 public:
  Index_expression(Expression* left, Expression* start, Expression* end,
		   Location location)
    : Parser_expression(EXPRESSION_INDEX, location),
      left_(left), start_(start), end_(end), is_lvalue_(false)
  { }

  // Record that this expression is an lvalue.
  void
  set_is_lvalue()
  { this->is_lvalue_ = true; }

  // Dump an index expression, i.e. an expression of the form
  // expr[expr] or expr[expr:expr], to a dump context.
  static void
  dump_index_expression(Ast_dump_context*, const Expression* expr, 
                        const Expression* start, const Expression* end);

 protected:
  int
  do_traverse(Traverse*);

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  Expression*
  do_copy()
  {
    return new Index_expression(this->left_->copy(), this->start_->copy(),
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

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The expression being indexed.
  Expression* left_;
  // The first index.
  Expression* start_;
  // The second index.  This is NULL for an index, non-NULL for a
  // slice.
  Expression* end_;
  // Whether this is being used as an l-value.  We set this during the
  // parse because map index expressions need to know.
  bool is_lvalue_;
};

// An index into a map.

class Map_index_expression : public Expression
{
 public:
  Map_index_expression(Expression* map, Expression* index,
		       Location location)
    : Expression(EXPRESSION_MAP_INDEX, location),
      map_(map), index_(index), is_lvalue_(false),
      is_in_tuple_assignment_(false)
  { }

  // Return the map.
  Expression*
  map()
  { return this->map_; }

  const Expression*
  map() const
  { return this->map_; }

  // Return the index.
  Expression*
  index()
  { return this->index_; }

  const Expression*
  index() const
  { return this->index_; }

  // Get the type of the map being indexed.
  Map_type*
  get_map_type() const;

  // Record that this map expression is an lvalue.  The difference is
  // that an lvalue always inserts the key.
  void
  set_is_lvalue()
  { this->is_lvalue_ = true; }

  // Return whether this map expression occurs in an assignment to a
  // pair of values.
  bool
  is_in_tuple_assignment() const
  { return this->is_in_tuple_assignment_; }

  // Record that this map expression occurs in an assignment to a pair
  // of values.
  void
  set_is_in_tuple_assignment()
  { this->is_in_tuple_assignment_ = true; }

  // Return a tree for the map index.  This returns a tree which
  // evaluates to a pointer to a value in the map.  If INSERT is true,
  // the key will be inserted if not present, and the value pointer
  // will be zero initialized.  If INSERT is false, and the key is not
  // present in the map, the pointer will be NULL.
  tree
  get_value_pointer(Translate_context*, bool insert);

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
    return Expression::make_map_index(this->map_->copy(),
				      this->index_->copy(),
				      this->location());
  }

  bool
  do_must_eval_subexpressions_in_order(int* skip) const
  {
    *skip = 1;
    return true;
  }

  // A map index expression is an lvalue but it is not addressable.

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The map we are looking into.
  Expression* map_;
  // The index.
  Expression* index_;
  // Whether this is an lvalue.
  bool is_lvalue_;
  // Whether this is in a tuple assignment to a pair of values.
  bool is_in_tuple_assignment_;
};

// An expression which represents a method bound to its first
// argument.

class Bound_method_expression : public Expression
{
 public:
  Bound_method_expression(Expression* expr, Named_object* method,
			  Location location)
    : Expression(EXPRESSION_BOUND_METHOD, location),
      expr_(expr), expr_type_(NULL), method_(method)
  { }

  // Return the object which is the first argument.
  Expression*
  first_argument()
  { return this->expr_; }

  // Return the implicit type of the first argument.  This will be
  // non-NULL when using a method from an anonymous field without
  // using an explicit stub.
  Type*
  first_argument_type() const
  { return this->expr_type_; }

  // Return the method function.
  Named_object*
  method()
  { return this->method_; }

  // Set the implicit type of the expression.
  void
  set_first_argument_type(Type* type)
  { this->expr_type_ = type; }

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
    return new Bound_method_expression(this->expr_->copy(), this->method_,
				       this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The object used to find the method.  This is passed to the method
  // as the first argument.
  Expression* expr_;
  // The implicit type of the object to pass to the method.  This is
  // NULL in the normal case, non-NULL when using a method from an
  // anonymous field which does not require a stub.
  Type* expr_type_;
  // The method itself.
  Named_object* method_;
};

// A reference to a field in a struct.

class Field_reference_expression : public Expression
{
 public:
  Field_reference_expression(Expression* expr, unsigned int field_index,
			     Location location)
    : Expression(EXPRESSION_FIELD_REFERENCE, location),
      expr_(expr), field_index_(field_index)
  { }

  // Return the struct expression.
  Expression*
  expr() const
  { return this->expr_; }

  // Return the field index.
  unsigned int
  field_index() const
  { return this->field_index_; }

  // Set the struct expression.  This is used when parsing.
  void
  set_struct_expression(Expression* expr)
  {
    go_assert(this->expr_ == NULL);
    this->expr_ = expr;
  }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->expr_, traverse); }

  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { this->expr_->determine_type_no_context(); }

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_field_reference(this->expr_->copy(),
					    this->field_index_,
					    this->location());
  }

  bool
  do_is_addressable() const
  { return this->expr_->is_addressable(); }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The expression we are looking into.  This should have a type of
  // struct.
  Expression* expr_;
  // The zero-based index of the field we are retrieving.
  unsigned int field_index_;
};

// A reference to a field of an interface.

class Interface_field_reference_expression : public Expression
{
 public:
  Interface_field_reference_expression(Expression* expr,
				       const std::string& name,
				       Location location)
    : Expression(EXPRESSION_INTERFACE_FIELD_REFERENCE, location),
      expr_(expr), name_(name)
  { }

  // Return the expression for the interface object.
  Expression*
  expr()
  { return this->expr_; }

  // Return the name of the method to call.
  const std::string&
  name() const
  { return this->name_; }

  // Return a tree for the pointer to the function to call, given a
  // tree for the expression.
  tree
  get_function_tree(Translate_context*, tree);

  // Return a tree for the first argument to pass to the interface
  // function, given a tree for the expression.  This is the real
  // object associated with the interface object.
  tree
  get_underlying_object_tree(Translate_context*, tree);

 protected:
  int
  do_traverse(Traverse* traverse);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_interface_field_reference(this->expr_->copy(),
						      this->name_,
						      this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The expression for the interface object.  This should have a type
  // of interface or pointer to interface.
  Expression* expr_;
  // The field we are retrieving--the name of the method.
  std::string name_;
};

// A type guard expression.

class Type_guard_expression : public Expression
{
 public:
  Type_guard_expression(Expression* expr, Type* type, Location location)
    : Expression(EXPRESSION_TYPE_GUARD, location),
      expr_(expr), type_(type)
  { }

  // Return the expression to convert.
  Expression*
  expr()
  { return this->expr_; }

  // Return the type to which to convert.
  Type*
  type()
  { return this->type_; }

 protected:
  int
  do_traverse(Traverse* traverse);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*)
  { this->expr_->determine_type_no_context(); }

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return new Type_guard_expression(this->expr_->copy(), this->type_,
				     this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The expression to convert.
  Expression* expr_;
  // The type to which to convert.
  Type* type_;
};

// A receive expression.

class Receive_expression : public Expression
{
 public:
  Receive_expression(Expression* channel, Location location)
    : Expression(EXPRESSION_RECEIVE, location),
      channel_(channel)
  { }

  // Return the channel.
  Expression*
  channel()
  { return this->channel_; }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->channel_, traverse); }

  void
  do_discarding_value()
  { }

  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { this->channel_->determine_type_no_context(); }

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_receive(this->channel_->copy(), this->location());
  }

  bool
  do_must_eval_in_order() const
  { return true; }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The channel from which we are receiving.
  Expression* channel_;
};

#endif // !defined(GO_EXPRESSIONS_H)
