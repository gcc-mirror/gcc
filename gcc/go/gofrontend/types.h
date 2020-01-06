// types.h -- Go frontend types.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_TYPES_H
#define GO_TYPES_H

#include <ostream>

#include "go-linemap.h"
#include "escape.h"

class Gogo;
class Package;
class Variable;
class Traverse;
class Typed_identifier;
class Typed_identifier_list;
class Integer_type;
class Float_type;
class Complex_type;
class String_type;
class Function_type;
class Backend_function_type;
class Struct_field;
class Struct_field_list;
class Struct_type;
class Pointer_type;
class Array_type;
class Map_type;
class Channel_type;
class Interface_type;
class Named_type;
class Forward_declaration_type;
class Method;
class Methods;
class Type_hash_identical;
class Type_identical;
class Expression;
class Expression_list;
class Call_expression;
class Field_reference_expression;
class Bound_method_expression;
class Bindings;
class Named_object;
class Function;
class Translate_context;
class Export;
class Import;
class Btype;
class Bexpression;
class Bvariable;

// Type codes used in type descriptors.  These must match the values
// in libgo/runtime/go-type.h.  They also match the values in the gc
// compiler in src/cmd/gc/reflect.c and src/pkg/runtime/type.go,
// although this is not required.

static const int RUNTIME_TYPE_KIND_BOOL = 1;
static const int RUNTIME_TYPE_KIND_INT = 2;
static const int RUNTIME_TYPE_KIND_INT8 = 3;
static const int RUNTIME_TYPE_KIND_INT16 = 4;
static const int RUNTIME_TYPE_KIND_INT32 = 5;
static const int RUNTIME_TYPE_KIND_INT64 = 6;
static const int RUNTIME_TYPE_KIND_UINT = 7;
static const int RUNTIME_TYPE_KIND_UINT8 = 8;
static const int RUNTIME_TYPE_KIND_UINT16 = 9;
static const int RUNTIME_TYPE_KIND_UINT32 = 10;
static const int RUNTIME_TYPE_KIND_UINT64 = 11;
static const int RUNTIME_TYPE_KIND_UINTPTR = 12;
static const int RUNTIME_TYPE_KIND_FLOAT32 = 13;
static const int RUNTIME_TYPE_KIND_FLOAT64 = 14;
static const int RUNTIME_TYPE_KIND_COMPLEX64 = 15;
static const int RUNTIME_TYPE_KIND_COMPLEX128 = 16;
static const int RUNTIME_TYPE_KIND_ARRAY = 17;
static const int RUNTIME_TYPE_KIND_CHAN = 18;
static const int RUNTIME_TYPE_KIND_FUNC = 19;
static const int RUNTIME_TYPE_KIND_INTERFACE = 20;
static const int RUNTIME_TYPE_KIND_MAP = 21;
static const int RUNTIME_TYPE_KIND_PTR = 22;
static const int RUNTIME_TYPE_KIND_SLICE = 23;
static const int RUNTIME_TYPE_KIND_STRING = 24;
static const int RUNTIME_TYPE_KIND_STRUCT = 25;
static const int RUNTIME_TYPE_KIND_UNSAFE_POINTER = 26;

static const int RUNTIME_TYPE_KIND_DIRECT_IFACE = (1 << 5);
static const int RUNTIME_TYPE_KIND_GC_PROG = (1 << 6);
static const int RUNTIME_TYPE_KIND_NO_POINTERS = (1 << 7);

// To build the complete list of methods for a named type we need to
// gather all methods from anonymous fields.  Those methods may
// require an arbitrary set of indirections and field offsets.  There
// is also the possibility of ambiguous methods, which we could ignore
// except that we want to give a better error message for that case.
// This is a base class.  There are two types of methods: named
// methods, and methods which are inherited from an anonymous field of
// interface type.

class Method
{
 public:
  // For methods in anonymous types we need to know the sequence of
  // field references used to extract the pointer to pass to the
  // method.  Since each method for a particular anonymous field will
  // have the sequence of field indexes, and since the indexes can be
  // shared going down the chain, we use a manually managed linked
  // list.  The first entry in the list is the field index for the
  // last field, the one passed to the method.

  struct Field_indexes
  {
    const Field_indexes* next;
    unsigned int field_index;
  };

  virtual ~Method()
  { }

  // Get the list of field indexes.
  const Field_indexes*
  field_indexes() const
  { return this->field_indexes_; }

  // Get the depth.
  unsigned int
  depth() const
  { return this->depth_; }

  // Return whether this is a value method--a method which does not
  // require a pointer expression.
  bool
  is_value_method() const
  { return this->is_value_method_; }

  // Return whether we need a stub method--this is true if we can't
  // just pass the main object to the method.
  bool
  needs_stub_method() const
  { return this->needs_stub_method_; }

  // Return whether this is an ambiguous method name.
  bool
  is_ambiguous() const
  { return this->is_ambiguous_; }

  // Note that this method is ambiguous.
  void
  set_is_ambiguous()
  { this->is_ambiguous_ = true; }

  // Return the type of the method.
  Function_type*
  type() const
  { return this->do_type(); }

  // Return the location of the method receiver.
  Location
  receiver_location() const
  { return this->do_receiver_location(); }

  // Return an expression which binds this method to EXPR.  This is
  // something which can be used with a function call.
  Expression*
  bind_method(Expression* expr, Location location) const;

  // Return the named object for this method.  This may only be called
  // after methods are finalized.
  Named_object*
  named_object() const;

  // Get the stub object.
  Named_object*
  stub_object() const
  {
    go_assert(this->stub_ != NULL);
    return this->stub_;
  }

  // Set the stub object.
  void
  set_stub_object(Named_object* no)
  {
    go_assert(this->stub_ == NULL);
    this->stub_ = no;
  }

  // Get the direct interface method stub object.
  Named_object*
  iface_stub_object() const
  {
    go_assert(this->iface_stub_ != NULL);
    return this->iface_stub_;
  }

  // Set the direct interface method stub object.
  void
  set_iface_stub_object(Named_object* no)
  {
    go_assert(this->iface_stub_ == NULL);
    this->iface_stub_ = no;
  }

  // Return true if this method should not participate in any
  // interfaces.
  bool
  nointerface() const
  { return this->do_nointerface(); }

 protected:
  // These objects are only built by the child classes.
  Method(const Field_indexes* field_indexes, unsigned int depth,
	 bool is_value_method, bool needs_stub_method)
    : field_indexes_(field_indexes), depth_(depth), stub_(NULL), iface_stub_(NULL),
      is_value_method_(is_value_method), needs_stub_method_(needs_stub_method),
      is_ambiguous_(false)
  { }

  // The named object for this method.
  virtual Named_object*
  do_named_object() const = 0;

  // The type of the method.
  virtual Function_type*
  do_type() const = 0;

  // Return the location of the method receiver.
  virtual Location
  do_receiver_location() const = 0;

  // Bind a method to an object.
  virtual Expression*
  do_bind_method(Expression* expr, Location location) const = 0;

  // Return whether this method should not participate in interfaces.
  virtual bool
  do_nointerface() const = 0;

 private:
  // The sequence of field indexes used for this method.  If this is
  // NULL, then the method is defined for the current type.
  const Field_indexes* field_indexes_;
  // The depth at which this method was found.
  unsigned int depth_;
  // If a stub method is required, this is its object.  This is only
  // set after stub methods are built in finalize_methods.
  Named_object* stub_;
  // Stub object for direct interface type.  This is only set after
  // stub methods are built in finalize_methods.
  Named_object* iface_stub_;
  // Whether this is a value method--a method that does not require a
  // pointer.
  bool is_value_method_;
  // Whether a stub method is required.
  bool needs_stub_method_;
  // Whether this method is ambiguous.
  bool is_ambiguous_;
};

// A named method.  This is what you get with a method declaration,
// either directly on the type, or inherited from some anonymous
// embedded field.

class Named_method : public Method
{
 public:
  Named_method(Named_object* named_object, const Field_indexes* field_indexes,
	       unsigned int depth, bool is_value_method,
	       bool needs_stub_method)
    : Method(field_indexes, depth, is_value_method, needs_stub_method),
      named_object_(named_object)
  { }

 protected:
  // Get the Named_object for the method.
  Named_object*
  do_named_object() const
  { return this->named_object_; }

  // The type of the method.
  Function_type*
  do_type() const;

  // Return the location of the method receiver.
  Location
  do_receiver_location() const;

  // Bind a method to an object.
  Expression*
  do_bind_method(Expression* expr, Location location) const;

  // Return whether this method should not participate in interfaces.
  bool
  do_nointerface() const;

 private:
  // The method itself.  For a method which needs a stub, this starts
  // out as the underlying method, and is later replaced with the stub
  // method.
  Named_object* named_object_;
};

// An interface method.  This is used when an interface appears as an
// anonymous field in a named struct.

class Interface_method : public Method
{
 public:
  Interface_method(const std::string& name, Location location,
		   Function_type* fntype, const Field_indexes* field_indexes,
		   unsigned int depth)
    : Method(field_indexes, depth, true, true),
      name_(name), location_(location), fntype_(fntype)
  { }

 protected:
  // Get the Named_object for the method.  This should never be
  // called, as we always create a stub.
  Named_object*
  do_named_object() const
  { go_unreachable(); }

  // The type of the method.
  Function_type*
  do_type() const
  { return this->fntype_; }

  // Return the location of the method receiver.
  Location
  do_receiver_location() const
  { return this->location_; }

  // Bind a method to an object.
  Expression*
  do_bind_method(Expression* expr, Location location) const;

  // Return whether this method should not participate in interfaces.
  bool
  do_nointerface() const
  { return false; }

 private:
  // The name of the interface method to call.
  std::string name_;
  // The location of the definition of the interface method.
  Location location_;
  // The type of the interface method.
  Function_type* fntype_;
};

// A mapping from method name to Method.  This is a wrapper around a
// hash table.

class Methods
{
 private:
  typedef Unordered_map(std::string, Method*) Method_map;

 public:
  typedef Method_map::const_iterator const_iterator;

  Methods()
    : methods_()
  { }

  // Insert a new method.  Returns true if it was inserted, false if
  // it was overidden or ambiguous.
  bool
  insert(const std::string& name, Method* m);

  // The number of (unambiguous) methods.
  size_t
  count() const;

  // Iterate.
  const_iterator
  begin() const
  { return this->methods_.begin(); }

  const_iterator
  end() const
  { return this->methods_.end(); }

  // Lookup.
  const_iterator
  find(const std::string& name) const
  { return this->methods_.find(name); }

  bool
  empty() const
  { return this->methods_.empty(); }

 private:
  Method_map methods_;
};

// The base class for all types.

class Type
{
 public:
  // The types of types.
  enum Type_classification
  {
    TYPE_ERROR,
    TYPE_VOID,
    TYPE_BOOLEAN,
    TYPE_INTEGER,
    TYPE_FLOAT,
    TYPE_COMPLEX,
    TYPE_STRING,
    TYPE_SINK,
    TYPE_FUNCTION,
    TYPE_POINTER,
    TYPE_NIL,
    TYPE_CALL_MULTIPLE_RESULT,
    TYPE_STRUCT,
    TYPE_ARRAY,
    TYPE_MAP,
    TYPE_CHANNEL,
    TYPE_INTERFACE,
    TYPE_NAMED,
    TYPE_FORWARD
  };

  virtual ~Type();

  // Creators.

  static Type*
  make_error_type();

  static Type*
  make_void_type();

  // Get the unnamed bool type.
  static Type*
  make_boolean_type();

  // Get the named type "bool".
  static Named_type*
  lookup_bool_type();

  // Make the named type "bool".
  static Named_type*
  make_named_bool_type();

  // Make an abstract integer type.
  static Integer_type*
  make_abstract_integer_type();

  // Make an abstract type for a character constant.
  static Integer_type*
  make_abstract_character_type();

  // Make a named integer type with a specified size.
  // RUNTIME_TYPE_KIND is the code to use in reflection information,
  // to distinguish int and int32.
  static Named_type*
  make_integer_type(const char* name, bool is_unsigned, int bits,
		    int runtime_type_kind);

  // Look up a named integer type.
  static Named_type*
  lookup_integer_type(const char* name);

  // Make an abstract floating point type.
  static Float_type*
  make_abstract_float_type();

  // Make a named floating point type with a specific size.
  // RUNTIME_TYPE_KIND is the code to use in reflection information,
  // to distinguish float and float32.
  static Named_type*
  make_float_type(const char* name, int bits, int runtime_type_kind);

  // Look up a named float type.
  static Named_type*
  lookup_float_type(const char* name);

  // Make an abstract complex type.
  static Complex_type*
  make_abstract_complex_type();

  // Make a named complex type with a specific size.
  // RUNTIME_TYPE_KIND is the code to use in reflection information,
  // to distinguish complex and complex64.
  static Named_type*
  make_complex_type(const char* name, int bits, int runtime_type_kind);

  // Look up a named complex type.
  static Named_type*
  lookup_complex_type(const char* name);

  // Get the unnamed string type.
  static Type*
  make_string_type();

  // Get the named type "string".
  static Named_type*
  lookup_string_type();

  // Make the named type "string".
  static Named_type*
  make_named_string_type();

  static Type*
  make_sink_type();

  static Function_type*
  make_function_type(Typed_identifier* receiver,
		     Typed_identifier_list* parameters,
		     Typed_identifier_list* results,
		     Location);

  static Backend_function_type*
  make_backend_function_type(Typed_identifier* receiver,
                             Typed_identifier_list* parameters,
                             Typed_identifier_list* results,
                             Location);

  static Pointer_type*
  make_pointer_type(Type*);

  static void
  finish_pointer_types(Gogo* gogo);

  static Type*
  make_nil_type();

  static Type*
  make_call_multiple_result_type(Call_expression*);

  static Struct_type*
  make_struct_type(Struct_field_list* fields, Location);

  static Array_type*
  make_array_type(Type* element_type, Expression* length);

  static Map_type*
  make_map_type(Type* key_type, Type* value_type, Location);

  static Channel_type*
  make_channel_type(bool send, bool receive, Type*);

  static Interface_type*
  make_interface_type(Typed_identifier_list* methods, Location);

  static Interface_type*
  make_empty_interface_type(Location);

  static Type*
  make_type_descriptor_type();

  static Type*
  make_type_descriptor_ptr_type();

  static Named_type*
  make_named_type(Named_object*, Type*, Location);

  static Type*
  make_forward_declaration(Named_object*);

  // Make a builtin struct type from a list of fields.
  static Struct_type*
  make_builtin_struct_type(int nfields, ...);

  // Make a builtin named type.
  static Named_type*
  make_builtin_named_type(const char* name, Type* type);

  // Traverse a type.
  static int
  traverse(Type*, Traverse*);

  // Verify the type.  This is called after parsing, and verifies that
  // types are complete and meet the language requirements.  This
  // returns false if the type is invalid and we should not continue
  // traversing it.
  bool
  verify()
  { return this->do_verify(); }

  // Bit flags to pass to are_identical and friends.

  // Treat error types as their own distinct type.  Sometimes we
  // ignore error types--treat them as identical to every other
  // type--to avoid cascading errors.
  static const int COMPARE_ERRORS = 1;

  // Compare struct field tags when comparing structs.  We ignore
  // struct field tags for purposes of type conversion.
  static const int COMPARE_TAGS = 2;

  // Compare aliases: treat an alias to T as distinct from T.
  static const int COMPARE_ALIASES = 4;

  // When comparing interface types compare the interface embedding heirarchy,
  // if any, rather than only comparing method sets. Useful primarily when
  // exporting types.
  static const int COMPARE_EMBEDDED_INTERFACES = 8;

  // Return true if two types are identical.  If this returns false,
  // and REASON is not NULL, it may set *REASON.
  static bool
  are_identical(const Type* lhs, const Type* rhs, int flags,
		std::string* reason);

  // Return true if two types are compatible for use in a binary
  // operation, other than a shift, comparison, or channel send.  This
  // is an equivalence relation.
  static bool
  are_compatible_for_binop(const Type* t1, const Type* t2);

  // Return true if two types are compatible for use with the
  // comparison operator.  IS_EQUALITY_OP is true if this is an
  // equality comparison, false if it is an ordered comparison.  This
  // is an equivalence relation.  If this returns false, and REASON is
  // not NULL, it sets *REASON.
  static bool
  are_compatible_for_comparison(bool is_equality_op, const Type *t1,
				const Type *t2, std::string* reason);

  // Return true if a type is comparable with itself.  This is true of
  // most types, but false for, e.g., function types.
  bool
  is_comparable() const
  { return Type::are_compatible_for_comparison(true, this, this, NULL); }

  // Return true if a value with type RHS is assignable to a variable
  // with type LHS.  This is not an equivalence relation.  If this
  // returns false, and REASON is not NULL, it sets *REASON.
  static bool
  are_assignable(const Type* lhs, const Type* rhs, std::string* reason);

  // Return true if a value with type RHS may be converted to type
  // LHS.  If this returns false, and REASON is not NULL, it sets
  // *REASON.
  static bool
  are_convertible(const Type* lhs, const Type* rhs, std::string* reason);

  // Return true if values of this type can be compared using an
  // identity function which gets nothing but a pointer to the value
  // and a size.
  bool
  compare_is_identity(Gogo* gogo)
  { return this->do_compare_is_identity(gogo); }

  // Return whether values of this type are reflexive: if a comparison
  // of a value with itself always returns true.
  bool
  is_reflexive()
  { return this->do_is_reflexive(); }

  // Return whether values of this, when used as a key in map,
  // requires the key to be updated when an assignment is made.
  bool
  needs_key_update()
  { return this->do_needs_key_update(); }

  // Return whether the hash function of this type might panic.  This
  // is only called for types used as a key in a map type.
  bool
  hash_might_panic()
  { return this->do_hash_might_panic(); }

  // Whether the type is permitted in the heap.
  bool
  in_heap()
  { return this->do_in_heap(); }

  // Return a hash code for this type for the method hash table.
  // Types which are equivalent according to are_identical will have
  // the same hash code.
  unsigned int
  hash_for_method(Gogo*, int) const;

  // Return the type classification.
  Type_classification
  classification() const
  { return this->classification_; }

  // Return the base type for this type.  This looks through forward
  // declarations and names.  Using this with a forward declaration
  // which has not been defined will return an error type.
  Type*
  base();

  const Type*
  base() const;

  // Return the type skipping defined forward declarations.  If this
  // type is a forward declaration which has not been defined, it will
  // return the Forward_declaration_type.  This differs from base() in
  // that it will return a Named_type, and for a
  // Forward_declaration_type which is not defined it will return that
  // type rather than an error type.
  Type*
  forwarded();

  const Type*
  forwarded() const;

  // Return the type skipping any alias definitions and any defined
  // forward declarations.  This is like forwarded, but also
  // recursively expands alias definitions to the aliased type.
  Type*
  unalias();

  const Type*
  unalias() const;

  // Return true if this is a basic type: a type which is not composed
  // of other types, and is not void.
  bool
  is_basic_type() const;

  // Return true if this is an abstract type--an integer, floating
  // point, or complex type whose size has not been determined.
  bool
  is_abstract() const;

  // Return a non-abstract version of an abstract type.
  Type*
  make_non_abstract_type();

  // Return true if this type is or contains a pointer.  This
  // determines whether the garbage collector needs to look at a value
  // of this type.
  bool
  has_pointer() const
  { return this->do_has_pointer(); }

  // Return true if this is the error type.  This returns false for a
  // type which is not defined, as it is called by the parser before
  // all types are defined.
  bool
  is_error_type() const;

  // Return true if this is the error type or if the type is
  // undefined.  If the type is undefined, this will give an error.
  // This should only be called after parsing is complete.
  bool
  is_error() const
  { return this->base()->is_error_type(); }

  // Return true if this is a void type.
  bool
  is_void_type() const
  { return this->classification_ == TYPE_VOID; }

  // If this is an integer type, return the Integer_type.  Otherwise,
  // return NULL.  This is a controlled dynamic_cast.
  Integer_type*
  integer_type()
  { return this->convert<Integer_type, TYPE_INTEGER>(); }

  const Integer_type*
  integer_type() const
  { return this->convert<const Integer_type, TYPE_INTEGER>(); }

  // If this is a floating point type, return the Float_type.
  // Otherwise, return NULL.  This is a controlled dynamic_cast.
  Float_type*
  float_type()
  { return this->convert<Float_type, TYPE_FLOAT>(); }

  const Float_type*
  float_type() const
  { return this->convert<const Float_type, TYPE_FLOAT>(); }

  // If this is a complex type, return the Complex_type.  Otherwise,
  // return NULL.
  Complex_type*
  complex_type()
  { return this->convert<Complex_type, TYPE_COMPLEX>(); }

  const Complex_type*
  complex_type() const
  { return this->convert<const Complex_type, TYPE_COMPLEX>(); }

  // Return whether this is a numeric type.
  bool
  is_numeric_type() const
  {
    Type_classification tc = this->base()->classification_;
    return tc == TYPE_INTEGER || tc == TYPE_FLOAT || tc == TYPE_COMPLEX;
  }

  // Return true if this is a boolean type.
  bool
  is_boolean_type() const
  { return this->base()->classification_ == TYPE_BOOLEAN; }

  // Return true if this is an abstract boolean type.
  bool
  is_abstract_boolean_type() const
  { return this->classification_ == TYPE_BOOLEAN; }

  // Return true if this is a string type.
  bool
  is_string_type() const
  { return this->base()->classification_ == TYPE_STRING; }

  // Return true if this is an abstract string type.
  bool
  is_abstract_string_type() const
  { return this->classification_ == TYPE_STRING; }

  // Return true if this is the sink type.  This is the type of the
  // blank identifier _.
  bool
  is_sink_type() const
  { return this->base()->classification_ == TYPE_SINK; }

  // If this is a function type, return it.  Otherwise, return NULL.
  Function_type*
  function_type()
  { return this->convert<Function_type, TYPE_FUNCTION>(); }

  const Function_type*
  function_type() const
  { return this->convert<const Function_type, TYPE_FUNCTION>(); }

  // If this is a pointer type, return the type to which it points.
  // Otherwise, return NULL.
  Type*
  points_to() const;

  // If this is a pointer type, return the type to which it points.
  // Otherwise, return the type itself.
  Type*
  deref()
  {
    Type* pt = this->points_to();
    return pt != NULL ? pt : this;
  }

  const Type*
  deref() const
  {
    const Type* pt = this->points_to();
    return pt != NULL ? pt : this;
  }

  // Return true if this is the nil type.  We don't use base() here,
  // because this can be called during parse, and there is no way to
  // name the nil type anyhow.
  bool
  is_nil_type() const
  { return this->classification_ == TYPE_NIL; }

  // Return true if this is the predeclared constant nil being used as
  // a type.  This is what the parser produces for type switches which
  // use "case nil".
  bool
  is_nil_constant_as_type() const;

  // Return true if this is the return type of a function which
  // returns multiple values.
  bool
  is_call_multiple_result_type() const
  { return this->base()->classification_ == TYPE_CALL_MULTIPLE_RESULT; }

  // If this is a struct type, return it.  Otherwise, return NULL.
  Struct_type*
  struct_type()
  { return this->convert<Struct_type, TYPE_STRUCT>(); }

  const Struct_type*
  struct_type() const
  { return this->convert<const Struct_type, TYPE_STRUCT>(); }

  // If this is an array type, return it.  Otherwise, return NULL.
  Array_type*
  array_type()
  { return this->convert<Array_type, TYPE_ARRAY>(); }

  const Array_type*
  array_type() const
  { return this->convert<const Array_type, TYPE_ARRAY>(); }

  // Return whether if this is a slice type.
  bool
  is_slice_type() const;

  // If this is a map type, return it.  Otherwise, return NULL.
  Map_type*
  map_type()
  { return this->convert<Map_type, TYPE_MAP>(); }

  const Map_type*
  map_type() const
  { return this->convert<const Map_type, TYPE_MAP>(); }

  // If this is a channel type, return it.  Otherwise, return NULL.
  Channel_type*
  channel_type()
  { return this->convert<Channel_type, TYPE_CHANNEL>(); }

  const Channel_type*
  channel_type() const
  { return this->convert<const Channel_type, TYPE_CHANNEL>(); }

  // If this is an interface type, return it.  Otherwise, return NULL.
  Interface_type*
  interface_type()
  { return this->convert<Interface_type, TYPE_INTERFACE>(); }

  const Interface_type*
  interface_type() const
  { return this->convert<const Interface_type, TYPE_INTERFACE>(); }

  // If this is a named type, return it.  Otherwise, return NULL.
  Named_type*
  named_type();

  const Named_type*
  named_type() const;

  // If this is a forward declaration, return it.  Otherwise, return
  // NULL.
  Forward_declaration_type*
  forward_declaration_type()
  { return this->convert_no_base<Forward_declaration_type, TYPE_FORWARD>(); }

  const Forward_declaration_type*
  forward_declaration_type() const
  {
    return this->convert_no_base<const Forward_declaration_type,
				 TYPE_FORWARD>();
  }

  // Return true if this type is not yet defined.
  bool
  is_undefined() const;

  // Return true if this is the unsafe.pointer type.  We currently
  // represent that as pointer-to-void.
  bool
  is_unsafe_pointer_type() const
  { return this->points_to() != NULL && this->points_to()->is_void_type(); }

  // Return whether this type is stored directly in an interface's
  // data word.
  bool
  is_direct_iface_type() const;

  // Return a version of this type with any expressions copied, but
  // only if copying the expressions will affect the size of the type.
  // If there are no such expressions in the type (expressions can
  // only occur in array types), just return the same type.  If any
  // expressions can not affect the size of the type, just return the
  // same type.
  Type*
  copy_expressions();

  // Look for field or method NAME for TYPE.  Return an expression for
  // it, bound to EXPR.
  static Expression*
  bind_field_or_method(Gogo*, const Type* type, Expression* expr,
		       const std::string& name, Location);

  // Return true if NAME is an unexported field or method of TYPE.
  static bool
  is_unexported_field_or_method(Gogo*, const Type*, const std::string&,
				std::vector<const Named_type*>*);

  // Convert the builtin named types.
  static void
  convert_builtin_named_types(Gogo*);

  // Return the backend representation of this type.
  Btype*
  get_backend(Gogo*);

  // Return a placeholder for the backend representation of the type.
  // This will return a type of the correct size, but for which some
  // of the fields may still need to be completed.
  Btype*
  get_backend_placeholder(Gogo*);

  // Finish the backend representation of a placeholder.
  void
  finish_backend(Gogo*, Btype*);

  // Build a type descriptor entry for this type.  Return a pointer to
  // it.  The location is the location which causes us to need the
  // entry.
  Bexpression*
  type_descriptor_pointer(Gogo* gogo, Location);

  // Build the Garbage Collection symbol for this type.  Return a pointer to it.
  Bexpression*
  gc_symbol_pointer(Gogo* gogo);

  // Return whether this type needs a garbage collection program.
  // Sets *PTRSIZE and *PTRDATA.
  bool
  needs_gcprog(Gogo*, int64_t* ptrsize, int64_t* ptrdata);

  // Return a ptrmask variable for this type.
  Bvariable*
  gc_ptrmask_var(Gogo*, int64_t ptrsize, int64_t ptrdata);

  // Return the type reflection string for this type.
  std::string
  reflection(Gogo*) const;

  // Return a mangled name for the type.  This is a name which can be
  // used in assembler code.  Identical types should have the same
  // manged name.
  std::string
  mangled_name(Gogo*) const;

  // If the size of the type can be determined, set *PSIZE to the size
  // in bytes and return true.  Otherwise, return false.  This queries
  // the backend.
  bool
  backend_type_size(Gogo*, int64_t* psize);

  // If the alignment of the type can be determined, set *PALIGN to
  // the alignment in bytes and return true.  Otherwise, return false.
  bool
  backend_type_align(Gogo*, int64_t* palign);

  // If the alignment of a struct field of this type can be
  // determined, set *PALIGN to the alignment in bytes and return
  // true.  Otherwise, return false.
  bool
  backend_type_field_align(Gogo*, int64_t* palign);

  // Determine the ptrdata size for the backend version of this type:
  // the length of the prefix of the type that can contain a pointer
  // value.  If it can be determined, set *PPTRDATA to the value in
  // bytes and return true.  Otherwise, return false.
  bool
  backend_type_ptrdata(Gogo*, int64_t* pptrdata);

  // Determine the ptrdata size that we are going to set in the type
  // descriptor.  This is normally the same as backend_type_ptrdata,
  // but differs if we use a gcprog for an array.  The arguments and
  // results are as for backend_type_ptrdata.
  bool
  descriptor_ptrdata(Gogo*, int64_t* pptrdata);

  // Whether the backend size is known.
  bool
  is_backend_type_size_known(Gogo*);

  // Return whether the type needs specially built type functions.
  bool
  needs_specific_type_functions(Gogo*);

  // Get the equality function for a type.  Returns NULL if the type
  // is not comparable.
  Named_object*
  equal_function(Gogo*, Named_type* name, Function_type* equal_fntype);

  // Get the hash function for a type.  Returns NULL if the type is
  // not comparable.
  Named_object*
  hash_function(Gogo*, Function_type* hash_fntype);

  // Write the equal function for a type.
  void
  write_equal_function(Gogo*, Named_type*, int64_t size,
		       const std::string& equal_name,
		       Function_type* equal_fntype);

  // Write the hash function for a type.
  void
  write_hash_function(Gogo*, int64_t size, const std::string& hash_name,
		      Function_type* hash_fntype);

  // Return the alignment required by the memequalN function.
  static int64_t memequal_align(Gogo*, int size);

  // Export the type.
  void
  export_type(Export* exp) const
  { this->do_export(exp); }

  // Import a type.
  static Type*
  import_type(Import*);

 protected:
  Type(Type_classification);

  // Functions implemented by the child class.

  // Traverse the subtypes.
  virtual int
  do_traverse(Traverse*);

  // Verify the type.
  virtual bool
  do_verify()
  { return true; }

  virtual bool
  do_has_pointer() const
  { return false; }

  virtual bool
  do_compare_is_identity(Gogo*) = 0;

  virtual bool
  do_is_reflexive()
  { return true; }

  virtual bool
  do_needs_key_update()
  { return false; }

  virtual bool
  do_hash_might_panic()
  { return false; }

  virtual bool
  do_in_heap()
  { return true; }

  virtual unsigned int
  do_hash_for_method(Gogo*, int) const;

  virtual Btype*
  do_get_backend(Gogo*) = 0;

  virtual Expression*
  do_type_descriptor(Gogo*, Named_type* name) = 0;

  virtual void
  do_reflection(Gogo*, std::string*) const = 0;

  virtual void
  do_mangled_name(Gogo*, std::string*) const = 0;

  virtual void
  do_export(Export*) const;

  // Return whether a method expects a pointer as the receiver.
  static bool
  method_expects_pointer(const Named_object*);

  // Finalize the methods for a type.
  static void
  finalize_methods(Gogo*, const Type*, Location, Methods**);

  // Return a method from a set of methods.
  static Method*
  method_function(const Methods*, const std::string& name,
		  bool* is_ambiguous);

  // A mapping from interfaces to the associated interface method
  // tables for this type.  This maps to a decl.
  typedef Unordered_map_hash(Interface_type*, Expression*, Type_hash_identical,
			     Type_identical) Interface_method_tables;

  // Return a pointer to the interface method table for TYPE for the
  // interface INTERFACE.
  static Expression*
  interface_method_table(Type* type,
			 Interface_type *interface, bool is_pointer,
			 Interface_method_tables** method_tables,
			 Interface_method_tables** pointer_tables);

  // Return a composite literal for the type descriptor entry for a
  // type.
  static Expression*
  type_descriptor(Gogo*, Type*);

  // Return a composite literal for the type descriptor entry for
  // TYPE, using NAME as the name of the type.
  static Expression*
  named_type_descriptor(Gogo*, Type* type, Named_type* name);

  // Return a composite literal for a plain type descriptor for this
  // type with the given kind and name.
  Expression*
  plain_type_descriptor(Gogo*, int runtime_type_kind, Named_type* name);

  // Build a composite literal for the basic type descriptor.
  Expression*
  type_descriptor_constructor(Gogo*, int runtime_type_kind, Named_type*,
			      const Methods*, bool only_value_methods);

  // For the benefit of child class reflection string generation.
  void
  append_reflection(const Type* type, Gogo* gogo, std::string* ret) const
  { type->do_reflection(gogo, ret); }

  // For the benefit of child class mangling.
  void
  append_mangled_name(const Type* type, Gogo* gogo, std::string* ret) const
  { type->do_mangled_name(gogo, ret); }

  // Return the backend representation for the underlying type of a
  // named type.
  static Btype*
  get_named_base_btype(Gogo* gogo, Type* base_type)
  { return base_type->get_btype_without_hash(gogo); }

 private:
  // Convert to the desired type classification, or return NULL.  This
  // is a controlled dynamic_cast.
  template<typename Type_class, Type_classification type_classification>
  Type_class*
  convert()
  {
    Type* base = this->base();
    return (base->classification_ == type_classification
	    ? static_cast<Type_class*>(base)
	    : NULL);
  }

  template<typename Type_class, Type_classification type_classification>
  const Type_class*
  convert() const
  {
    const Type* base = this->base();
    return (base->classification_ == type_classification
	    ? static_cast<Type_class*>(base)
	    : NULL);
  }

  template<typename Type_class, Type_classification type_classification>
  Type_class*
  convert_no_base()
  {
    return (this->classification_ == type_classification
	    ? static_cast<Type_class*>(this)
	    : NULL);
  }

  template<typename Type_class, Type_classification type_classification>
  const Type_class*
  convert_no_base() const
  {
    return (this->classification_ == type_classification
	    ? static_cast<Type_class*>(this)
	    : NULL);
  }

  // Map unnamed types to type descriptor decls.
  typedef Unordered_map_hash(const Type*, Bvariable*, Type_hash_identical,
			     Type_identical) Type_descriptor_vars;

  static Type_descriptor_vars type_descriptor_vars;

  // Build the type descriptor variable for this type.
  void
  make_type_descriptor_var(Gogo*);

  // Map unnamed types to type descriptor decls.
  typedef Unordered_map_hash(const Type*, Bvariable*, Type_hash_identical,
			     Type_identical) GC_symbol_vars;

  static GC_symbol_vars gc_symbol_vars;

  // Map ptrmask symbol names to the ptrmask variable.
  typedef Unordered_map(std::string, Bvariable*) GC_gcbits_vars;

  static GC_gcbits_vars gc_gcbits_vars;

  // Build the GC symbol for this type.
  void
  make_gc_symbol_var(Gogo*);

  // Return true if the type descriptor for this type should be
  // defined in some other package.  If NAME is not NULL, it is the
  // name of this type.  If this returns true it sets *PACKAGE to the
  // package where the type descriptor is defined.
  bool
  type_descriptor_defined_elsewhere(Named_type* name, const Package** package);

  // Make a composite literal for the garbage collection program for
  // this type.
  Expression*
  gcprog_constructor(Gogo*, int64_t ptrsize, int64_t ptrdata);

  // Build the hash function for a type that needs specific functions.
  Named_object*
  build_hash_function(Gogo*, int64_t size, Function_type* hash_fntype);

  // Build the equal function for a type that needs specific functions.
  Named_object*
  build_equal_function(Gogo*, Named_type*, int64_t size,
		       Function_type* equal_fntype);

  void
  write_identity_hash(Gogo*, int64_t size);

  void
  write_identity_equal(Gogo*, int64_t size);

  void
  write_named_equal(Gogo*, Named_type*);

  // Build a composite literal for the uncommon type information.
  Expression*
  uncommon_type_constructor(Gogo*, Type* uncommon_type,
			    Named_type*, const Methods*,
			    bool only_value_methods) const;

  // Build a composite literal for the methods.
  Expression*
  methods_constructor(Gogo*, Type* methods_type, const Methods*,
		      bool only_value_methods) const;

  // Build a composite literal for one method.
  Expression*
  method_constructor(Gogo*, Type* method_type, const std::string& name,
		     const Method*, bool only_value_methods) const;

  // Add all methods for TYPE to the list of methods for THIS.
  static void
  add_methods_for_type(const Type* type, const Method::Field_indexes*,
		       unsigned int depth, bool, bool,
		       std::vector<const Named_type*>*,
		       Methods*);

  static void
  add_local_methods_for_type(const Named_type* type,
			     const Method::Field_indexes*,
			     unsigned int depth, bool, bool, Methods*);

  static void
  add_embedded_methods_for_type(const Type* type,
				const Method::Field_indexes*,
				unsigned int depth, bool, bool,
				std::vector<const Named_type*>*,
				Methods*);

  static void
  add_interface_methods_for_type(const Type* type,
				 const Method::Field_indexes*,
				 unsigned int depth, Methods*);

  // Build stub methods for a type.
  static void
  build_stub_methods(Gogo*, const Type* type, const Methods* methods,
		     Location);

  static void
  build_one_stub_method(Gogo*, Method*, const char* receiver_name,
			const Typed_identifier_list*, bool is_varargs,
			Location);

  // Build direct interface stub methods for a type.
  static void
  build_direct_iface_stub_methods(Gogo*, const Type*, Methods*, Location);

  static void
  build_one_iface_stub_method(Gogo*, Method*, const char*,
                              const Typed_identifier_list*,
                              bool, Location);

  static Expression*
  apply_field_indexes(Expression*, const Method::Field_indexes*,
		      Location);

  // Look for a field or method named NAME in TYPE.
  static bool
  find_field_or_method(const Type* type, const std::string& name,
		       bool receiver_can_be_pointer,
		       std::vector<const Named_type*>*, int* level,
		       bool* is_method, bool* found_pointer_method,
		       std::string* ambig1, std::string* ambig2);

  // Helper function for is_direct_iface_type, to prevent infinite
  // recursion.
  bool
  is_direct_iface_type_helper(Unordered_set(const Type*)*) const;

  // Get the backend representation for a type without looking in the
  // hash table for identical types.
  Btype*
  get_btype_without_hash(Gogo*);

  // A backend type that may be a placeholder.
  struct Type_btype_entry
  {
    Btype *btype;
    bool is_placeholder;
  };

  // A mapping from Type to Btype*, used to ensure that the backend
  // representation of identical types is identical.  This is only
  // used for unnamed types.
  typedef Unordered_map_hash(const Type*, Type_btype_entry,
			     Type_hash_identical, Type_identical) Type_btypes;

  static Type_btypes type_btypes;

  // A list of builtin named types.
  static std::vector<Named_type*> named_builtin_types;

  // A map from types that need a specific hash or equality function
  // to the hash or equality function.
  typedef Unordered_map_hash(const Type*, Named_object*, Type_hash_identical,
			     Type_identical) Type_function;

  static Type_function type_hash_functions_table;
  static Type_function type_equal_functions_table;

  // Cache for reusing existing pointer types; maps from pointed-to-type
  // to pointer type.
  typedef Unordered_map(Type*, Pointer_type*) Pointer_type_table;

  static Pointer_type_table pointer_types;

  // List of placeholder pointer types.
  static std::vector<Type*> placeholder_pointers;

  // The type classification.
  Type_classification classification_;
  // The backend representation of the type, once it has been
  // determined.
  Btype* btype_;
  // The type descriptor for this type.  This starts out as NULL and
  // is filled in as needed.
  Bvariable* type_descriptor_var_;
  // The GC symbol for this type.  This starts out as NULL and
  // is filled in as needed.
  Bvariable* gc_symbol_var_;
  // Whether this type can appear in the heap.
  bool in_heap_;
};

// Type hash table operations, treating aliases as identical to the
// types that they alias.

class Type_hash_identical
{
 public:
  unsigned int
  operator()(const Type* type) const
  {
    return type->hash_for_method(NULL,
				 Type::COMPARE_ERRORS | Type::COMPARE_TAGS);
  }
};

class Type_identical
{
 public:
  bool
  operator()(const Type* t1, const Type* t2) const
  {
    return Type::are_identical(t1, t2,
			       Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			       NULL);
  }
};

// An identifier with a type.

class Typed_identifier
{
 public:
  Typed_identifier(const std::string& name, Type* type,
		   Location location)
    : name_(name), type_(type), location_(location), note_(NULL)
  { }

  // Get the name.
  const std::string&
  name() const
  { return this->name_; }

  // Get the type.
  Type*
  type() const
  { return this->type_; }

  // Return the location where the name was seen.  This is not always
  // meaningful.
  Location
  location() const
  { return this->location_; }

  // Set the type--sometimes we see the identifier before the type.
  void
  set_type(Type* type)
  {
    go_assert(this->type_ == NULL || type->is_error_type());
    this->type_ = type;
  }

  // Get the escape note.
  std::string*
  note() const
  { return this->note_; }

  // Set the escape note.
  void
  set_note(const std::string& note)
  {
    if (this->note_ != NULL)
      go_assert(*this->note_ == note);
    else
      this->note_ = new std::string(note);
  }

 private:
  // Identifier name.
  std::string name_;
  // Type.
  Type* type_;
  // The location where the name was seen.
  Location location_;
  // Escape note for this typed identifier.  Used when importing and exporting
  // functions.
  std::string* note_;
};

// A list of Typed_identifiers.

class Typed_identifier_list
{
 public:
  Typed_identifier_list()
    : entries_()
  { }

  // Whether the list is empty.
  bool
  empty() const
  { return this->entries_.empty(); }

  // Return the number of entries in the list.
  size_t
  size() const
  { return this->entries_.size(); }

  // Add an entry to the end of the list.
  void
  push_back(const Typed_identifier& td)
  { this->entries_.push_back(td); }

  // Remove an entry from the end of the list.
  void
  pop_back()
  { this->entries_.pop_back(); }

  // Set the type of entry I to TYPE.
  void
  set_type(size_t i, Type* type)
  {
    go_assert(i < this->entries_.size());
    this->entries_[i].set_type(type);
  }

  // Sort the entries by name.
  void
  sort_by_name();

  // Traverse types.
  int
  traverse(Traverse*) const;

  // Return the first and last elements.
  Typed_identifier&
  front()
  { return this->entries_.front(); }

  const Typed_identifier&
  front() const
  { return this->entries_.front(); }

  Typed_identifier&
  back()
  { return this->entries_.back(); }

  const Typed_identifier&
  back() const
  { return this->entries_.back(); }

  Typed_identifier&
  at(size_t i)
  { return this->entries_.at(i); }

  const Typed_identifier&
  at(size_t i) const
  { return this->entries_.at(i); }

  void
  set(size_t i, const Typed_identifier& t)
  { this->entries_.at(i) = t; }

  void
  resize(size_t c)
  {
    go_assert(c <= this->entries_.size());
    this->entries_.resize(c, Typed_identifier("", NULL,
                                              Linemap::unknown_location()));
  }

  void
  reserve(size_t c)
  { this->entries_.reserve(c); }

  // Iterators.

  typedef std::vector<Typed_identifier>::iterator iterator;
  typedef std::vector<Typed_identifier>::const_iterator const_iterator;

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

  // Return a copy of this list.  This returns an independent copy of
  // the vector, but does not copy the types.
  Typed_identifier_list*
  copy() const;

 private:
  std::vector<Typed_identifier> entries_;
};

// A type used to indicate a parsing error.  This exists to simplify
// later error detection.

class Error_type : public Type
{
 public:
  Error_type()
    : Type(TYPE_ERROR)
  { }

 protected:
  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  Btype*
  do_get_backend(Gogo* gogo);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string* ret) const;
};

// The void type.

class Void_type : public Type
{
 public:
  Void_type()
    : Type(TYPE_VOID)
  { }

 protected:
  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  Btype*
  do_get_backend(Gogo* gogo);

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  { go_unreachable(); }

  void
  do_reflection(Gogo*, std::string*) const
  { }

  void
  do_mangled_name(Gogo*, std::string* ret) const;
};

// The boolean type.

class Boolean_type : public Type
{
 public:
  Boolean_type()
    : Type(TYPE_BOOLEAN)
  { }

 protected:
  bool
  do_compare_is_identity(Gogo*)
  { return true; }

  Btype*
  do_get_backend(Gogo* gogo);

  Expression*
  do_type_descriptor(Gogo*, Named_type* name);

  // We should not be asked for the reflection string of a basic type.
  void
  do_reflection(Gogo*, std::string* ret) const
  { ret->append("bool"); }

  void
  do_mangled_name(Gogo*, std::string* ret) const;
};

// The type of an integer.

class Integer_type : public Type
{
 public:
  // Create a new integer type.
  static Named_type*
  create_integer_type(const char* name, bool is_unsigned, int bits,
		      int runtime_type_kind);

  // Look up an existing integer type.
  static Named_type*
  lookup_integer_type(const char* name);

  // Create an abstract integer type.
  static Integer_type*
  create_abstract_integer_type();

  // Create an abstract character type.
  static Integer_type*
  create_abstract_character_type();

  // Whether this is an abstract integer type.
  bool
  is_abstract() const
  { return this->is_abstract_; }

  // Whether this is an unsigned type.
  bool
  is_unsigned() const
  { return this->is_unsigned_; }

  // The number of bits.
  int
  bits() const
  { return this->bits_; }

  // Whether this type is the same as T.
  bool
  is_identical(const Integer_type* t) const;

  // Whether this is the type "byte" or another name for "byte".
  bool
  is_byte() const
  { return this->is_byte_; }

  // Mark this as the "byte" type.
  void
  set_is_byte()
  { this->is_byte_ = true; }

  // Whether this is the type "rune" or another name for "rune".
  bool
  is_rune() const
  { return this->is_rune_; }

  // Mark this as the "rune" type.
  void
  set_is_rune()
  { this->is_rune_ = true; }

protected:
  bool
  do_compare_is_identity(Gogo*)
  { return true; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

 private:
  Integer_type(bool is_abstract, bool is_unsigned, int bits,
	       int runtime_type_kind)
    : Type(TYPE_INTEGER),
      is_abstract_(is_abstract), is_unsigned_(is_unsigned), is_byte_(false),
      is_rune_(false), bits_(bits), runtime_type_kind_(runtime_type_kind)
  { }

  // Map names of integer types to the types themselves.
  typedef std::map<std::string, Named_type*> Named_integer_types;
  static Named_integer_types named_integer_types;

  // True if this is an abstract type.
  bool is_abstract_;
  // True if this is an unsigned type.
  bool is_unsigned_;
  // True if this is the byte type.
  bool is_byte_;
  // True if this is the rune type.
  bool is_rune_;
  // The number of bits.
  int bits_;
  // The runtime type code used in the type descriptor for this type.
  int runtime_type_kind_;
};

// The type of a floating point number.

class Float_type : public Type
{
 public:
  // Create a new float type.
  static Named_type*
  create_float_type(const char* name, int bits, int runtime_type_kind);

  // Look up an existing float type.
  static Named_type*
  lookup_float_type(const char* name);

  // Create an abstract float type.
  static Float_type*
  create_abstract_float_type();

  // Whether this is an abstract float type.
  bool
  is_abstract() const
  { return this->is_abstract_; }

  // The number of bits.
  int
  bits() const
  { return this->bits_; }

  // Whether this type is the same as T.
  bool
  is_identical(const Float_type* t) const;

 protected:
  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  bool
  do_is_reflexive()
  { return false; }

  // Distinction between +0 and -0 requires a key update.
  bool
  do_needs_key_update()
  { return true; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

 private:
  Float_type(bool is_abstract, int bits, int runtime_type_kind)
    : Type(TYPE_FLOAT),
      is_abstract_(is_abstract), bits_(bits),
      runtime_type_kind_(runtime_type_kind)
  { }

  // Map names of float types to the types themselves.
  typedef std::map<std::string, Named_type*> Named_float_types;
  static Named_float_types named_float_types;

  // True if this is an abstract type.
  bool is_abstract_;
  // The number of bits in the floating point value.
  int bits_;
  // The runtime type code used in the type descriptor for this type.
  int runtime_type_kind_;
};

// The type of a complex number.

class Complex_type : public Type
{
 public:
  // Create a new complex type.
  static Named_type*
  create_complex_type(const char* name, int bits, int runtime_type_kind);

  // Look up an existing complex type.
  static Named_type*
  lookup_complex_type(const char* name);

  // Create an abstract complex type.
  static Complex_type*
  create_abstract_complex_type();

  // Whether this is an abstract complex type.
  bool
  is_abstract() const
  { return this->is_abstract_; }

  // The number of bits: 64 or 128.
  int bits() const
  { return this->bits_; }

  // Whether this type is the same as T.
  bool
  is_identical(const Complex_type* t) const;

 protected:
  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  bool
  do_is_reflexive()
  { return false; }

  // Distinction between +0 and -0 requires a key update.
  bool
  do_needs_key_update()
  { return true; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

 private:
  Complex_type(bool is_abstract, int bits, int runtime_type_kind)
    : Type(TYPE_COMPLEX),
      is_abstract_(is_abstract), bits_(bits),
      runtime_type_kind_(runtime_type_kind)
  { }

  // Map names of complex types to the types themselves.
  typedef std::map<std::string, Named_type*> Named_complex_types;
  static Named_complex_types named_complex_types;

  // True if this is an abstract type.
  bool is_abstract_;
  // The number of bits in the complex value--64 or 128.
  int bits_;
  // The runtime type code used in the type descriptor for this type.
  int runtime_type_kind_;
};

// The type of a string.

class String_type : public Type
{
 public:
  String_type()
    : Type(TYPE_STRING)
  { }

 protected:
  bool
  do_has_pointer() const
  { return true; }

  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  // New string might have a smaller backing store.
  bool
  do_needs_key_update()
  { return true; }

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string* ret) const;

 private:
  // The named string type.
  static Named_type* string_type_;
};

// The type of a function.

class Function_type : public Type
{
 public:
  Function_type(Typed_identifier* receiver, Typed_identifier_list* parameters,
		Typed_identifier_list* results, Location location)
    : Type(TYPE_FUNCTION),
      receiver_(receiver), parameters_(parameters), results_(results),
      location_(location), is_varargs_(false), is_builtin_(false),
      fnbtype_(NULL), is_tagged_(false)
  { }

  // Get the receiver.
  const Typed_identifier*
  receiver() const
  { return this->receiver_; }

  // Add an escape note for the receiver.
  void
  add_receiver_note(int encoding)
  { this->receiver_->set_note(Escape_note::make_tag(encoding)); }

  // Get the return names and types.
  const Typed_identifier_list*
  results() const
  { return this->results_; }

  // Get the parameter names and types.
  const Typed_identifier_list*
  parameters() const
  { return this->parameters_; }

  // Add an escape note for the ith parameter.
  void
  add_parameter_note(int index, int encoding)
  { this->parameters_->at(index).set_note(Escape_note::make_tag(encoding)); }

  // Whether this function has been tagged during escape analysis.
  bool
  is_tagged() const
  { return this->is_tagged_; }

  // Mark this function as tagged after analyzing its escape.
  void
  set_is_tagged()
  { this->is_tagged_ = true; }

  // Whether this is a varargs function.
  bool
  is_varargs() const
  { return this->is_varargs_; }

  // Whether this is a builtin function.
  bool
  is_builtin() const
  { return this->is_builtin_; }

  // The location where this type was defined.
  Location
  location() const
  { return this->location_; }

  // Return whether this is a method type.
  bool
  is_method() const
  { return this->receiver_ != NULL; }

  // Whether T is a valid redeclaration of this type.  This is called
  // when a function is declared more than once.
  bool
  is_valid_redeclaration(const Function_type* t, std::string*) const;

  // Whether this type is the same as T.
  bool
  is_identical(const Function_type* t, bool ignore_receiver, int flags,
	       std::string*) const;

  // Record that this is a varargs function.
  void
  set_is_varargs()
  { this->is_varargs_ = true; }

  // Record that this is a builtin function.
  void
  set_is_builtin()
  { this->is_builtin_ = true; }

  // Import a function type.
  static Function_type*
  do_import(Import*);

  // Return a copy of this type without a receiver.  This is only
  // valid for a method type.
  Function_type*
  copy_without_receiver() const;

  // Return a copy of this type with a receiver.  This is used when an
  // interface method is attached to a named or struct type.
  Function_type*
  copy_with_receiver(Type*) const;

  // Return a copy of this type with the receiver treated as the first
  // parameter.  If WANT_POINTER_RECEIVER is true, the receiver is
  // forced to be a pointer.
  Function_type*
  copy_with_receiver_as_param(bool want_pointer_receiver) const;

  // Return a copy of this type ignoring any receiver and using dummy
  // names for all parameters.  This is used for thunks for method
  // values.
  Function_type*
  copy_with_names() const;

  static Type*
  make_function_type_descriptor_type();

  // Return the backend representation of this function type. This is used
  // as the real type of a backend function declaration or defintion.
  Btype*
  get_backend_fntype(Gogo*);

  // Return whether this is a Backend_function_type.
  virtual bool
  is_backend_function_type() const
  { return false; }

 protected:
  int
  do_traverse(Traverse*);

  // A function descriptor may be allocated on the heap.
  bool
  do_has_pointer() const
  { return true; }

  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  Expression*
  type_descriptor_params(Type*, const Typed_identifier*,
			 const Typed_identifier_list*);

  // A mapping from a list of result types to a backend struct type.
  class Results_hash
  {
  public:
    unsigned int
    operator()(const Typed_identifier_list*) const;
  };

  class Results_equal
  {
  public:
    bool
    operator()(const Typed_identifier_list*,
	       const Typed_identifier_list*) const;
  };

  typedef Unordered_map_hash(Typed_identifier_list*, Btype*,
			     Results_hash, Results_equal) Results_structs;

  static Results_structs results_structs;

  // The receiver name and type.  This will be NULL for a normal
  // function, non-NULL for a method.
  Typed_identifier* receiver_;
  // The parameter names and types.
  Typed_identifier_list* parameters_;
  // The result names and types.  This will be NULL if no result was
  // specified.
  Typed_identifier_list* results_;
  // The location where this type was defined.  This exists solely to
  // give a location for the fields of the struct if this function
  // returns multiple values.
  Location location_;
  // Whether this function takes a variable number of arguments.
  bool is_varargs_;
  // Whether this is a special builtin function which can not simply
  // be called.  This is used for len, cap, etc.
  bool is_builtin_;
  // The backend representation of this type for backend function
  // declarations and definitions.
  Btype* fnbtype_;
  // Whether this function has been analyzed by escape analysis.  If this is
  // TRUE, this function type's parameters contain a summary of the analysis.
  bool is_tagged_;
};

// The type of a function's backend representation.

class Backend_function_type : public Function_type
{
 public:
  Backend_function_type(Typed_identifier* receiver,
                        Typed_identifier_list* parameters,
                        Typed_identifier_list* results, Location location)
      : Function_type(receiver, parameters, results, location)
  { }

  // Return whether this is a Backend_function_type. This overrides
  // Function_type::is_backend_function_type.
  bool
  is_backend_function_type() const
  { return true; }

 protected:
  Btype*
  do_get_backend(Gogo* gogo)
  { return this->get_backend_fntype(gogo); }
};

// The type of a pointer.

class Pointer_type : public Type
{
 public:
  Pointer_type(Type* to_type)
    : Type(TYPE_POINTER),
      to_type_(to_type)
  {}

  Type*
  points_to() const
  { return this->to_type_; }

  // Import a pointer type.
  static Pointer_type*
  do_import(Import*);

  static Type*
  make_pointer_type_descriptor_type();

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_verify()
  { return this->to_type_->verify(); }

  bool
  do_has_pointer() const
  { return true; }

  bool
  do_compare_is_identity(Gogo*)
  { return true; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  // The type to which this type points.
  Type* to_type_;
};

// The nil type.  We use a special type for nil because it is not the
// same as any other type.  In C term nil has type void*, but there is
// no such type in Go.

class Nil_type : public Type
{
 public:
  Nil_type()
    : Type(TYPE_NIL)
  { }

 protected:
  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  Btype*
  do_get_backend(Gogo* gogo);

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  { go_unreachable(); }

  void
  do_reflection(Gogo*, std::string*) const
  { go_unreachable(); }

  void
  do_mangled_name(Gogo*, std::string* ret) const;
};

// The type of a field in a struct.

class Struct_field
{
 public:
  explicit Struct_field(const Typed_identifier& typed_identifier)
    : typed_identifier_(typed_identifier), tag_(NULL), is_imported_(false)
  { }

  // The field name.
  const std::string&
  field_name() const;

  // Return whether this struct field is named NAME.
  bool
  is_field_name(const std::string& name) const;

  // Return whether this struct field is an unexported field named NAME.
  bool
  is_unexported_field_name(Gogo*, const std::string& name) const;

  // Return whether this struct field is an embedded built-in type.
  bool
  is_embedded_builtin(Gogo*) const;

  // The field type.
  Type*
  type() const
  { return this->typed_identifier_.type(); }

  // The field location.
  Location
  location() const
  { return this->typed_identifier_.location(); }

  // Whether the field has a tag.
  bool
  has_tag() const
  { return this->tag_ != NULL; }

  // The tag.
  const std::string&
  tag() const
  {
    go_assert(this->tag_ != NULL);
    return *this->tag_;
  }

  // Whether this is an anonymous field.
  bool
  is_anonymous() const
  { return this->typed_identifier_.name().empty(); }

  // Set the tag.  FIXME: This is never freed.
  void
  set_tag(const std::string& tag)
  { this->tag_ = new std::string(tag); }

  // Record that this field is defined in an imported struct.
  void
  set_is_imported()
  { this->is_imported_ = true; }

  // Set the type.  This is only used in error cases.
  void
  set_type(Type* type)
  { this->typed_identifier_.set_type(type); }

 private:
  // The field name, type, and location.
  Typed_identifier typed_identifier_;
  // The field tag.  This is NULL if the field has no tag.
  std::string* tag_;
  // Whether this field is defined in an imported struct.
  bool is_imported_;
};

// A list of struct fields.

class Struct_field_list
{
 public:
  Struct_field_list()
    : entries_()
  { }

  // Whether the list is empty.
  bool
  empty() const
  { return this->entries_.empty(); }

  // Return the number of entries.
  size_t
  size() const
  { return this->entries_.size(); }

  // Add an entry to the end of the list.
  void
  push_back(const Struct_field& sf)
  { this->entries_.push_back(sf); }

  // Index into the list.
  const Struct_field&
  at(size_t i) const
  { return this->entries_.at(i); }

  // Last entry in list.
  Struct_field&
  back()
  { return this->entries_.back(); }

  // Iterators.

  typedef std::vector<Struct_field>::iterator iterator;
  typedef std::vector<Struct_field>::const_iterator const_iterator;

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

 private:
  std::vector<Struct_field> entries_;
};

// The type of a struct.

class Struct_type : public Type
{
 public:
  Struct_type(Struct_field_list* fields, Location location)
    : Type(TYPE_STRUCT),
      fields_(fields), location_(location), all_methods_(NULL),
      is_struct_incomparable_(false), has_padding_(false)
  { }

  // Return the field NAME.  This only looks at local fields, not at
  // embedded types.  If the field is found, and PINDEX is not NULL,
  // this sets *PINDEX to the field index.  If the field is not found,
  // this returns NULL.
  const Struct_field*
  find_local_field(const std::string& name, unsigned int *pindex) const;

  // Return the field number INDEX.
  const Struct_field*
  field(unsigned int index) const
  { return &this->fields_->at(index); }

  // Get the struct fields.
  const Struct_field_list*
  fields() const
  { return this->fields_; }

  // Return the number of fields.
  size_t
  field_count() const
  { return this->fields_->size(); }

  // Location of struct definition.
  Location
  location() const
  { return this->location_; }

  // Push a new field onto the end of the struct.  This is used when
  // building a closure variable.
  void
  push_field(const Struct_field& sf)
  { this->fields_->push_back(sf); }

  // Return an expression referring to field NAME in STRUCT_EXPR, or
  // NULL if there is no field with that name.
  Field_reference_expression*
  field_reference(Expression* struct_expr, const std::string& name,
		  Location) const;

  // Return the total number of fields, including embedded fields.
  // This is the number of values that can appear in a conversion to
  // this type.
  unsigned int
  total_field_count() const;

  // Whether this type is identical with T.
  bool
  is_identical(const Struct_type* t, int) const;

  // Return whether NAME is a local field which is not exported.  This
  // is only used for better error reporting.
  bool
  is_unexported_local_field(Gogo*, const std::string& name) const;

  // If this is an unnamed struct, build the complete list of methods,
  // including those from anonymous fields, and build methods stubs if
  // needed.
  void
  finalize_methods(Gogo*);

  // Return whether this type has any methods.  This should only be
  // called after the finalize_methods pass.
  bool
  has_any_methods() const
  { return this->all_methods_ != NULL; }

  // Return the methods for this type.  This should only be called
  // after the finalize_methods pass.
  const Methods*
  methods() const
  { return this->all_methods_; }

  // Return the method to use for NAME.  This returns NULL if there is
  // no such method or if the method is ambiguous.  When it returns
  // NULL, this sets *IS_AMBIGUOUS if the method name is ambiguous.
  Method*
  method_function(const std::string& name, bool* is_ambiguous) const;

  // Return a pointer to the interface method table for this type for
  // the interface INTERFACE.  If IS_POINTER is true, set the type
  // descriptor to a pointer to this type, otherwise set it to this
  // type.
  Expression*
  interface_method_table(Interface_type* interface, bool is_pointer);

  // Traverse just the field types of a struct type.
  int
  traverse_field_types(Traverse* traverse)
  { return this->do_traverse(traverse); }

  // If the offset of field INDEX in the backend implementation can be
  // determined, set *POFFSET to the offset in bytes and return true.
  // Otherwise, return false.
  bool
  backend_field_offset(Gogo*, unsigned int index, int64_t* poffset);

  // Finish the backend representation of all the fields.
  void
  finish_backend_fields(Gogo*);

  // Import a struct type.
  static Struct_type*
  do_import(Import*);

  static Type*
  make_struct_type_descriptor_type();

  // Return whether this is a generated struct that is not comparable.
  bool
  is_struct_incomparable() const
  { return this->is_struct_incomparable_; }

  // Record that this is a generated struct that is not comparable.
  void
  set_is_struct_incomparable()
  { this->is_struct_incomparable_ = true; }

  // Return whether this struct's backend type has padding, due to
  // trailing zero-sized field.
  bool
  has_padding() const
  { return this->has_padding_; }

  // Record that this struct's backend type has padding.
  void
  set_has_padding()
  { this->has_padding_ = true; }

  // Write the hash function for this type.
  void
  write_hash_function(Gogo*, Function_type*);

  // Write the equality function for this type.
  void
  write_equal_function(Gogo*, Named_type*);

  // Whether we can write this type to a C header file, to implement
  // -fgo-c-header.
  bool
  can_write_to_c_header(std::vector<const Named_object*>*,
			std::vector<const Named_object*>*) const;

  // Write this type to a C header file, to implement -fgo-c-header.
  void
  write_to_c_header(std::ostream&) const;

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_verify();

  bool
  do_has_pointer() const;

  bool
  do_compare_is_identity(Gogo*);

  bool
  do_is_reflexive();

  bool
  do_needs_key_update();

  bool
  do_hash_might_panic();

  bool
  do_in_heap();

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  bool
  can_write_type_to_c_header(const Type*,
			     std::vector<const Named_object*>*,
			     std::vector<const Named_object*>*) const;

  void
  write_field_to_c_header(std::ostream&, const std::string&, const Type*) const;

  // Used to merge method sets of identical unnamed structs.
  typedef Unordered_map_hash(Struct_type*, Struct_type*, Type_hash_identical,
			     Type_identical) Identical_structs;

  static Identical_structs identical_structs;

  // Used to manage method tables for identical unnamed structs.
  typedef std::pair<Interface_method_tables*, Interface_method_tables*>
    Struct_method_table_pair;

  typedef Unordered_map_hash(Struct_type*, Struct_method_table_pair*,
			     Type_hash_identical, Type_identical)
    Struct_method_tables;

  static Struct_method_tables struct_method_tables;

  // Used to avoid infinite loops in field_reference_depth.
  struct Saw_named_type
  {
    Saw_named_type* next;
    Named_type* nt;
  };

  Field_reference_expression*
  field_reference_depth(Expression* struct_expr, const std::string& name,
			Location, Saw_named_type*,
			unsigned int* depth) const;

  // The fields of the struct.
  Struct_field_list* fields_;
  // The place where the struct was declared.
  Location location_;
  // If this struct is unnamed, a list of methods.
  Methods* all_methods_;
  // True if this is a generated struct that is not considered to be
  // comparable.
  bool is_struct_incomparable_;
  // True if this struct's backend type has padding, due to trailing
  // zero-sized field.
  bool has_padding_;
};

// The type of an array.

class Array_type : public Type
{
 public:
  Array_type(Type* element_type, Expression* length)
    : Type(TYPE_ARRAY),
      element_type_(element_type), length_(length), blength_(NULL),
      issued_length_error_(false), is_array_incomparable_(false)
  { }

  // Return the element type.
  Type*
  element_type() const
  { return this->element_type_; }

  // Return the length.  This will return NULL for a slice.
  Expression*
  length() const
  { return this->length_; }

  // Store the length as an int64_t into *PLEN.  Return false if the
  // length can not be determined.  This will assert if called for a
  // slice.
  bool
  int_length(int64_t* plen) const;

  // Whether this type is identical with T.
  bool
  is_identical(const Array_type* t, int) const;

  // Return an expression for the pointer to the values in an array.
  Expression*
  get_value_pointer(Gogo*, Expression* array, bool is_lvalue) const;

  // Return an expression for the length of an array with this type.
  Expression*
  get_length(Gogo*, Expression* array) const;

  // Return an expression for the capacity of an array with this type.
  Expression*
  get_capacity(Gogo*, Expression* array) const;

  // Import an array type.
  static Array_type*
  do_import(Import*);

  // Return the backend representation of the element type.
  Btype*
  get_backend_element(Gogo*, bool use_placeholder);

  // Return the backend representation of the length.
  Bexpression*
  get_backend_length(Gogo*);

  // Finish the backend representation of the element type.
  void
  finish_backend_element(Gogo*);

  static Type*
  make_array_type_descriptor_type();

  static Type*
  make_slice_type_descriptor_type();

  // Return whether this is a generated array that is not comparable.
  bool
  is_array_incomparable() const
  { return this->is_array_incomparable_; }

  // Record that this is a generated array that is not comparable.
  void
  set_is_array_incomparable()
  { this->is_array_incomparable_ = true; }

  // Write the hash function for this type.
  void
  write_hash_function(Gogo*, Function_type*);

  // Write the equality function for this type.
  void
  write_equal_function(Gogo*, Named_type*);

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_verify();

  bool
  do_has_pointer() const;

  bool
  do_compare_is_identity(Gogo*);

  bool
  do_is_reflexive()
  {
    return this->length_ != NULL && this->element_type_->is_reflexive();
  }

  bool
  do_needs_key_update()
  { return this->element_type_->needs_key_update(); }

  bool
  do_hash_might_panic()
  { return this->length_ != NULL && this->element_type_->hash_might_panic(); }

  bool
  do_in_heap()
  { return this->length_ == NULL || this->element_type_->in_heap(); }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  bool
  verify_length();

  Expression*
  array_type_descriptor(Gogo*, Named_type*);

  Expression*
  slice_type_descriptor(Gogo*, Named_type*);

  // The type of elements of the array.
  Type* element_type_;
  // The number of elements.  This may be NULL.
  Expression* length_;
  // The backend representation of the length.
  // We only want to compute this once.
  Bexpression* blength_;
  // Whether or not an invalid length error has been issued for this type,
  // to avoid knock-on errors.
  mutable bool issued_length_error_;
  // True if this is a generated array that is not considered to be
  // comparable.
  bool is_array_incomparable_;
};

// The type of a map.

class Map_type : public Type
{
 public:
  Map_type(Type* key_type, Type* val_type, Location location)
    : Type(TYPE_MAP),
      key_type_(key_type), val_type_(val_type), hmap_type_(NULL),
      bucket_type_(NULL), hiter_type_(NULL), location_(location)
  { }

  // Return the key type.
  Type*
  key_type() const
  { return this->key_type_; }

  // Return the value type.
  Type*
  val_type() const
  { return this->val_type_; }

  // Return the type used for an iteration over this map.
  Type*
  hiter_type(Gogo*);

  // If this map requires the "fat" functions, returns the pointer to
  // pass as the zero value to those functions.  Otherwise, in the
  // normal case, returns NULL.
  Expression*
  fat_zero_value(Gogo*);

  // Map algorithm to use for this map type.  We may use specialized
  // fast map routines for certain key types.
  enum Map_alg
    {
      // 32-bit key.
      MAP_ALG_FAST32,
      // 32-bit pointer key.
      MAP_ALG_FAST32PTR,
      // 64-bit key.
      MAP_ALG_FAST64,
      // 64-bit pointer key.
      MAP_ALG_FAST64PTR,
      // String key.
      MAP_ALG_FASTSTR,
      // Anything else.
      MAP_ALG_SLOW,
    };

  Map_alg
  algorithm(Gogo*);

  // Return whether VAR is the map zero value.
  static bool
  is_zero_value(Variable* var);

  // Return the backend representation of the map zero value.
  static Bvariable*
  backend_zero_value(Gogo*);

  // Whether this type is identical with T.
  bool
  is_identical(const Map_type* t, int) const;

  // Import a map type.
  static Map_type*
  do_import(Import*);

  static Type*
  make_map_type_descriptor_type();

  // This must be in  sync with libgo/go/runtime/map.go.
  static const int bucket_size = 8;

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_verify();

  bool
  do_has_pointer() const
  { return true; }

  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  bool
  do_is_reflexive()
  {
    return this->key_type_->is_reflexive() && this->val_type_->is_reflexive();
  }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  // These must be in sync with libgo/go/runtime/map.go.
  static const int max_key_size = 128;
  static const int max_val_size = 128;
  static const int max_zero_size = 1024;

  // Maps with value types larger than max_zero_size require passing a
  // zero value pointer to the map functions.

  // The zero value variable.
  static Named_object* zero_value;

  // The current size of the zero value.
  static int64_t zero_value_size;

  // The current alignment of the zero value.
  static int64_t zero_value_align;

  Type*
  bucket_type(Gogo*, int64_t, int64_t);

  Type*
  hmap_type(Type*);

  // The key type.
  Type* key_type_;
  // The value type.
  Type* val_type_;
  // The hashmap type.  At run time a map is represented as a pointer
  // to this type.
  Type* hmap_type_;
  // The bucket type, the type used to hold keys and values at run time.
  Type* bucket_type_;
  // The iterator type.
  Type* hiter_type_;
  // Where the type was defined.
  Location location_;
};

// The type of a channel.

class Channel_type : public Type
{
 public:
  Channel_type(bool may_send, bool may_receive, Type* element_type)
    : Type(TYPE_CHANNEL),
      may_send_(may_send), may_receive_(may_receive),
      element_type_(element_type)
  { go_assert(may_send || may_receive); }

  // Whether this channel can send data.
  bool
  may_send() const
  { return this->may_send_; }

  // Whether this channel can receive data.
  bool
  may_receive() const
  { return this->may_receive_; }

  // The type of the values that may be sent on this channel.  This is
  // NULL if any type may be sent.
  Type*
  element_type() const
  { return this->element_type_; }

  // Whether this type is identical with T.
  bool
  is_identical(const Channel_type* t, int) const;

  // Import a channel type.
  static Channel_type*
  do_import(Import*);

  static Type*
  make_chan_type_descriptor_type();

  static Type*
  select_case_type();

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Type::traverse(this->element_type_, traverse); }

  bool
  do_verify();

  bool
  do_has_pointer() const
  { return true; }

  bool
  do_compare_is_identity(Gogo*)
  { return true; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  // Whether this channel can send data.
  bool may_send_;
  // Whether this channel can receive data.
  bool may_receive_;
  // The types of elements which may be sent on this channel.  If this
  // is NULL, it means that any type may be sent.
  Type* element_type_;
};

// An interface type.

class Interface_type : public Type
{
 public:
  Interface_type(Typed_identifier_list* methods, Location location)
    : Type(TYPE_INTERFACE),
      parse_methods_(methods), all_methods_(NULL), location_(location),
      package_(NULL), interface_btype_(NULL), bmethods_(NULL),
      assume_identical_(NULL), methods_are_finalized_(false),
      bmethods_is_placeholder_(false), seen_(false)
  { go_assert(methods == NULL || !methods->empty()); }

  // The location where the interface type was defined.
  Location
  location() const
  { return this->location_; }

  // The package where the interface type was defined.  Returns NULL
  // for the package currently being compiled.
  Package*
  package() const
  { return this->package_; }

  // Return whether this is an empty interface.
  bool
  is_empty() const
  {
    go_assert(this->methods_are_finalized_);
    return this->all_methods_ == NULL;
  }

  // Return the list of locally defined methods.  This will return NULL
  // for an empty interface.  Embedded interfaces will appear in this
  // list as an entry with no name.
  const Typed_identifier_list*
  local_methods() const
  { return this->parse_methods_; }

  // Return the list of all methods.  This will return NULL for an
  // empty interface.
  const Typed_identifier_list*
  methods() const;

  // Return the number of methods.
  size_t
  method_count() const;

  // Return the method NAME, or NULL.
  const Typed_identifier*
  find_method(const std::string& name) const;

  // Return the zero-based index of method NAME.
  size_t
  method_index(const std::string& name) const;

  // Finalize the methods.  This sets all_methods_.  This handles
  // interface inheritance.
  void
  finalize_methods();

  // Return true if T implements this interface.  If this returns
  // false, and REASON is not NULL, it sets *REASON to the reason that
  // it fails.
  bool
  implements_interface(const Type* t, std::string* reason) const;

  // Whether this type is identical with T.  REASON is as in
  // implements_interface.
  bool
  is_identical(const Interface_type* t, int) const;

  // Whether we can assign T to this type.  is_identical is known to
  // be false.
  bool
  is_compatible_for_assign(const Interface_type*, std::string* reason) const;

  // Return whether NAME is a method which is not exported.  This is
  // only used for better error reporting.
  bool
  is_unexported_method(Gogo*, const std::string& name) const;

  // Import an interface type.
  static Interface_type*
  do_import(Import*);

  // Make a struct for an empty interface type.
  static Btype*
  get_backend_empty_interface_type(Gogo*);

  // Get a pointer to the backend representation of the method table.
  Btype*
  get_backend_methods(Gogo*);

  // Return a placeholder for the backend representation of the
  // pointer to the method table.
  Btype*
  get_backend_methods_placeholder(Gogo*);

  // Finish the backend representation of the method types.
  void
  finish_backend_methods(Gogo*);

  static Type*
  make_interface_type_descriptor_type();

  // Return whether methods are finalized for this interface.
  bool
  methods_are_finalized() const
  { return this->methods_are_finalized_; }

  // Sort embedded interfaces by name. Needed when we are preparing
  // to emit types into the export data.
  void
  sort_embedded()
  {
    if (parse_methods_ != NULL)
      parse_methods_->sort_by_name();
  }

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_has_pointer() const
  { return true; }

  bool
  do_compare_is_identity(Gogo*)
  { return false; }

  // Not reflexive if it contains a float.
  bool
  do_is_reflexive()
  { return false; }

  // Distinction between +0 and -0 requires a key update if it
  // contains a float.
  bool
  do_needs_key_update()
  { return true; }

  // Hashing an unhashable type stored in an interface might panic.
  bool
  do_hash_might_panic()
  { return true; }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string*) const;

  void
  do_export(Export*) const;

 private:
  // This type guards against infinite recursion when comparing
  // interface types.  We keep a list of interface types assumed to be
  // identical during comparison.  We just keep the list on the stack.
  // This permits us to compare cases like
  // type I1 interface { F() interface{I1} }
  // type I2 interface { F() interface{I2} }
  struct Assume_identical
  {
    Assume_identical* next;
    const Interface_type* t1;
    const Interface_type* t2;
  };

  bool
  assume_identical(const Interface_type*, const Interface_type*) const;

  struct Bmethods_map_entry
  {
    Btype *btype;
    bool is_placeholder;
  };

  // A mapping from Interface_type to the backend type of its bmethods_,
  // used to ensure that the backend representation of identical types
  // is identical.
  typedef Unordered_map_hash(const Interface_type*, Bmethods_map_entry,
                             Type_hash_identical, Type_identical) Bmethods_map;

  static Bmethods_map bmethods_map;

  // The list of methods associated with the interface from the
  // parser.  This will be NULL for the empty interface.  This may
  // include unnamed interface types.
  Typed_identifier_list* parse_methods_;
  // The list of all methods associated with the interface.  This
  // expands any interface types listed in methods_.  It is set by
  // finalize_methods.  This will be NULL for the empty interface.
  Typed_identifier_list* all_methods_;
  // The location where the interface was defined.
  Location location_;
  // The package where the interface was defined.  This is NULL for
  // the package being compiled.
  Package* package_;
  // The backend representation of this type during backend conversion.
  Btype* interface_btype_;
  // The backend representation of the pointer to the method table.
  Btype* bmethods_;
  // A list of interface types assumed to be identical during
  // interface comparison.
  mutable Assume_identical* assume_identical_;
  // Whether the methods have been finalized.
  bool methods_are_finalized_;
  // Whether the bmethods_ field is a placeholder.
  bool bmethods_is_placeholder_;
  // Used to avoid endless recursion in do_mangled_name.
  mutable bool seen_;
};

// The value we keep for a named type.  This lets us get the right
// name when we convert to backend.  Note that we don't actually keep
// the name here; the name is in the Named_object which points to
// this.  This object exists to hold a unique backend representation for
// the type.

class Named_type : public Type
{
 public:
  Named_type(Named_object* named_object, Type* type, Location location)
    : Type(TYPE_NAMED),
      named_object_(named_object), in_function_(NULL), in_function_index_(0),
      type_(type), local_methods_(NULL), all_methods_(NULL),
      interface_method_tables_(NULL), pointer_interface_method_tables_(NULL),
      location_(location), named_btype_(NULL), dependencies_(),
      is_alias_(false), is_visible_(true), is_error_(false), in_heap_(true),
      is_placeholder_(false), is_converted_(false), is_verified_(false),
      seen_(false), seen_in_compare_is_identity_(false),
      seen_in_get_backend_(false), seen_alias_(false)
  { }

  // Return the associated Named_object.  This holds the actual name.
  Named_object*
  named_object()
  { return this->named_object_; }

  const Named_object*
  named_object() const
  { return this->named_object_; }

  // Set the Named_object.  This is used when we see a type
  // declaration followed by a type.
  void
  set_named_object(Named_object* no)
  { this->named_object_ = no; }

  // Whether this is an alias (type T1 = T2) rather than an ordinary
  // named type (type T1 T2).
  bool
  is_alias() const
  { return this->is_alias_; }

  // Record that this type is an alias.
  void
  set_is_alias()
  { this->is_alias_ = true; }

  // Mark this type as not permitted in the heap.
  void
  set_not_in_heap()
  { this->in_heap_ = false; }

  // Return the function in which this type is defined.  This will
  // return NULL for a type defined in global scope.
  const Named_object*
  in_function(unsigned int *pindex) const
  {
    *pindex = this->in_function_index_;
    return this->in_function_;
  }

  // Set the function in which this type is defined.
  void
  set_in_function(Named_object* f, unsigned int index)
  {
    this->in_function_ = f;
    this->in_function_index_ = index;
  }

  // Return the name of the type.
  const std::string&
  name() const;

  // Return the name of the type for an error message.  The difference
  // is that if the type is defined in a different package, this will
  // return PACKAGE.NAME.
  std::string
  message_name() const;

  // Return the underlying type.
  Type*
  real_type()
  { return this->type_; }

  const Type*
  real_type() const
  { return this->type_; }

  // Return the location.
  Location
  location() const
  { return this->location_; }

  // Whether this type is visible.  This only matters when parsing.
  bool
  is_visible() const
  { return this->is_visible_; }

  // Mark this type as visible.
  void
  set_is_visible()
  { this->is_visible_ = true; }

  // Mark this type as invisible.
  void
  clear_is_visible()
  { this->is_visible_ = false; }

  // Whether this is a builtin type.
  bool
  is_builtin() const
  { return Linemap::is_predeclared_location(this->location_); }

  // Whether this named type is valid.  A recursive named type is invalid.
  bool
  is_valid() const
  { return !this->is_error_; }

  // Return the base type for this type.
  Type*
  named_base();

  const Type*
  named_base() const;

  // Return whether this is an error type.
  bool
  is_named_error_type() const;

  // Return whether this type is comparable.  If REASON is not NULL,
  // set *REASON when returning false.
  bool
  named_type_is_comparable(std::string* reason) const;

  // Add a method to this type.
  Named_object*
  add_method(const std::string& name, Function*);

  // Add a method declaration to this type.
  Named_object*
  add_method_declaration(const std::string& name, Package* package,
			 Function_type* type, Location location);

  // Add an existing method--one defined before the type itself was
  // defined--to a type.
  void
  add_existing_method(Named_object*);

  // Look up a local method.
  Named_object*
  find_local_method(const std::string& name) const;

  // Return the list of local methods.
  const Bindings*
  local_methods() const;

  // Build the complete list of methods, including those from
  // anonymous fields, and build method stubs if needed.
  void
  finalize_methods(Gogo*);

  // Return whether this type has any methods.  This should only be
  // called after the finalize_methods pass.
  bool
  has_any_methods() const;

  // Return the methods for this type.  This should only be called
  // after the finalized_methods pass.
  const Methods*
  methods() const;

  // Return the method to use for NAME.  This returns NULL if there is
  // no such method or if the method is ambiguous.  When it returns
  // NULL, this sets *IS_AMBIGUOUS if the method name is ambiguous.
  Method*
  method_function(const std::string& name, bool *is_ambiguous) const;

  // Return whether NAME is a known field or method which is not
  // exported.  This is only used for better error reporting.
  bool
  is_unexported_local_method(Gogo*, const std::string& name) const;

  // Return a pointer to the interface method table for this type for
  // the interface INTERFACE.  If IS_POINTER is true, set the type
  // descriptor to a pointer to this type, otherwise set it to this
  // type.
  Expression*
  interface_method_table(Interface_type* interface, bool is_pointer);

  // Note that a type must be converted to the backend representation
  // before we convert this type.
  void
  add_dependency(Named_type* nt)
  { this->dependencies_.push_back(nt); }

  // Return true if the size and alignment of the backend
  // representation of this type is known.  This is always true after
  // types have been converted, but may be false beforehand.
  bool
  is_named_backend_type_size_known() const
  { return this->named_btype_ != NULL && !this->is_placeholder_; }

  // Add to the reflection string as for Type::append_reflection, but
  // if USE_ALIAS use the alias name rather than the alias target.
  void
  append_reflection_type_name(Gogo*, bool use_alias, std::string*) const;

  // Append the mangled type name as for Type::append_mangled_name,
  // but if USE_ALIAS use the alias name rather than the alias target.
  void
  append_mangled_type_name(Gogo*, bool use_alias, std::string*) const;

  // Import a named type.
  static void
  import_named_type(Import*, Named_type**);

  // Initial conversion to backend representation.
  void
  convert(Gogo*);

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Type::traverse(this->type_, traverse); }

  bool
  do_verify();

  bool
  do_has_pointer() const;

  bool
  do_compare_is_identity(Gogo*);

  bool
  do_is_reflexive();

  bool
  do_needs_key_update();

  bool
  do_in_heap()
  { return this->in_heap_ && this->type_->in_heap(); }

  unsigned int
  do_hash_for_method(Gogo*, int) const;

  Btype*
  do_get_backend(Gogo*);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string* ret) const;

  void
  do_export(Export*) const;

 private:
  // Create the placeholder during conversion.
  void
  create_placeholder(Gogo*);

  // A pointer back to the Named_object for this type.
  Named_object* named_object_;
  // If this type is defined in a function, a pointer back to the
  // function in which it is defined.
  Named_object* in_function_;
  // The index of this type in IN_FUNCTION_.
  unsigned int in_function_index_;
  // The actual type.
  Type* type_;
  // The list of methods defined for this type.  Any named type can
  // have methods.
  Bindings* local_methods_;
  // The full list of methods for this type, including methods
  // declared for anonymous fields.
  Methods* all_methods_;
  // A mapping from interfaces to the associated interface method
  // tables for this type.
  Interface_method_tables* interface_method_tables_;
  // A mapping from interfaces to the associated interface method
  // tables for pointers to this type.
  Interface_method_tables* pointer_interface_method_tables_;
  // The location where this type was defined.
  Location location_;
  // The backend representation of this type during backend
  // conversion.  This is used to avoid endless recursion when a named
  // type refers to itself.
  Btype* named_btype_;
  // A list of types which must be converted to the backend
  // representation before this type can be converted.  This is for
  // cases like
  //   type S1 { p *S2 }
  //   type S2 { s S1 }
  // where we can't convert S2 to the backend representation unless we
  // have converted S1.
  std::vector<Named_type*> dependencies_;
  // Whether this is an alias type.
  bool is_alias_;
  // Whether this type is visible.  This is false if this type was
  // created because it was referenced by an imported object, but the
  // type itself was not exported.  This will always be true for types
  // created in the current package.
  bool is_visible_;
  // Whether this type is erroneous.
  bool is_error_;
  // Whether this type is permitted in the heap.  This is true by
  // default, false if there is a magic //go:notinheap comment.
  bool in_heap_;
  // Whether the current value of named_btype_ is a placeholder for
  // which the final size of the type is not known.
  bool is_placeholder_;
  // Whether this type has been converted to the backend
  // representation.  Implies that is_placeholder_ is false.
  bool is_converted_;
  // Whether this type has been verified.
  bool is_verified_;
  // In a recursive operation such as has_pointer, this flag is used
  // to prevent infinite recursion when a type refers to itself.  This
  // is mutable because it is always reset to false when the function
  // exits.
  mutable bool seen_;
  // Like seen_, but used only by do_compare_is_identity.
  bool seen_in_compare_is_identity_;
  // Like seen_, but used only by do_get_backend.
  bool seen_in_get_backend_;
  // Like seen_, but used when resolving aliases.
  mutable bool seen_alias_;
};

// A forward declaration.  This handles a type which has been declared
// but not defined.

class Forward_declaration_type : public Type
{
 public:
  Forward_declaration_type(Named_object* named_object);

  // The named object associated with this type declaration.  This
  // will be resolved.
  Named_object*
  named_object();

  const Named_object*
  named_object() const;

  // Return the name of the type.
  const std::string&
  name() const;

  // Return the type to which this points.  Give an error if the type
  // has not yet been defined.
  Type*
  real_type();

  const Type*
  real_type() const;

  // Whether the base type has been defined.
  bool
  is_defined() const;

  // Add a method to this type.
  Named_object*
  add_method(const std::string& name, Function*);

  // Add a method declaration to this type.
  Named_object*
  add_method_declaration(const std::string& name, Package*, Function_type*,
			 Location);

  // Add an already created object as a method to this type.
  void
  add_existing_method(Named_object*);

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_verify();

  bool
  do_has_pointer() const
  { return this->real_type()->has_pointer(); }

  bool
  do_compare_is_identity(Gogo* gogo)
  { return this->real_type()->compare_is_identity(gogo); }

  bool
  do_is_reflexive()
  { return this->real_type()->is_reflexive(); }

  bool
  do_needs_key_update()
  { return this->real_type()->needs_key_update(); }

  bool
  do_in_heap()
  { return this->real_type()->in_heap(); }

  unsigned int
  do_hash_for_method(Gogo* gogo, int flags) const
  { return this->real_type()->hash_for_method(gogo, flags); }

  Btype*
  do_get_backend(Gogo* gogo);

  Expression*
  do_type_descriptor(Gogo*, Named_type*);

  void
  do_reflection(Gogo*, std::string*) const;

  void
  do_mangled_name(Gogo*, std::string* ret) const;

  void
  do_export(Export*) const;

 private:
  // Issue a warning about a use of an undefined type.
  void
  warn() const;

  // The type declaration.
  Named_object* named_object_;
  // Whether we have issued a warning about this type.
  mutable bool warned_;
};

// The Type_context struct describes what we expect for the type of an
// expression.

struct Type_context
{
  // The exact type we expect, if known.  This may be NULL.
  Type* type;
  // Whether an abstract type is permitted.
  bool may_be_abstract;

  // Constructors.
  Type_context()
    : type(NULL), may_be_abstract(false)
  { }

  Type_context(Type* a_type, bool a_may_be_abstract)
    : type(a_type), may_be_abstract(a_may_be_abstract)
  { }
};

#endif // !defined(GO_TYPES_H)
