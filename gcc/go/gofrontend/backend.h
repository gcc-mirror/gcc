// backend.h -- Go frontend interface to backend  -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_BACKEND_H
#define GO_BACKEND_H

class Function_type;
class Struct_type;
class Interface_type;

// Pointers to these types are created by the backend, passed to the
// frontend, and passed back to the backend.  The types must be
// defined by the backend using these names.

// The backend representation of a type.
class Btype;

// The backend represention of an expression.
class Bexpression;

// The backend representation of a statement.
class Bstatement;

// A list of backend types.
typedef std::vector<Btype*> Btypes;

// The backend interface.  This is a pure abstract class that a
// specific backend will implement.

class Backend
{
 public:
  virtual ~Backend() { }

  // Types.

  // Produce an error type.  Actually the backend could probably just
  // crash if this is called.
  virtual Btype*
  error_type() = 0;

  // Get a void type.  This is used in (at least) two ways: 1) as the
  // return type of a function with no result parameters; 2)
  // unsafe.Pointer is represented as *void.
  virtual Btype*
  void_type() = 0;

  // Get the unnamed boolean type.
  virtual Btype*
  bool_type() = 0;

  // Get an unnamed integer type with the given signedness and number
  // of bits.
  virtual Btype*
  integer_type(bool is_unsigned, int bits) = 0;

  // Get an unnamed floating point type with the given number of bits.
  virtual Btype*
  float_type(int bits) = 0;

  // Get the unnamed string type.
  virtual Btype*
  string_type() = 0;

  // Get a function type.  The receiver, parameter, and results are
  // generated from the types in the Function_type.  The Function_type
  // is provided so that the names are available.
  virtual Btype*
  function_type(const Function_type*, Btype* receiver,
		const Btypes* parameters,
		const Btypes* results) = 0;

  // Get a struct type.  The Struct_type is provided to get the field
  // names.
  virtual Btype*
  struct_type(const Struct_type*, const Btypes* field_types) = 0;

  // Get an array type.
  virtual Btype*
  array_type(const Btype* element_type, const Bexpression* length) = 0;

  // Get a slice type.
  virtual Btype*
  slice_type(const Btype* element_type) = 0;

  // Get a map type.
  virtual Btype*
  map_type(const Btype* key_type, const Btype* value_type, source_location) = 0;

  // Get a channel type.
  virtual Btype*
  channel_type(const Btype* element_type) = 0;

  // Get an interface type.  The Interface_type is provided to get the
  // method names.
  virtual Btype*
  interface_type(const Interface_type*, const Btypes* method_types) = 0;

  // Statements.

  // Create an assignment statement.
  virtual Bstatement*
  assignment(Bexpression* lhs, Bexpression* rhs, source_location location) = 0;
};

// The backend interface has to define this function.

extern Backend* go_get_backend();

// FIXME: Temporary helper functions while converting to new backend
// interface.

extern Bexpression* tree_to_expr(tree);
extern tree statement_to_tree(Bstatement*);

#endif // !defined(GO_BACKEND_H)
