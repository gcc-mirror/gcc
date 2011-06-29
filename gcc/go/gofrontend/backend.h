// backend.h -- Go frontend interface to backend  -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_BACKEND_H
#define GO_BACKEND_H

// Pointers to these types are created by the backend, passed to the
// frontend, and passed back to the backend.  The types must be
// defined by the backend using these names.

// The backend representation of a type.
class Btype;

// The backend represention of an expression.
class Bexpression;

// The backend representation of a statement.
class Bstatement;

// The backend representation of a function definition.
class Bfunction;

// The backend representation of a block.
class Bblock;

// The backend representation of a variable.
class Bvariable;

// The backend representation of a label.
class Blabel;

// The backend interface.  This is a pure abstract class that a
// specific backend will implement.

class Backend
{
 public:
  virtual ~Backend() { }

  // Name/type/location.  Used for function parameters, struct fields,
  // interface methods.
  struct Btyped_identifier
  {
    std::string name;
    Btype* btype;
    source_location location;

    Btyped_identifier()
      : name(), btype(NULL), location(UNKNOWN_LOCATION)
    { }

    Btyped_identifier(const std::string& a_name, Btype* a_btype,
		     source_location a_location)
      : name(a_name), btype(a_btype), location(a_location)
    { }
  };

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

  // Get an unnamed floating point type with the given number of bits
  // (32 or 64).
  virtual Btype*
  float_type(int bits) = 0;

  // Get an unnamed complex type with the given number of bits (64 or 128).
  virtual Btype*
  complex_type(int bits) = 0;

  // Get a pointer type.
  virtual Btype*
  pointer_type(Btype* to_type) = 0;

  // Get a function type.  The receiver, parameter, and results are
  // generated from the types in the Function_type.  The Function_type
  // is provided so that the names are available.
  virtual Btype*
  function_type(const Btyped_identifier& receiver,
		const std::vector<Btyped_identifier>& parameters,
		const std::vector<Btyped_identifier>& results,
		source_location location) = 0;

  // Get a struct type.
  virtual Btype*
  struct_type(const std::vector<Btyped_identifier>& fields) = 0;

  // Get an array type.
  virtual Btype*
  array_type(Btype* element_type, Bexpression* length) = 0;

  // Create a placeholder pointer type.  This is used for a named
  // pointer type, since in Go a pointer type may refer to itself.
  // NAME is the name of the type, and the location is where the named
  // type is defined.  This function is also used for unnamed function
  // types with multiple results, in which case the type has no name
  // and NAME will be empty.  FOR_FUNCTION is true if this is for a Go
  // function type, which corresponds to a C/C++ pointer to function
  // type.  The return value will later be passed as the first
  // parameter to set_placeholder_pointer_type or
  // set_placeholder_function_type.
  virtual Btype*
  placeholder_pointer_type(const std::string& name, source_location,
			   bool for_function) = 0;

  // Fill in a placeholder pointer type as a pointer.  This takes a
  // type returned by placeholder_pointer_type and arranges for it to
  // point to to_type.  Returns true on success, false on failure.
  virtual bool
  set_placeholder_pointer_type(Btype* placeholder, Btype* to_type) = 0;

  // Fill in a placeholder pointer type as a function.  This takes a
  // type returned by placeholder_pointer_type and arranges for it to
  // become a real Go function type (which corresponds to a C/C++
  // pointer to function type).  FT will be something returned by the
  // function_type method.  Returns true on success, false on failure.
  virtual bool
  set_placeholder_function_type(Btype* placeholder, Btype* ft) = 0;

  // Create a placeholder struct type.  This is used for a named
  // struct type, as with placeholder_pointer_type.
  virtual Btype*
  placeholder_struct_type(const std::string& name, source_location) = 0;

  // Fill in a placeholder struct type.  This takes a type returned by
  // placeholder_struct_type and arranges for it to become a real
  // struct type.  The parameter is as for struct_type.  Returns true
  // on success, false on failure.
  virtual bool
  set_placeholder_struct_type(Btype* placeholder,
			      const std::vector<Btyped_identifier>& fields)
  			= 0;

  // Create a placeholder array type.  This is used for a named array
  // type, as with placeholder_pointer_type, to handle cases like
  // type A []*A.
  virtual Btype*
  placeholder_array_type(const std::string& name, source_location) = 0;

  // Fill in a placeholder array type.  This takes a type returned by
  // placeholder_array_type and arranges for it to become a real array
  // type.  The parameters are as for array_type.  Returns true on
  // success, false on failure.
  virtual bool
  set_placeholder_array_type(Btype* placeholder, Btype* element_type,
			     Bexpression* length) = 0;

  // Return a named version of a type.  The location is the location
  // of the type definition.  This will not be called for a type
  // created via placeholder_pointer_type, placeholder_struct_type, or
  // placeholder_array_type..  (It may be called for a pointer,
  // struct, or array type in a case like "type P *byte; type Q P".)
  virtual Btype*
  named_type(const std::string& name, Btype*, source_location) = 0;

  // Create a marker for a circular pointer type.  Go pointer and
  // function types can refer to themselves in ways that are not
  // permitted in C/C++.  When a circular type is found, this function
  // is called for the circular reference.  This permits the backend
  // to decide how to handle such a type.  PLACEHOLDER is the
  // placeholder type which has already been created; if the backend
  // is prepared to handle a circular pointer type, it may simply
  // return PLACEHOLDER.  FOR_FUNCTION is true if this is for a
  // function type.
  //
  // For "type P *P" the sequence of calls will be
  //   bt1 = placeholder_pointer_type();
  //   bt2 = circular_pointer_type(bt1, false);
  //   set_placeholder_pointer_type(bt1, bt2);
  virtual Btype*
  circular_pointer_type(Btype* placeholder, bool for_function) = 0;

  // Return whether the argument could be a special type created by
  // circular_pointer_type.  This is used to introduce explicit type
  // conversions where needed.  If circular_pointer_type returns its
  // PLACEHOLDER parameter, this may safely always return false.
  virtual bool
  is_circular_pointer_type(Btype*) = 0;

  // Expressions.

  // Return an expression for a zero value of the given type.  This is
  // used for cases such as local variable initialization and
  // converting nil to other types.
  virtual Bexpression*
  zero_expression(Btype*) = 0;

  // Statements.

  // Create an error statement.  This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  virtual Bstatement*
  error_statement() = 0;

  // Create an expression statement.
  virtual Bstatement*
  expression_statement(Bexpression*) = 0;

  // Create a variable initialization statement.  This initializes a
  // local variable at the point in the program flow where it is
  // declared.
  virtual Bstatement*
  init_statement(Bvariable* var, Bexpression* init) = 0;

  // Create an assignment statement.
  virtual Bstatement*
  assignment_statement(Bexpression* lhs, Bexpression* rhs,
		       source_location) = 0;

  // Create a return statement, passing the representation of the
  // function and the list of values to return.
  virtual Bstatement*
  return_statement(Bfunction*, const std::vector<Bexpression*>&,
		   source_location) = 0;

  // Create an if statement.  ELSE_BLOCK may be NULL.
  virtual Bstatement*
  if_statement(Bexpression* condition, Bblock* then_block, Bblock* else_block,
	       source_location) = 0;

  // Create a switch statement where the case values are constants.
  // CASES and STATEMENTS must have the same number of entries.  If
  // VALUE matches any of the list in CASES[i], which will all be
  // integers, then STATEMENTS[i] is executed.  STATEMENTS[i] will
  // either end with a goto statement or will fall through into
  // STATEMENTS[i + 1].  CASES[i] is empty for the default clause,
  // which need not be last.
  virtual Bstatement*
  switch_statement(Bexpression* value,
		   const std::vector<std::vector<Bexpression*> >& cases,
		   const std::vector<Bstatement*>& statements,
		   source_location) = 0;

  // Create a single statement from two statements.
  virtual Bstatement*
  compound_statement(Bstatement*, Bstatement*) = 0;

  // Create a single statement from a list of statements.
  virtual Bstatement*
  statement_list(const std::vector<Bstatement*>&) = 0;

  // Blocks.

  // Create a block.  The frontend will call this function when it
  // starts converting a block within a function.  FUNCTION is the
  // current function.  ENCLOSING is the enclosing block; it will be
  // NULL for the top-level block in a function.  VARS is the list of
  // local variables defined within this block; each entry will be
  // created by the local_variable function.  START_LOCATION is the
  // location of the start of the block, more or less the location of
  // the initial curly brace.  END_LOCATION is the location of the end
  // of the block, more or less the location of the final curly brace.
  // The statements will be added after the block is created.
  virtual Bblock*
  block(Bfunction* function, Bblock* enclosing,
	const std::vector<Bvariable*>& vars,
	source_location start_location, source_location end_location) = 0;

  // Add the statements to a block.  The block is created first.  Then
  // the statements are created.  Then the statements are added to the
  // block.  This will called exactly once per block.  The vector may
  // be empty if there are no statements.
  virtual void
  block_add_statements(Bblock*, const std::vector<Bstatement*>&) = 0;

  // Return the block as a statement.  This is used to include a block
  // in a list of statements.
  virtual Bstatement*
  block_statement(Bblock*) = 0;

  // Variables.

  // Create an error variable.  This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  virtual Bvariable*
  error_variable() = 0;

  // Create a global variable.  PACKAGE_NAME is the name of the
  // package where the variable is defined.  UNIQUE_PREFIX is the
  // prefix for that package, from the -fgo-prefix option.  NAME is
  // the name of the variable.  BTYPE is the type of the variable.
  // IS_EXTERNAL is true if the variable is defined in some other
  // package.  IS_HIDDEN is true if the variable is not exported (name
  // begins with a lower case letter).  LOCATION is where the variable
  // was defined.
  virtual Bvariable*
  global_variable(const std::string& package_name,
		  const std::string& unique_prefix,
		  const std::string& name,
		  Btype* btype,
		  bool is_external,
		  bool is_hidden,
		  source_location location) = 0;

  // A global variable will 1) be initialized to zero, or 2) be
  // initialized to a constant value, or 3) be initialized in the init
  // function.  In case 2, the frontend will call
  // global_variable_set_init to set the initial value.  If this is
  // not called, the backend should initialize a global variable to 0.
  // The init function may then assign a value to it.
  virtual void
  global_variable_set_init(Bvariable*, Bexpression*) = 0;

  // Create a local variable.  The frontend will create the local
  // variables first, and then create the block which contains them.
  // FUNCTION is the function in which the variable is defined.  NAME
  // is the name of the variable.  TYPE is the type.  IS_ADDRESS_TAKEN
  // is true if the address of this variable is taken (this implies
  // that the address does not escape the function, as otherwise the
  // variable would be on the heap).  LOCATION is where the variable
  // is defined.  For each local variable the frontend will call
  // init_statement to set the initial value.
  virtual Bvariable*
  local_variable(Bfunction* function, const std::string& name, Btype* type,
		 bool is_address_taken, source_location location) = 0;

  // Create a function parameter.  This is an incoming parameter, not
  // a result parameter (result parameters are treated as local
  // variables).  The arguments are as for local_variable.
  virtual Bvariable*
  parameter_variable(Bfunction* function, const std::string& name,
		     Btype* type, bool is_address_taken,
		     source_location location) = 0;

  // Create a temporary variable.  A temporary variable has no name,
  // just a type.  We pass in FUNCTION and BLOCK in case they are
  // needed.  If INIT is not NULL, the variable should be initialized
  // to that value.  Otherwise the initial value is irrelevant--the
  // backend does not have to explicitly initialize it to zero.
  // ADDRESS_IS_TAKEN is true if the programs needs to take the
  // address of this temporary variable.  LOCATION is the location of
  // the statement or expression which requires creating the temporary
  // variable, and may not be very useful.  This function should
  // return a variable which can be referenced later and should set
  // *PSTATEMENT to a statement which initializes the variable.
  virtual Bvariable*
  temporary_variable(Bfunction*, Bblock*, Btype*, Bexpression* init,
		     bool address_is_taken, source_location location,
		     Bstatement** pstatement) = 0;

  // Create a named immutable initialized data structure.  This is
  // used for type descriptors and map descriptors.  This returns a
  // Bvariable because it corresponds to an initialized const global
  // variable in C.
  //
  // NAME is the name to use for the initialized global variable which
  // this call will create.
  //
  // IS_COMMON is true if NAME may be defined by several packages, and
  // the linker should merge all such definitions.  If IS_COMMON is
  // false, NAME should be defined in only one file.  In general
  // IS_COMMON will be true for the type descriptor of an unnamed type
  // or a builtin type.
  //
  // TYPE will be a struct type; the type of the returned expression
  // must be a pointer to this struct type.
  // 
  // We must create the named structure before we know its
  // initializer, because the initializer refer to its own address.
  // After calling this the frontend will call
  // set_immutable_struct_initializer.
  virtual Bvariable*
  immutable_struct(const std::string& name, bool is_common, Btype* type,
		   source_location) = 0;

  // Set the initial value of a variable created by immutable_struct.
  // The NAME, IS_COMMON, TYPE, and location parameters are the same
  // ones passed to immutable_struct.  INITIALIZER will be a composite
  // literal of type TYPE.  It will not contain any function calls or
  // anything else which can not be put into a read-only data section.
  // It may contain the address of variables created by
  // immutable_struct.
  virtual void
  immutable_struct_set_init(Bvariable*, const std::string& name,
			    bool is_common, Btype* type, source_location,
			    Bexpression* initializer) = 0;

  // Create a reference to a named immutable initialized data
  // structure defined in some other package.  This will be a
  // structure created by a call to immutable_struct_expression with
  // the same NAME and TYPE and with IS_COMMON passed as false.  This
  // corresponds to an extern const global variable in C.
  virtual Bvariable*
  immutable_struct_reference(const std::string& name, Btype* type,
			     source_location) = 0;

  // Labels.
  
  // Create a new label.  NAME will be empty if this is a label
  // created by the frontend for a loop construct.  The location is
  // where the the label is defined.
  virtual Blabel*
  label(Bfunction*, const std::string& name, source_location) = 0;

  // Create a statement which defines a label.  This statement will be
  // put into the codestream at the point where the label should be
  // defined.
  virtual Bstatement*
  label_definition_statement(Blabel*) = 0;

  // Create a goto statement to a label.
  virtual Bstatement*
  goto_statement(Blabel*, source_location) = 0;

  // Create an expression for the address of a label.  This is used to
  // get the return address of a deferred function which may call
  // recover.
  virtual Bexpression*
  label_address(Blabel*, source_location) = 0;
};

// The backend interface has to define this function.

extern Backend* go_get_backend();

// FIXME: Temporary helper functions while converting to new backend
// interface.

extern Btype* tree_to_type(tree);
extern Bexpression* tree_to_expr(tree);
extern Bstatement* tree_to_stat(tree);
extern Bfunction* tree_to_function(tree);
extern Bblock* tree_to_block(tree);
extern tree type_to_tree(Btype*);
extern tree expr_to_tree(Bexpression*);
extern tree stat_to_tree(Bstatement*);
extern tree block_to_tree(Bblock*);
extern tree var_to_tree(Bvariable*);

#endif // !defined(GO_BACKEND_H)
