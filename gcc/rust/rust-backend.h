// Copyright (C) 2020-2021 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_BACKEND_H
#define RUST_BACKEND_H

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>

#include "rust-location.h"
#include "rust-linemap.h"
#include "rust-diagnostics.h"
#include "operator.h"
#include "rust-abi.h"

#include "tree.h"

extern bool
saw_errors (void);

// TODO: Will have to be significantly modified to work with Rust and current
// setup of gccrs

// Pointers to these types are created by the backend, passed to the
// frontend, and passed back to the backend.  The types must be
// defined by the backend using these names.

// The backend representation of a variable.
class Bvariable;

// The backend interface.  This is a pure abstract class that a
// specific backend will implement.

class Backend
{
public:
  virtual ~Backend () {}

  // Name/type/location.  Used for function parameters, struct fields,
  // interface methods.
  struct typed_identifier
  {
    std::string name;
    tree type;
    Location location;

    typed_identifier ()
      : name (), type (NULL_TREE), location (Linemap::unknown_location ())
    {}

    typed_identifier (const std::string &a_name, tree a_type,
		      Location a_location)
      : name (a_name), type (a_type), location (a_location)
    {}
  };

  // debug
  virtual void debug (tree) = 0;
  virtual void debug (Bvariable *) = 0;

  // const folder helpers
  virtual bool const_size_cast (tree, size_t *) = 0;
  virtual std::string const_size_val_to_string (tree) = 0;
  virtual bool const_values_equal (tree, tree) = 0;

  static Rust::ABI get_abi_from_string (const std::string &abi, Location locus)
  {
    if (abi.compare ("rust") == 0)
      return Rust::ABI::C;
    else if (abi.compare ("rust-intrinsic") == 0)
      return Rust::ABI::INTRINSIC;
    else if (abi.compare ("C") == 0)
      return Rust::ABI::C;
    else if (abi.compare ("cdecl") == 0)
      return Rust::ABI::CDECL;
    else if (abi.compare ("stdcall") == 0)
      return Rust::ABI::STDCALL;
    else if (abi.compare ("fastcall") == 0)
      return Rust::ABI::FASTCALL;

    rust_error_at (locus, "unknown abi specified");

    return Rust::ABI::UNKNOWN;
  }

  static std::string get_string_from_abi (Rust::ABI abi)
  {
    switch (abi)
      {
      case Rust::ABI::RUST:
	return "rust";
      case Rust::ABI::INTRINSIC:
	return "rust-intrinsic";
      case Rust::ABI::C:
	return "C";
      case Rust::ABI::CDECL:
	return "cdecl";
      case Rust::ABI::STDCALL:
	return "stdcall";
      case Rust::ABI::FASTCALL:
	return "fastcall";

      case Rust::ABI::UNKNOWN:
	return "unknown";
      }
    return "unknown";
  }

  virtual tree get_identifier_node (const std::string &str) = 0;

  // Types.

  // Produce an error type.  Actually the backend could probably just
  // crash if this is called.
  virtual tree error_type () = 0;

  // Get a void type.  This is used in (at least) two ways: 1) as the
  // return type of a function with no result parameters; 2)
  // unsafe.Pointer is represented as *void.
  virtual tree void_type () = 0;

  // get unit-type
  virtual tree unit_type () = 0;

  // Get the unnamed boolean type.
  virtual tree bool_type () = 0;

  // Get the char type
  virtual tree char_type () = 0;

  // Get the wchar type
  virtual tree wchar_type () = 0;

  // Get the Host pointer size in bits
  virtual int get_pointer_size () = 0;

  // Get the raw str type const char*
  virtual tree raw_str_type () = 0;

  // Get an unnamed integer type with the given signedness and number
  // of bits.
  virtual tree integer_type (bool is_unsigned, int bits) = 0;

  // Get an unnamed floating point type with the given number of bits
  // (32 or 64).
  virtual tree float_type (int bits) = 0;

  // Get an unnamed complex type with the given number of bits (64 or 128).
  virtual tree complex_type (int bits) = 0;

  // Get a pointer type.
  virtual tree pointer_type (tree to_type) = 0;

  // Get a reference type.
  virtual tree reference_type (tree to_type) = 0;

  // make type immutable
  virtual tree immutable_type (tree base) = 0;

  // Get a function type.  The receiver, parameter, and results are
  // generated from the types in the Function_type.  The Function_type
  // is provided so that the names are available.  This should return
  // not the type of a Go function (which is a pointer to a struct)
  // but the type of a C function pointer (which will be used as the
  // type of the first field of the struct).  If there is more than
  // one result, RESULT_STRUCT is a struct type to hold the results,
  // and RESULTS may be ignored; if there are zero or one results,
  // RESULT_STRUCT is NULL.
  virtual tree function_type (const typed_identifier &receiver,
			      const std::vector<typed_identifier> &parameters,
			      const std::vector<typed_identifier> &results,
			      tree result_struct, Location location)
    = 0;

  virtual tree
  function_type_varadic (const typed_identifier &receiver,
			 const std::vector<typed_identifier> &parameters,
			 const std::vector<typed_identifier> &results,
			 tree result_struct, Location location)
    = 0;

  virtual tree function_ptr_type (tree result,
				  const std::vector<tree> &praameters,
				  Location location)
    = 0;

  // Get a struct type.
  virtual tree struct_type (const std::vector<typed_identifier> &fields) = 0;

  // Get a union type.
  virtual tree union_type (const std::vector<typed_identifier> &fields) = 0;

  // Get an array type.
  virtual tree array_type (tree element_type, tree length) = 0;

  // Return a named version of a type.  The location is the location
  // of the type definition.  This will not be called for a type
  // created via placeholder_pointer_type, placeholder_struct_type, or
  // placeholder_array_type..  (It may be called for a pointer,
  // struct, or array type in a case like "type P *byte; type Q P".)
  virtual tree named_type (const std::string &name, tree, Location) = 0;

  // Return the size of a type.
  virtual int64_t type_size (tree) = 0;

  // Return the alignment of a type.
  virtual int64_t type_alignment (tree) = 0;

  // Return the alignment of a struct field of this type.  This is
  // normally the same as type_alignment, but not always.
  virtual int64_t type_field_alignment (tree) = 0;

  // Return the offset of field INDEX in a struct type.  INDEX is the
  // entry in the FIELDS std::vector parameter of struct_type or
  // set_placeholder_struct_type.
  virtual int64_t type_field_offset (tree, size_t index) = 0;

  // Expressions.

  // Return an expression for a zero value of the given type.  This is
  // used for cases such as local variable initialization and
  // converting nil to other types.
  virtual tree zero_expression (tree) = 0;

  // Create an error expression. This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  virtual tree error_expression () = 0;

  // return whether this is error_mark_node
  virtual bool is_error_expression (tree) = 0;

  // Create a nil pointer expression.
  virtual tree nil_pointer_expression () = 0;

  virtual tree unit_expression () = 0;

  // Create a reference to a variable.
  virtual tree var_expression (Bvariable *var, Location) = 0;

  // Create an expression that indirects through the pointer expression EXPR
  // (i.e., return the expression for *EXPR). KNOWN_VALID is true if the pointer
  // is known to point to a valid memory location.  BTYPE is the expected type
  // of the indirected EXPR.
  virtual tree indirect_expression (tree btype, tree expr, bool known_valid,
				    Location)
    = 0;

  // Return an expression that declares a constant named NAME with the
  // constant value VAL in BTYPE.
  virtual tree named_constant_expression (tree btype, const std::string &name,
					  tree val, Location)
    = 0;

  // Return an expression for the multi-precision integer VAL in BTYPE.
  virtual tree integer_constant_expression (tree btype, mpz_t val) = 0;

  // Return an expression for the floating point value VAL in BTYPE.
  virtual tree float_constant_expression (tree btype, mpfr_t val) = 0;

  // Return an expression for the complex value VAL in BTYPE.
  virtual tree complex_constant_expression (tree btype, mpc_t val) = 0;

  // Return an expression for the string value VAL.
  virtual tree string_constant_expression (const std::string &val) = 0;

  // Get a char literal
  virtual tree char_constant_expression (char c) = 0;

  // Get a char literal
  virtual tree wchar_constant_expression (wchar_t c) = 0;

  // Return an expression for the boolean value VAL.
  virtual tree boolean_constant_expression (bool val) = 0;

  // Return an expression for the real part of BCOMPLEX.
  virtual tree real_part_expression (tree bcomplex, Location) = 0;

  // Return an expression for the imaginary part of BCOMPLEX.
  virtual tree imag_part_expression (tree bcomplex, Location) = 0;

  // Return an expression for the complex number (BREAL, BIMAG).
  virtual tree complex_expression (tree breal, tree bimag, Location) = 0;

  // Return an expression that converts EXPR to TYPE.
  virtual tree convert_expression (tree type, tree expr, Location) = 0;

  // Create an expression for the address of a function.  This is used to
  // get the address of the code for a function.
  virtual tree function_code_expression (tree, Location) = 0;

  // Create an expression that takes the address of an expression.
  virtual tree address_expression (tree, Location) = 0;

  // Return an expression for the field at INDEX in BSTRUCT.
  virtual tree struct_field_expression (tree bstruct, size_t index, Location)
    = 0;

  // Create an expression that executes BSTAT before BEXPR.
  virtual tree compound_expression (tree bstat, tree bexpr, Location) = 0;

  // Return an expression that executes THEN_EXPR if CONDITION is true, or
  // ELSE_EXPR otherwise and returns the result as type BTYPE, within the
  // specified function FUNCTION.  ELSE_EXPR may be NULL.  BTYPE may be NULL.
  virtual tree conditional_expression (tree function, tree btype,
				       tree condition, tree then_expr,
				       tree else_expr, Location)
    = 0;

  // Return an expression for the negation operation OP EXPR.
  // Supported values of OP are enumerated in NegationOperator.
  virtual tree negation_expression (NegationOperator op, tree expr, Location)
    = 0;

  // Return an expression for the operation LEFT OP RIGHT.
  // Supported values of OP are enumerated in ArithmeticOrLogicalOperator.
  virtual tree arithmetic_or_logical_expression (ArithmeticOrLogicalOperator op,
						 tree left, tree right,
						 Location)
    = 0;

  // Return an expression for the operation LEFT OP RIGHT.
  // Supported values of OP are enumerated in ComparisonOperator.
  virtual tree comparison_expression (ComparisonOperator op, tree left,
				      tree right, Location)
    = 0;

  // Return an expression for the operation LEFT OP RIGHT.
  // Supported values of OP are enumerated in LazyBooleanOperator.
  virtual tree lazy_boolean_expression (LazyBooleanOperator op, tree left,
					tree right, Location)
    = 0;

  // Return an expression that constructs BTYPE with VALS.  BTYPE must be the
  // backend representation a of struct.  VALS must be in the same order as the
  // corresponding fields in BTYPE.
  virtual tree constructor_expression (tree btype, bool is_variant,
				       const std::vector<tree> &vals, int,
				       Location)
    = 0;

  // Return an expression that constructs an array of BTYPE with INDEXES and
  // VALS.  INDEXES and VALS must have the same amount of elements. Each index
  // in INDEXES must be in the same order as the corresponding value in VALS.
  virtual tree
  array_constructor_expression (tree btype,
				const std::vector<unsigned long> &indexes,
				const std::vector<tree> &vals, Location)
    = 0;

  // Return an expression for the address of BASE[INDEX].
  // BASE has a pointer type.  This is used for slice indexing.
  virtual tree pointer_offset_expression (tree base, tree index, Location) = 0;

  // Return an expression for ARRAY[INDEX] as an l-value.  ARRAY is a valid
  // fixed-length array, not a slice.
  virtual tree array_index_expression (tree array, tree index, Location) = 0;

  // Create an expression for a call to FN with ARGS, taking place within
  // caller CALLER.
  virtual tree call_expression (tree caller, tree fn,
				const std::vector<tree> &args,
				tree static_chain, Location)
    = 0;

  // Statements.

  // Create an error statement.  This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  virtual tree error_statement () = 0;

  // Create an expression statement within the specified function.
  virtual tree expression_statement (tree, tree) = 0;

  // Create a variable initialization statement in the specified
  // function.  This initializes a local variable at the point in the
  // program flow where it is declared.
  virtual tree init_statement (tree, Bvariable *var, tree init) = 0;

  // Create an assignment statement within the specified function.
  virtual tree assignment_statement (tree, tree lhs, tree rhs, Location) = 0;

  // Create a return statement, passing the representation of the
  // function and the list of values to return.
  virtual tree return_statement (tree, const std::vector<tree> &, Location) = 0;

  // Create an if statement within a function.  ELSE_BLOCK may be NULL.
  virtual tree if_statement (tree, tree condition, tree then_block,
			     tree else_block, Location)
    = 0;

  // infinite loop expressions
  virtual tree loop_expression (tree body, Location) = 0;

  // exit expressions
  virtual tree exit_expression (tree condition, Location) = 0;

  // Create a switch statement where the case values are constants.
  // CASES and STATEMENTS must have the same number of entries.  If
  // VALUE matches any of the list in CASES[i], which will all be
  // integers, then STATEMENTS[i] is executed.  STATEMENTS[i] will
  // either end with a goto statement or will fall through into
  // STATEMENTS[i + 1].  CASES[i] is empty for the default clause,
  // which need not be last.  FUNCTION is the current function.
  virtual tree switch_statement (tree function, tree value,
				 const std::vector<std::vector<tree> > &cases,
				 const std::vector<tree> &statements, Location)
    = 0;

  // Create a single statement from two statements.
  virtual tree compound_statement (tree, tree) = 0;

  // Create a single statement from a list of statements.
  virtual tree statement_list (const std::vector<tree> &) = 0;

  // Create a statement that attempts to execute BSTAT and calls EXCEPT_STMT if
  // an exception occurs. EXCEPT_STMT may be NULL.  FINALLY_STMT may be NULL and
  // if not NULL, it will always be executed.  This is used for handling defers
  // in Go functions.  In C++, the resulting code is of this form:
  //   try { BSTAT; } catch { EXCEPT_STMT; } finally { FINALLY_STMT; }
  virtual tree exception_handler_statement (tree bstat, tree except_stmt,
					    tree finally_stmt, Location)
    = 0;

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
  virtual tree block (tree function, tree enclosing,
		      const std::vector<Bvariable *> &vars,
		      Location start_location, Location end_location)
    = 0;

  // Add the statements to a block.  The block is created first.  Then
  // the statements are created.  Then the statements are added to the
  // block.  This will called exactly once per block.  The vector may
  // be empty if there are no statements.
  virtual void block_add_statements (tree, const std::vector<tree> &) = 0;

  // Return the block as a statement.  This is used to include a block
  // in a list of statements.
  virtual tree block_statement (tree) = 0;

  // Variables.

  // Create an error variable.  This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  virtual Bvariable *error_variable () = 0;

  // Create a global variable. NAME is the package-qualified name of
  // the variable.  ASM_NAME is the encoded identifier for the
  // variable, incorporating the package, and made safe for the
  // assembler.  BTYPE is the type of the variable.  IS_EXTERNAL is
  // true if the variable is defined in some other package.  IS_HIDDEN
  // is true if the variable is not exported (name begins with a lower
  // case letter).  IN_UNIQUE_SECTION is true if the variable should
  // be put into a unique section if possible; this is intended to
  // permit the linker to garbage collect the variable if it is not
  // referenced.  LOCATION is where the variable was defined.
  virtual Bvariable *global_variable (const std::string &name,
				      const std::string &asm_name, tree btype,
				      bool is_external, bool is_hidden,
				      bool in_unique_section, Location location)
    = 0;

  // A global variable will 1) be initialized to zero, or 2) be
  // initialized to a constant value, or 3) be initialized in the init
  // function.  In case 2, the frontend will call
  // global_variable_set_init to set the initial value.  If this is
  // not called, the backend should initialize a global variable to 0.
  // The init function may then assign a value to it.
  virtual void global_variable_set_init (Bvariable *, tree) = 0;

  // Create a local variable.  The frontend will create the local
  // variables first, and then create the block which contains them.
  // FUNCTION is the function in which the variable is defined.  NAME
  // is the name of the variable.  TYPE is the type.  DECL_VAR, if not
  // null, gives the location at which the value of this variable may
  // be found, typically used to create an inner-scope reference to an
  // outer-scope variable, to extend the lifetime of the variable beyond
  // the inner scope.  IS_ADDRESS_TAKEN is true if the address of this
  // variable is taken (this implies that the address does not escape
  // the function, as otherwise the variable would be on the heap).
  // LOCATION is where the variable is defined.  For each local variable
  // the frontend will call init_statement to set the initial value.
  virtual Bvariable *local_variable (tree function, const std::string &name,
				     tree type, Bvariable *decl_var,
				     bool is_address_taken, Location location)
    = 0;

  // Create a function parameter.  This is an incoming parameter, not
  // a result parameter (result parameters are treated as local
  // variables).  The arguments are as for local_variable.
  virtual Bvariable *parameter_variable (tree function, const std::string &name,
					 tree type, bool is_address_taken,
					 Location location)
    = 0;

  // Create a static chain parameter.  This is the closure parameter.
  virtual Bvariable *static_chain_variable (tree function,
					    const std::string &name, tree type,
					    Location location)
    = 0;

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
  virtual Bvariable *temporary_variable (tree, tree, tree, tree init,
					 bool address_is_taken,
					 Location location, tree *pstatement)
    = 0;

  // Create an implicit variable that is compiler-defined.  This is
  // used when generating GC data and roots, when storing the values
  // of a slice constructor, and for the zero value of types.  This returns a
  // Bvariable because it corresponds to an initialized variable in C.
  //
  // NAME is the name to use for the initialized variable this will create.
  //
  // ASM_NAME is encoded assembler-friendly version of the name, or the
  // empty string if no encoding is needed.
  //
  // TYPE is the type of the implicit variable.
  //
  // IS_HIDDEN will be true if the descriptor should only be visible
  // within the current object.
  //
  // IS_CONSTANT is true if the implicit variable should be treated like it is
  // immutable.  For slice initializers, if the values must be copied to the
  // heap, the variable IS_CONSTANT.
  //
  // IS_COMMON is true if the implicit variable should
  // be treated as a common variable (multiple definitions with
  // different sizes permitted in different object files, all merged
  // into the largest definition at link time); this will be true for
  // the zero value.  IS_HIDDEN and IS_COMMON will never both be true.
  //
  // If ALIGNMENT is not zero, it is the desired alignment of the variable.
  virtual Bvariable *implicit_variable (const std::string &name,
					const std::string &asm_name, tree type,
					bool is_hidden, bool is_constant,
					bool is_common, int64_t alignment)
    = 0;

  // Set the initial value of a variable created by implicit_variable.
  // This must be called even if there is no initializer, i.e., INIT is NULL.
  // The NAME, TYPE, IS_HIDDEN, IS_CONSTANT, and IS_COMMON parameters are
  // the same ones passed to implicit_variable.  INIT will be a composite
  // literal of type TYPE.  It will not contain any function calls or anything
  // else that can not be put into a read-only data section.
  // It may contain the address of variables created by implicit_variable.
  //
  // If IS_COMMON is true, INIT will be NULL, and the
  // variable should be initialized to all zeros.
  virtual void implicit_variable_set_init (Bvariable *, const std::string &name,
					   tree type, bool is_hidden,
					   bool is_constant, bool is_common,
					   tree init)
    = 0;

  // Create a reference to a named implicit variable defined in some
  // other package.  This will be a variable created by a call to
  // implicit_variable with the same NAME, ASM_NAME and TYPE and with
  // IS_COMMON passed as false.  This corresponds to an extern global
  // variable in C.
  virtual Bvariable *implicit_variable_reference (const std::string &name,
						  const std::string &asm_name,
						  tree type)
    = 0;

  // Create a named immutable initialized data structure.  This is
  // used for type descriptors, map descriptors, and function
  // descriptors.  This returns a Bvariable because it corresponds to
  // an initialized const variable in C.
  //
  // NAME is the name to use for the initialized global variable which
  // this call will create.
  //
  // ASM_NAME is the encoded, assembler-friendly version of NAME, or
  // the empty string if no encoding is needed.
  //
  // IS_HIDDEN will be true if the descriptor should only be visible
  // within the current object.
  //
  // IS_COMMON is true if NAME may be defined by several packages, and
  // the linker should merge all such definitions.  If IS_COMMON is
  // false, NAME should be defined in only one file.  In general
  // IS_COMMON will be true for the type descriptor of an unnamed type
  // or a builtin type.  IS_HIDDEN and IS_COMMON will never both be
  // true.
  //
  // TYPE will be a struct type; the type of the returned expression
  // must be a pointer to this struct type.
  //
  // We must create the named structure before we know its
  // initializer, because the initializer may refer to its own
  // address.  After calling this the frontend will call
  // immutable_struct_set_init.
  virtual Bvariable *
  immutable_struct (const std::string &name, const std::string &asm_name,
		    bool is_hidden, bool is_common, tree type, Location)
    = 0;

  // Set the initial value of a variable created by immutable_struct.
  // The NAME, IS_HIDDEN, IS_COMMON, TYPE, and location parameters are
  // the same ones passed to immutable_struct.  INITIALIZER will be a
  // composite literal of type TYPE.  It will not contain any function
  // calls or anything else that can not be put into a read-only data
  // section.  It may contain the address of variables created by
  // immutable_struct.
  virtual void immutable_struct_set_init (Bvariable *, const std::string &name,
					  bool is_hidden, bool is_common,
					  tree type, Location, tree initializer)
    = 0;

  // Create a reference to a named immutable initialized data
  // structure defined in some other package.  This will be a
  // structure created by a call to immutable_struct with the same
  // NAME, ASM_NAME and TYPE and with IS_COMMON passed as false.  This
  // corresponds to an extern const global variable in C.
  virtual Bvariable *immutable_struct_reference (const std::string &name,
						 const std::string &asm_name,
						 tree type, Location)
    = 0;

  // Labels.

  // Create a new label.  NAME will be empty if this is a label
  // created by the frontend for a loop construct.  The location is
  // where the label is defined.
  virtual tree label (tree, const std::string &name, Location) = 0;

  // Create a statement which defines a label.  This statement will be
  // put into the codestream at the point where the label should be
  // defined.
  virtual tree label_definition_statement (tree) = 0;

  // Create a goto statement to a label.
  virtual tree goto_statement (tree, Location) = 0;

  // Create an expression for the address of a label.  This is used to
  // get the return address of a deferred function which may call
  // recover.
  virtual tree label_address (tree, Location) = 0;

  // Functions.

  // Create an error function.  This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  virtual tree error_function () = 0;

  // Bit flags to pass to the function method.

  // Set if the function should be visible outside of the current
  // compilation unit.
  static const unsigned int function_is_visible = 1 << 0;

  // Set if this is a function declaration rather than a definition;
  // the definition will be in another compilation unit.
  static const unsigned int function_is_declaration = 1 << 1;

  // Set if the function should never be inlined because they call
  // recover and must be visible for correct panic recovery.
  static const unsigned int function_is_uninlinable = 1 << 2;

  // Set if the function does not return.  This is set for the
  // implementation of panic.
  static const unsigned int function_does_not_return = 1 << 3;

  // Set if the function should be put in a unique section if
  // possible.  This is used for field tracking.
  static const unsigned int function_in_unique_section = 1 << 4;

  // Set if the function should be available for inlining in the
  // backend, but should not be emitted as a standalone function.  Any
  // call to the function that is not inlined should be treated as a
  // call to a function defined in a different compilation unit.  This
  // is like a C99 function marked inline but not extern.
  static const unsigned int function_only_inline = 1 << 5;

  // const function
  static const unsigned int function_read_only = 1 << 6;

  // Declare or define a function of FNTYPE.
  // NAME is the Go name of the function.  ASM_NAME, if not the empty
  // string, is the name that should be used in the symbol table; this
  // will be non-empty if a magic extern comment is used.  FLAGS is
  // bit flags described above.
  virtual tree function (tree fntype, const std::string &name,
			 const std::string &asm_name, unsigned int flags,
			 Location)
    = 0;

  virtual tree specify_abi_attribute (tree type, Rust::ABI abi) = 0;

  // Create a statement that runs all deferred calls for FUNCTION.  This should
  // be a statement that looks like this in C++:
  //   finish:
  //     try { DEFER_RETURN; } catch { CHECK_DEFER; goto finish; }
  virtual tree function_defer_statement (tree function, tree undefer,
					 tree check_defer, Location)
    = 0;

  // Record PARAM_VARS as the variables to use for the parameters of FUNCTION.
  // This will only be called for a function definition.  Returns true on
  // success, false on failure.
  virtual bool
  function_set_parameters (tree function,
			   const std::vector<Bvariable *> &param_vars)
    = 0;

  // Set the function body for FUNCTION using the code in CODE_STMT.  Returns
  // true on success, false on failure.
  virtual bool function_set_body (tree function, tree code_stmt) = 0;

  // Look up a named built-in function in the current backend implementation.
  // Returns NULL if no built-in function by that name exists.
  virtual tree lookup_gcc_builtin (const std::string &) = 0;

  virtual tree lookup_builtin_by_rust_name (const std::string &) = 0;

  // Utility.

  // Write the definitions for all TYPE_DECLS, CONSTANT_DECLS,
  // FUNCTION_DECLS, and VARIABLE_DECLS declared globally.
  virtual void
  write_global_definitions (const std::vector<tree> &type_decls,
			    const std::vector<tree> &constant_decls,
			    const std::vector<tree> &function_decls,
			    const std::vector<Bvariable *> &variable_decls)
    = 0;

  // Write SIZE bytes of export data from BYTES to the proper
  // section in the output object file.
  virtual void write_export_data (const char *bytes, unsigned int size) = 0;
};

#endif // RUST_BACKEND_H
