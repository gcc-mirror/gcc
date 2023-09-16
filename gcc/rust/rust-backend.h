// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "util/rust-operators.h"
#include "tree.h"
#include "rust-gcc.h"

// Pointers to these types are created by the backend, passed to the
// frontend, and passed back to the backend.  The types must be
// defined by the backend using these names.

// The backend representation of a variable.
class Bvariable;

// The backend interface.  This is a pure abstract class that a
// specific backend will implement.

namespace Backend {

void
init ();

// Name/type/location.  Used for function parameters, struct fields,
// interface methods.
struct typed_identifier
{
  std::string name;
  tree type;
  location_t location;

  typed_identifier () : name (), type (NULL_TREE), location (UNKNOWN_LOCATION)
  {}

  typed_identifier (const std::string &a_name, tree a_type,
		    location_t a_location)
    : name (a_name), type (a_type), location (a_location)
  {}
};

// debug
void debug (tree);
void
debug (Bvariable *);

tree
get_identifier_node (const std::string &str);

// Types.

// Get the wchar type
tree
wchar_type ();

// Get the Host pointer size in bits
int
get_pointer_size ();

// Get the raw str type const char*
tree
raw_str_type ();

// Get an unnamed integer type with the given signedness and number
// of bits.
tree
integer_type (bool is_unsigned, int bits);

// Get an unnamed floating point type with the given number of bits
// (32 or 64).
tree
float_type (int bits);

// Get a pointer type.
tree
pointer_type (tree to_type);

// Get a reference type.
tree
reference_type (tree to_type);

// make type immutable
tree
immutable_type (tree base);

// Get a function type.  The receiver, parameter, and results are
// generated from the types in the Function_type.  The Function_type
// is provided so that the names are available.  This should return
// not the type of a Go function (which is a pointer to a struct)
// but the type of a C function pointer (which will be used as the
// type of the first field of the struct).  If there is more than
// one result, RESULT_STRUCT is a struct type to hold the results,
// and RESULTS may be ignored; if there are zero or one results,
// RESULT_STRUCT is NULL.
tree
function_type (const typed_identifier &receiver,
	       const std::vector<typed_identifier> &parameters,
	       const std::vector<typed_identifier> &results, tree result_struct,
	       location_t location);

tree
function_type_variadic (const typed_identifier &receiver,
			const std::vector<typed_identifier> &parameters,
			const std::vector<typed_identifier> &results,
			tree result_struct, location_t location);

tree
function_ptr_type (tree result, const std::vector<tree> &praameters,
		   location_t location);

// Get a struct type.
tree
struct_type (const std::vector<typed_identifier> &fields);

// Get a union type.
tree
union_type (const std::vector<typed_identifier> &fields);

// Get an array type.
tree
array_type (tree element_type, tree length);

// Return a named version of a type.  The location is the location
// of the type definition.  This will not be called for a type
// created via placeholder_pointer_type, placeholder_struct_type, or
// placeholder_array_type..  (It may be called for a pointer,
// struct, or array type in a case like "type P *byte; type Q P".)
tree
named_type (const std::string &name, tree, location_t);

// Return the size of a type.
int64_t type_size (tree);

// Return the alignment of a type.
int64_t type_alignment (tree);

// Return the alignment of a struct field of this type.  This is
// normally the same as type_alignment, but not always.
int64_t type_field_alignment (tree);

// Return the offset of field INDEX in a struct type.  INDEX is the
// entry in the FIELDS std::vector parameter of struct_type or
// set_placeholder_struct_type.
int64_t
type_field_offset (tree, size_t index);

// Expressions.

// Return an expression for a zero value of the given type.  This is
// used for cases such as local variable initialization and
// converting nil to other types.
tree zero_expression (tree);

// Create a reference to a variable.
tree
var_expression (Bvariable *var, location_t);

// Return an expression for the floating point value VAL in BTYPE.
tree
float_constant_expression (tree btype, mpfr_t val);

// Return an expression for the string value VAL.
tree
string_constant_expression (const std::string &val);

// Get a char literal
tree
char_constant_expression (char c);

// Get a char literal
tree
wchar_constant_expression (wchar_t c);

// Return an expression for the boolean value VAL.
tree
boolean_constant_expression (bool val);

// Return an expression that converts EXPR to TYPE.
tree
convert_expression (tree type, tree expr, location_t);

// Return an expression for the field at INDEX in BSTRUCT.
tree
struct_field_expression (tree bstruct, size_t index, location_t);

// Create an expression that executes BSTAT before BEXPR.
tree
compound_expression (tree bstat, tree bexpr, location_t);

// Return an expression that executes THEN_EXPR if CONDITION is true, or
// ELSE_EXPR otherwise and returns the result as type BTYPE, within the
// specified function FUNCTION.  ELSE_EXPR may be NULL.  BTYPE may be NULL.
tree
conditional_expression (tree function, tree btype, tree condition,
			tree then_expr, tree else_expr, location_t);

// Return an expression for the negation operation OP EXPR.
// Supported values of OP are enumerated in NegationOperator.
tree
negation_expression (NegationOperator op, tree expr, location_t);

// Return an expression for the operation LEFT OP RIGHT.
// Supported values of OP are enumerated in ArithmeticOrLogicalOperator.
tree
arithmetic_or_logical_expression (ArithmeticOrLogicalOperator op, tree left,
				  tree right, location_t loc);

// Return an expression for the operation LEFT OP RIGHT.
// Supported values of OP are enumerated in ArithmeticOrLogicalOperator.
// This function adds overflow checking and returns a list of statements to
// add to the current function context. The `receiver` variable refers to the
// variable which will contain the result of that operation.
tree
arithmetic_or_logical_expression_checked (ArithmeticOrLogicalOperator op,
					  tree left, tree right, location_t loc,
					  Bvariable *receiver);

// Return an expression for the operation LEFT OP RIGHT.
// Supported values of OP are enumerated in ComparisonOperator.
tree
comparison_expression (ComparisonOperator op, tree left, tree right,
		       location_t loc);

// Return an expression for the operation LEFT OP RIGHT.
// Supported values of OP are enumerated in LazyBooleanOperator.
tree
lazy_boolean_expression (LazyBooleanOperator op, tree left, tree right,
			 location_t);

// Return an expression that constructs BTYPE with VALS.  BTYPE must be the
// backend representation a of struct.  VALS must be in the same order as the
// corresponding fields in BTYPE.
tree
constructor_expression (tree btype, bool is_variant,
			const std::vector<tree> &vals, int, location_t);

// Return an expression that constructs an array of BTYPE with INDEXES and
// VALS.  INDEXES and VALS must have the same amount of elements. Each index
// in INDEXES must be in the same order as the corresponding value in VALS.
tree
array_constructor_expression (tree btype,
			      const std::vector<unsigned long> &indexes,
			      const std::vector<tree> &vals, location_t);

tree
array_initializer (tree, tree, tree, tree, tree, tree *, location_t);

// Return an expression for ARRAY[INDEX] as an l-value.  ARRAY is a valid
// fixed-length array, not a slice.
tree
array_index_expression (tree array, tree index, location_t);

// Create an expression for a call to FN with ARGS, taking place within
// caller CALLER.
tree
call_expression (tree fn, const std::vector<tree> &args, tree static_chain,
		 location_t);

// Statements.

// Create a variable initialization statement in the specified
// function.  This initializes a local variable at the point in the
// program flow where it is declared.
tree
init_statement (tree, Bvariable *var, tree init);

// Create an assignment statement within the specified function.
tree
assignment_statement (tree lhs, tree rhs, location_t);

// Create return statement for an decl for a value (can be NULL_TREE) at a
// location
tree
return_statement (tree fndecl, tree val, location_t);

// Create an if statement within a function.  ELSE_BLOCK may be NULL.
tree
if_statement (tree, tree condition, tree then_block, tree else_block,
	      location_t);

// infinite loop expressions
tree
loop_expression (tree body, location_t);

// exit expressions
tree
exit_expression (tree condition, location_t);

// Create a single statement from two statements.
tree compound_statement (tree, tree);

// Create a single statement from a list of statements.
tree
statement_list (const std::vector<tree> &);

// Create a statement that attempts to execute BSTAT and calls EXCEPT_STMT if
// an exception occurs. EXCEPT_STMT may be NULL.  FINALLY_STMT may be NULL and
// if not NULL, it will always be executed.  This is used for handling defers
// in Go functions.  In C++, the resulting code is of this form:
//   try { BSTAT; } catch { EXCEPT_STMT; } finally { FINALLY_STMT; }
tree
exception_handler_statement (tree bstat, tree except_stmt, tree finally_stmt,
			     location_t);

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
tree
block (tree function, tree enclosing, const std::vector<Bvariable *> &vars,
       location_t start_location, location_t end_location);

// Add the statements to a block.  The block is created first.  Then
// the statements are created.  Then the statements are added to the
// block.  This will called exactly once per block.  The vector may
// be empty if there are no statements.
void
block_add_statements (tree, const std::vector<tree> &);

// Variables.

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
Bvariable *
global_variable (const std::string &name, const std::string &asm_name,
		 tree btype, bool is_external, bool is_hidden,
		 bool in_unique_section, location_t location);

// A global variable will 1) be initialized to zero, or 2) be
// initialized to a constant value, or 3) be initialized in the init
// function.  In case 2, the frontend will call
// global_variable_set_init to set the initial value.  If this is
// not called, the backend should initialize a global variable to 0.
// The init function may then assign a value to it.
void
global_variable_set_init (Bvariable *, tree);

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
Bvariable *
local_variable (tree function, const std::string &name, tree type,
		Bvariable *decl_var, location_t location);

// Create a function parameter.  This is an incoming parameter, not
// a result parameter (result parameters are treated as local
// variables).  The arguments are as for local_variable.
Bvariable *
parameter_variable (tree function, const std::string &name, tree type,
		    location_t location);

// Create a static chain parameter.  This is the closure parameter.
Bvariable *
static_chain_variable (tree function, const std::string &name, tree type,
		       location_t location);

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
Bvariable *
temporary_variable (tree fndecl, tree bind_tree, tree type, tree init,
		    bool address_is_taken, location_t location,
		    tree *pstatement);

// Labels.

// Create a new label.  NAME will be empty if this is a label
// created by the frontend for a loop construct.  The location is
// where the label is defined.
tree
label (tree, const std::string &name, location_t);

// Create a statement which defines a label.  This statement will be
// put into the codestream at the point where the label should be
// defined.
tree label_definition_statement (tree);

// Create a goto statement to a label.
tree goto_statement (tree, location_t);

// Create an expression for the address of a label.  This is used to
// get the return address of a deferred function which may call
// recover.
tree label_address (tree, location_t);

// Functions.

// Bit flags to pass to the function method.

// Set if this is a function declaration rather than a definition;
// the definition will be in another compilation unit.
static const unsigned int function_is_declaration = 1 << 0;

// Set if the function should never be inlined because they call
// recover and must be visible for correct panic recovery.
static const unsigned int function_is_uninlinable = 1 << 1;

// Set if the function does not return.  This is set for the
// implementation of panic.
static const unsigned int function_does_not_return = 1 << 2;

// Set if the function should be put in a unique section if
// possible.  This is used for field tracking.
static const unsigned int function_in_unique_section = 1 << 3;

// Declare or define a function of FNTYPE.
// NAME is the Go name of the function.  ASM_NAME, if not the empty
// string, is the name that should be used in the symbol table; this
// will be non-empty if a magic extern comment is used.  FLAGS is
// bit flags described above.
tree
function (tree fntype, const std::string &name, const std::string &asm_name,
	  unsigned int flags, location_t);

// Create a statement that runs all deferred calls for FUNCTION.  This should
// be a statement that looks like this in C++:
//   finish:
//     try { DEFER_RETURN; } catch { CHECK_DEFER; goto finish; }
tree
function_defer_statement (tree function, tree undefer, tree check_defer,
			  location_t);

// Record PARAM_VARS as the variables to use for the parameters of FUNCTION.
// This will only be called for a function definition.  Returns true on
// success, false on failure.
bool
function_set_parameters (tree function,
			 const std::vector<Bvariable *> &param_vars);

// Utility.

// Write the definitions for all TYPE_DECLS, CONSTANT_DECLS,
// FUNCTION_DECLS, and VARIABLE_DECLS declared globally.
void
write_global_definitions (const std::vector<tree> &type_decls,
			  const std::vector<tree> &constant_decls,
			  const std::vector<tree> &function_decls,
			  const std::vector<Bvariable *> &variable_decls);

// TODO: make static

tree
fill_in_fields (tree, const std::vector<typed_identifier> &);

tree fill_in_array (tree, tree, tree);

tree non_zero_size_type (tree);

tree convert_tree (tree, tree, location_t);

} // namespace Backend

#endif // RUST_BACKEND_H
