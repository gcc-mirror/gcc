// gogo.h -- Go frontend parsed representation.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_GOGO_H
#define GO_GOGO_H

#include "go-linemap.h"

class Traverse;
class Statement_inserter;
class Type;
class Type_hash_identical;
class Type_equal;
class Type_identical;
class Typed_identifier;
class Typed_identifier_list;
class Function_type;
class Expression;
class Statement;
class Temporary_statement;
class Block;
class Function;
class Bindings;
class Bindings_snapshot;
class Package;
class Variable;
class Pointer_type;
class Struct_type;
class Struct_field;
class Struct_field_list;
class Array_type;
class Map_type;
class Channel_type;
class Interface_type;
class Named_type;
class Forward_declaration_type;
class Named_object;
class Label;
class Translate_context;
class Backend;
class Export;
class Import;
class Bexpression;
class Btype;
class Bstatement;
class Bblock;
class Bvariable;
class Blabel;
class Bfunction;
class Escape_context;
class Node;

// This file declares the basic classes used to hold the internal
// representation of Go which is built by the parser.

// An initialization function for an imported package.  This is a
// magic function which initializes variables and runs the "init"
// function.

class Import_init
{
 public:
  Import_init(const std::string& package_name, const std::string& init_name,
	      int priority)
    : package_name_(package_name), init_name_(init_name), priority_(priority)
  { }

  // The name of the package being imported.
  const std::string&
  package_name() const
  { return this->package_name_; }

  // The name of the package's init function.
  const std::string&
  init_name() const
  { return this->init_name_; }

  // The priority of the initialization function.  Functions with a
  // lower priority number must be run first.
  int
  priority() const
  { return this->priority_; }

 private:
  // The name of the package being imported.
  std::string package_name_;
  // The name of the package's init function.
  std::string init_name_;
  // The priority.
  int priority_;
};

// For sorting purposes.

inline bool
operator<(const Import_init& i1, const Import_init& i2)
{
  if (i1.priority() < i2.priority())
    return true;
  if (i1.priority() > i2.priority())
    return false;
  if (i1.package_name() != i2.package_name())
    return i1.package_name() < i2.package_name();
  return i1.init_name() < i2.init_name();
}

// The holder for the internal representation of the entire
// compilation unit.

class Gogo
{
 public:
  // Create the IR, passing in the sizes of the types "int" and
  // "uintptr" in bits.
  Gogo(Backend* backend, Linemap *linemap, int int_type_size, int pointer_size);

  // Get the backend generator.
  Backend*
  backend()
  { return this->backend_; }

  // Get the Location generator.
  Linemap*
  linemap()
  { return this->linemap_; }

  // Get the package name.
  const std::string&
  package_name() const;

  // Set the package name.
  void
  set_package_name(const std::string&, Location);

  // Return whether this is the "main" package.
  bool
  is_main_package() const;

  // If necessary, adjust the name to use for a hidden symbol.  We add
  // the package name, so that hidden symbols in different packages do
  // not collide.
  std::string
  pack_hidden_name(const std::string& name, bool is_exported) const
  {
    return (is_exported
	    ? name
	    : '.' + this->pkgpath() + '.' + name);
  }

  // Unpack a name which may have been hidden.  Returns the
  // user-visible name of the object.
  static std::string
  unpack_hidden_name(const std::string& name)
  { return name[0] != '.' ? name : name.substr(name.rfind('.') + 1); }

  // Return whether a possibly packed name is hidden.
  static bool
  is_hidden_name(const std::string& name)
  { return name[0] == '.'; }

  // Return the package path of a hidden name.
  static std::string
  hidden_name_pkgpath(const std::string& name)
  {
    go_assert(Gogo::is_hidden_name(name));
    return name.substr(1, name.rfind('.') - 1);
  }

  // Given a name which may or may not have been hidden, return the
  // name to use in an error message.
  static std::string
  message_name(const std::string& name);

  // Return whether a name is the blank identifier _.
  static bool
  is_sink_name(const std::string& name)
  {
    return (name[0] == '.'
	    && name[name.length() - 1] == '_'
	    && name[name.length() - 2] == '.');
  }

  // Convert a pkgpath into a string suitable for a symbol
  static std::string
  pkgpath_for_symbol(const std::string& pkgpath);

  // Return the package path to use for reflect.Type.PkgPath.
  const std::string&
  pkgpath() const;

  // Return the package path to use for a symbol name.
  const std::string&
  pkgpath_symbol() const;

  // Set the package path from a command line option.
  void
  set_pkgpath(const std::string&);

  // Set the prefix from a command line option.
  void
  set_prefix(const std::string&);

  // Return whether pkgpath was set from a command line option.
  bool
  pkgpath_from_option() const
  { return this->pkgpath_from_option_; }

  // Return the relative import path as set from the command line.
  // Returns an empty string if it was not set.
  const std::string&
  relative_import_path() const
  { return this->relative_import_path_; }

  // Set the relative import path from a command line option.
  void
  set_relative_import_path(const std::string& s)
  { this->relative_import_path_ = s; }

  // Return whether to check for division by zero in binary operations.
  bool
  check_divide_by_zero() const
  { return this->check_divide_by_zero_; }

  // Set the option to check division by zero from a command line option.
  void
  set_check_divide_by_zero(bool b)
  { this->check_divide_by_zero_ = b; }

  // Return whether to check for division overflow in binary operations.
  bool
  check_divide_overflow() const
  { return this->check_divide_overflow_; }

  // Set the option to check division overflow from a command line option.
  void
  set_check_divide_overflow(bool b)
  { this->check_divide_overflow_ = b; }

  // Return the level of escape analysis debug information to emit.
  int
  debug_escape_level() const
  { return this->debug_escape_level_; }

  // Set the level of escape analysis debugging from a command line option.
  void
  set_debug_escape_level(int level)
  { this->debug_escape_level_ = level; }

  // Return the priority to use for the package we are compiling.
  // This is two more than the largest priority of any package we
  // import.
  int
  package_priority() const;

  // Import a package.  FILENAME is the file name argument, LOCAL_NAME
  // is the local name to give to the package.  If LOCAL_NAME is empty
  // the declarations are added to the global scope.
  void
  import_package(const std::string& filename, const std::string& local_name,
		 bool is_local_name_exported, Location);

  // Whether we are the global binding level.
  bool
  in_global_scope() const;

  // Look up a name in the current binding contours.
  Named_object*
  lookup(const std::string&, Named_object** pfunction) const;

  // Look up a name in the current block.
  Named_object*
  lookup_in_block(const std::string&) const;

  // Look up a name in the global namespace--the universal scope.
  Named_object*
  lookup_global(const char*) const;

  // Add a new imported package.  REAL_NAME is the real name of the
  // package.  ALIAS is the alias of the package; this may be the same
  // as REAL_NAME.  This sets *PADD_TO_GLOBALS if symbols added to
  // this package should be added to the global namespace; this is
  // true if the alias is ".".  LOCATION is the location of the import
  // statement.  This returns the new package, or NULL on error.
  Package*
  add_imported_package(const std::string& real_name, const std::string& alias,
		       bool is_alias_exported,
		       const std::string& pkgpath,
		       const std::string& pkgpath_symbol,
		       Location location,
		       bool* padd_to_globals);

  // Register a package.  This package may or may not be imported.
  // This returns the Package structure for the package, creating if
  // it necessary.
  Package*
  register_package(const std::string& pkgpath,
		   const std::string& pkgpath_symbol, Location);

  // Start compiling a function.  ADD_METHOD_TO_TYPE is true if a
  // method function should be added to the type of its receiver.
  Named_object*
  start_function(const std::string& name, Function_type* type,
		 bool add_method_to_type, Location);

  // Finish compiling a function.
  void
  finish_function(Location);

  // Return the current function.
  Named_object*
  current_function() const;

  // Return the current block.
  Block*
  current_block();

  // Start a new block.  This is not initially associated with a
  // function.
  void
  start_block(Location);

  // Finish the current block and return it.
  Block*
  finish_block(Location);

  // Declare an erroneous name.  This is used to avoid knock-on errors
  // after a parsing error.
  Named_object*
  add_erroneous_name(const std::string& name);

  // Declare an unknown name.  This is used while parsing.  The name
  // must be resolved by the end of the parse.  Unknown names are
  // always added at the package level.
  Named_object*
  add_unknown_name(const std::string& name, Location);

  // Declare a function.
  Named_object*
  declare_function(const std::string&, Function_type*, Location);

  // Declare a function at the package level.  This is used for
  // functions generated for a type.
  Named_object*
  declare_package_function(const std::string&, Function_type*, Location);

  // Add a label.
  Label*
  add_label_definition(const std::string&, Location);

  // Add a label reference.  ISSUE_GOTO_ERRORS is true if we should
  // report errors for a goto from the current location to the label
  // location.
  Label*
  add_label_reference(const std::string&, Location,
		      bool issue_goto_errors);

  // An analysis set is a list of functions paired with a boolean that indicates
  // whether the list of functions are recursive.
  typedef std::pair<std::vector<Named_object*>, bool> Analysis_set;

  // Add a GROUP of possibly RECURSIVE functions to the Analysis_set for this
  // package.
  void
  add_analysis_set(const std::vector<Named_object*>& group, bool recursive)
  { this->analysis_sets_.push_back(std::make_pair(group, recursive)); }

  // Return a snapshot of the current binding state.
  Bindings_snapshot*
  bindings_snapshot(Location);

  // Add a statement to the current block.
  void
  add_statement(Statement*);

  // Add a block to the current block.
  void
  add_block(Block*, Location);

  // Add a constant.
  Named_object*
  add_constant(const Typed_identifier&, Expression*, int iota_value);

  // Add a type.
  void
  add_type(const std::string&, Type*, Location);

  // Add a named type.  This is used for builtin types, and to add an
  // imported type to the global scope.
  void
  add_named_type(Named_type*);

  // Declare a type.
  Named_object*
  declare_type(const std::string&, Location);

  // Declare a type at the package level.  This is used when the
  // parser sees an unknown name where a type name is required.
  Named_object*
  declare_package_type(const std::string&, Location);

  // Define a type which was already declared.
  void
  define_type(Named_object*, Named_type*);

  // Add a variable.
  Named_object*
  add_variable(const std::string&, Variable*);

  // Add a sink--a reference to the blank identifier _.
  Named_object*
  add_sink();

  // Add a type which needs to be verified.  This is used for sink
  // types, just to give appropriate error messages.
  void
  add_type_to_verify(Type* type);

  // Add a named object to the current namespace.  This is used for
  // import . "package".
  void
  add_dot_import_object(Named_object*);

  // Add an identifier to the list of names seen in the file block.
  void
  add_file_block_name(const std::string& name, Location location)
  { this->file_block_names_[name] = location; }

  // Add a linkname, from the go:linkname compiler directive.  This
  // changes the externally visible name of go_name to be ext_name.
  void
  add_linkname(const std::string& go_name, bool is_exported,
	       const std::string& ext_name, Location location);

  // Mark all local variables in current bindings as used.  This is
  // used when there is a parse error to avoid useless errors.
  void
  mark_locals_used();

  // Return a name to use for an error case.  This should only be used
  // after reporting an error, and is used to avoid useless knockon
  // errors.
  static std::string
  erroneous_name();

  // Return whether the name indicates an error.
  static bool
  is_erroneous_name(const std::string&);

  // Return a name to use for a thunk function.  A thunk function is
  // one we create during the compilation, for a go statement or a
  // defer statement or a method expression.
  static std::string
  thunk_name();

  // Return whether an object is a thunk.
  static bool
  is_thunk(const Named_object*);

  // Note that we've seen an interface type.  This is used to build
  // all required interface method tables.
  void
  record_interface_type(Interface_type*);

  // Note that we need an initialization function.
  void
  set_need_init_fn()
  { this->need_init_fn_ = true; }

  // Return whether the current file imported the unsafe package.
  bool
  current_file_imported_unsafe() const
  { return this->current_file_imported_unsafe_; }

  // Clear out all names in file scope.  This is called when we start
  // parsing a new file.
  void
  clear_file_scope();

  // Record that VAR1 must be initialized after VAR2.  This is used
  // when VAR2 does not appear in VAR1's INIT or PREINIT.
  void
  record_var_depends_on(Variable* var1, Named_object* var2)
  {
    go_assert(this->var_deps_.find(var1) == this->var_deps_.end());
    this->var_deps_[var1] = var2;
  }

  // Return the variable that VAR depends on, or NULL if none.
  Named_object*
  var_depends_on(Variable* var) const
  {
    Var_deps::const_iterator p = this->var_deps_.find(var);
    return p != this->var_deps_.end() ? p->second : NULL;
  }

  // Queue up a type-specific function to be written out.  This is
  // used when a type-specific function is needed when not at the top
  // level.
  void
  queue_specific_type_function(Type* type, Named_type* name,
			       const std::string& hash_name,
			       Function_type* hash_fntype,
			       const std::string& equal_name,
			       Function_type* equal_fntype);

  // Write out queued specific type functions.
  void
  write_specific_type_functions();

  // Whether we are done writing out specific type functions.
  bool
  specific_type_functions_are_written() const
  { return this->specific_type_functions_are_written_; }

  // Traverse the tree.  See the Traverse class.
  void
  traverse(Traverse*);

  // Define the predeclared global names.
  void
  define_global_names();

  // Verify and complete all types.
  void
  verify_types();

  // Lower the parse tree.
  void
  lower_parse_tree();

  // Lower all the statements in a block.
  void
  lower_block(Named_object* function, Block*);

  // Lower an expression.
  void
  lower_expression(Named_object* function, Statement_inserter*, Expression**);

  // Lower a constant.
  void
  lower_constant(Named_object*);

  // Flatten all the statements in a block.
  void
  flatten_block(Named_object* function, Block*);

  // Flatten an expression.
  void
  flatten_expression(Named_object* function, Statement_inserter*, Expression**);

  // Create all necessary function descriptors.
  void
  create_function_descriptors();

  // Finalize the method lists and build stub methods for named types.
  void
  finalize_methods();

  // Work out the types to use for unspecified variables and
  // constants.
  void
  determine_types();

  // Type check the program.
  void
  check_types();

  // Check the types in a single block.  This is used for complicated
  // go statements.
  void
  check_types_in_block(Block*);

  // Check for return statements.
  void
  check_return_statements();

  // Analyze the program flow for escape information.
  void
  analyze_escape();

  // Discover the groups of possibly recursive functions in this package.
  void
  discover_analysis_sets();

  // Build a connectivity graph between the objects in each analyzed function.
  void
  assign_connectivity(Escape_context*, Named_object*);

  // Traverse the objects in the connecitivty graph from the sink, adjusting the
  // escape levels of each object.
  void
  propagate_escape(Escape_context*, Node*);

  // Add notes about the escape level of a function's input and output
  // parameters for exporting and importing top level functions. 
  void
  tag_function(Escape_context*, Named_object*);

  // Do all exports.
  void
  do_exports();

  // Add an import control function for an imported package to the
  // list.
  void
  add_import_init_fn(const std::string& package_name,
		     const std::string& init_name, int prio);

  // Turn short-cut operators (&&, ||) into explicit if statements.
  void
  remove_shortcuts();

  // Use temporary variables to force order of evaluation.
  void
  order_evaluations();

  // Flatten parse tree.
  void
  flatten();

  // Build thunks for functions which call recover.
  void
  build_recover_thunks();

  // Simplify statements which might use thunks: go and defer
  // statements.
  void
  simplify_thunk_statements();

  // Dump AST if -fgo-dump-ast is set 
  void
  dump_ast(const char* basename);

  // Dump Call Graph if -fgo-dump-calls is set.
  void
  dump_call_graph(const char* basename);

  // Dump Connection Graphs if -fgo-dump-connections is set.
  void
  dump_connection_graphs(const char* basename);

  // Convert named types to the backend representation.
  void
  convert_named_types();

  // Convert named types in a list of bindings.
  void
  convert_named_types_in_bindings(Bindings*);

  // True if named types have been converted to the backend
  // representation.
  bool
  named_types_are_converted() const
  { return this->named_types_are_converted_; }

  // Write out the global values.
  void
  write_globals();

  // Build a call to the runtime error function.
  Expression*
  runtime_error(int code, Location);

  // Build required interface method tables.
  void
  build_interface_method_tables();

  // Return an expression which allocates memory to hold values of type TYPE.
  Expression*
  allocate_memory(Type *type, Location);

  // Get the name of the magic initialization function.
  const std::string&
  get_init_fn_name();

 private:
  // During parsing, we keep a stack of functions.  Each function on
  // the stack is one that we are currently parsing.  For each
  // function, we keep track of the current stack of blocks.
  struct Open_function
  {
    // The function.
    Named_object* function;
    // The stack of active blocks in the function.
    std::vector<Block*> blocks;
  };

  // The stack of functions.
  typedef std::vector<Open_function> Open_functions;

  // Set up the built-in unsafe package.
  void
  import_unsafe(const std::string&, bool is_exported, Location);

  // Return the current binding contour.
  Bindings*
  current_bindings();

  const Bindings*
  current_bindings() const;

  // Get the decl for the magic initialization function.
  Named_object*
  initialization_function_decl();

  // Create the magic initialization function.
  Named_object*
  create_initialization_function(Named_object* fndecl, Bstatement* code_stmt);

  // Initialize imported packages.
  void
  init_imports(std::vector<Bstatement*>&);

  // Register variables with the garbage collector.
  void
  register_gc_vars(const std::vector<Named_object*>&,
                   std::vector<Bstatement*>&);

  // Type used to map import names to packages.
  typedef std::map<std::string, Package*> Imports;

  // Type used to map package names to packages.
  typedef std::map<std::string, Package*> Packages;

  // Type used to map variables to the function calls that set them.
  // This is used for initialization dependency analysis.
  typedef std::map<Variable*, Named_object*> Var_deps;

  // Type used to map identifiers in the file block to the location
  // where they were defined.
  typedef Unordered_map(std::string, Location) File_block_names;

  // Type used to queue writing a type specific function.
  struct Specific_type_function
  {
    Type* type;
    Named_type* name;
    std::string hash_name;
    Function_type* hash_fntype;
    std::string equal_name;
    Function_type* equal_fntype;

    Specific_type_function(Type* atype, Named_type* aname,
			   const std::string& ahash_name,
			   Function_type* ahash_fntype,
			   const std::string& aequal_name,
			   Function_type* aequal_fntype)
      : type(atype), name(aname), hash_name(ahash_name),
	hash_fntype(ahash_fntype), equal_name(aequal_name),
	equal_fntype(aequal_fntype)
    { }
  };

  // The backend generator.
  Backend* backend_;
  // The object used to keep track of file names and line numbers.
  Linemap* linemap_;
  // The package we are compiling.
  Package* package_;
  // The list of currently open functions during parsing.
  Open_functions functions_;
  // The global binding contour.  This includes the builtin functions
  // and the package we are compiling.
  Bindings* globals_;
  // The list of names we have seen in the file block.
  File_block_names file_block_names_;
  // Mapping from import file names to packages.
  Imports imports_;
  // Whether the magic unsafe package was imported.
  bool imported_unsafe_;
  // Whether the magic unsafe package was imported by the current file.
  bool current_file_imported_unsafe_;
  // Mapping from package names we have seen to packages.  This does
  // not include the package we are compiling.
  Packages packages_;
  // The functions named "init", if there are any.
  std::vector<Named_object*> init_functions_;
  // A mapping from variables to the function calls that initialize
  // them, if it is not stored in the variable's init or preinit.
  // This is used for dependency analysis.
  Var_deps var_deps_;
  // Whether we need a magic initialization function.
  bool need_init_fn_;
  // The name of the magic initialization function.
  std::string init_fn_name_;
  // A list of import control variables for packages that we import.
  std::set<Import_init> imported_init_fns_;
  // The package path used for reflection data.
  std::string pkgpath_;
  // The package path to use for a symbol name.
  std::string pkgpath_symbol_;
  // The prefix to use for symbols, from the -fgo-prefix option.
  std::string prefix_;
  // Whether pkgpath_ has been set.
  bool pkgpath_set_;
  // Whether an explicit package path was set by -fgo-pkgpath.
  bool pkgpath_from_option_;
  // Whether an explicit prefix was set by -fgo-prefix.
  bool prefix_from_option_;
  // The relative import path, from the -fgo-relative-import-path
  // option.
  std::string relative_import_path_;
  // Whether or not to check for division by zero, from the
  // -fgo-check-divide-zero option.
  bool check_divide_by_zero_;
  // Whether or not to check for division overflow, from the
  // -fgo-check-divide-overflow option.
  bool check_divide_overflow_;
  // The level of escape analysis debug information to emit, from the
  // -fgo-debug-escape option.
  int debug_escape_level_;
  // A list of types to verify.
  std::vector<Type*> verify_types_;
  // A list of interface types defined while parsing.
  std::vector<Interface_type*> interface_types_;
  // Type specific functions to write out.
  std::vector<Specific_type_function*> specific_type_functions_;
  // Whether we are done writing out specific type functions.
  bool specific_type_functions_are_written_;
  // Whether named types have been converted.
  bool named_types_are_converted_;
  // A list containing groups of possibly mutually recursive functions to be
  // considered during escape analysis.
  std::vector<Analysis_set> analysis_sets_;
};

// A block of statements.

class Block
{
 public:
  Block(Block* enclosing, Location);

  // Return the enclosing block.
  const Block*
  enclosing() const
  { return this->enclosing_; }

  // Return the bindings of the block.
  Bindings*
  bindings()
  { return this->bindings_; }

  const Bindings*
  bindings() const
  { return this->bindings_; }

  // Look at the block's statements.
  const std::vector<Statement*>*
  statements() const
  { return &this->statements_; }

  // Return the start location.  This is normally the location of the
  // left curly brace which starts the block.
  Location
  start_location() const
  { return this->start_location_; }

  // Return the end location.  This is normally the location of the
  // right curly brace which ends the block.
  Location
  end_location() const
  { return this->end_location_; }

  // Add a statement to the block.
  void
  add_statement(Statement*);

  // Add a statement to the front of the block.
  void
  add_statement_at_front(Statement*);

  // Replace a statement in a block.
  void
  replace_statement(size_t index, Statement*);

  // Add a Statement before statement number INDEX.
  void
  insert_statement_before(size_t index, Statement*);

  // Add a Statement after statement number INDEX.
  void
  insert_statement_after(size_t index, Statement*);

  // Set the end location of the block.
  void
  set_end_location(Location location)
  { this->end_location_ = location; }

  // Traverse the tree.
  int
  traverse(Traverse*);

  // Set final types for unspecified variables and constants.
  void
  determine_types();

  // Return true if execution of this block may fall through to the
  // next block.
  bool
  may_fall_through() const;

  // Convert the block to the backend representation.
  Bblock*
  get_backend(Translate_context*);

  // Iterate over statements.

  typedef std::vector<Statement*>::iterator iterator;

  iterator
  begin()
  { return this->statements_.begin(); }

  iterator
  end()
  { return this->statements_.end(); }

 private:
  // Enclosing block.
  Block* enclosing_;
  // Statements in the block.
  std::vector<Statement*> statements_;
  // Binding contour.
  Bindings* bindings_;
  // Location of start of block.
  Location start_location_;
  // Location of end of block.
  Location end_location_;
};

// A function.

class Function
{
 public:
  Function(Function_type* type, Named_object*, Block*, Location);

  // Return the function's type.
  Function_type*
  type() const
  { return this->type_; }

  // Return the enclosing function if there is one.
  Named_object*
  enclosing() const
  { return this->enclosing_; }

  // Set the enclosing function.  This is used when building thunks
  // for functions which call recover.
  void
  set_enclosing(Named_object* enclosing)
  {
    go_assert(this->enclosing_ == NULL);
    this->enclosing_ = enclosing;
  }

  // The result variables.
  typedef std::vector<Named_object*> Results;

  // Create the result variables in the outer block.
  void
  create_result_variables(Gogo*);

  // Update the named result variables when cloning a function which
  // calls recover.
  void
  update_result_variables();

  // Return the result variables.
  Results*
  result_variables()
  { return this->results_; }

  bool
  is_sink() const
  { return this->is_sink_; }

  void
  set_is_sink()
  { this->is_sink_ = true; }

  // Whether the result variables have names.
  bool
  results_are_named() const
  { return this->results_are_named_; }

  // Set the assembler name.
  void
  set_asm_name(const std::string& asm_name)
  { this->asm_name_ = asm_name; }

  // Set the pragmas for this function.
  void
  set_pragmas(unsigned int pragmas)
  {
    this->pragmas_ = pragmas;
  }

  // Whether this method should not be included in the type
  // descriptor.
  bool
  nointerface() const;

  // Record that this method should not be included in the type
  // descriptor.
  void
  set_nointerface();

  // Record that this function is a stub method created for an unnamed
  // type.
  void
  set_is_unnamed_type_stub_method()
  {
    go_assert(this->is_method());
    this->is_unnamed_type_stub_method_ = true;
  }

  // Return the amount of enclosed variables in this closure.
  size_t
  closure_field_count() const
  { return this->closure_fields_.size(); }

  // Add a new field to the closure variable.
  void
  add_closure_field(Named_object* var, Location loc)
  { this->closure_fields_.push_back(std::make_pair(var, loc)); }

  // Whether this function needs a closure.
  bool
  needs_closure() const
  { return !this->closure_fields_.empty(); }

  // Return the closure variable, creating it if necessary.  This is
  // passed to the function as a static chain parameter.
  Named_object*
  closure_var();

  // Set the closure variable.  This is used when building thunks for
  // functions which call recover.
  void
  set_closure_var(Named_object* v)
  {
    go_assert(this->closure_var_ == NULL);
    this->closure_var_ = v;
  }

  // Return the variable for a reference to field INDEX in the closure
  // variable.
  Named_object*
  enclosing_var(unsigned int index)
  {
    go_assert(index < this->closure_fields_.size());
    return closure_fields_[index].first;
  }

  // Set the type of the closure variable if there is one.
  void
  set_closure_type();

  // Get the block of statements associated with the function.
  Block*
  block() const
  { return this->block_; }

  // Get the location of the start of the function.
  Location
  location() const
  { return this->location_; }

  // Return whether this function is actually a method.
  bool
  is_method() const;

  // Add a label definition to the function.
  Label*
  add_label_definition(Gogo*, const std::string& label_name, Location);

  // Add a label reference to a function.  ISSUE_GOTO_ERRORS is true
  // if we should report errors for a goto from the current location
  // to the label location.
  Label*
  add_label_reference(Gogo*, const std::string& label_name,
		      Location, bool issue_goto_errors);

  // Warn about labels that are defined but not used.
  void
  check_labels() const;

  // Note that a new local type has been added.  Return its index.
  unsigned int
  new_local_type_index()
  { return this->local_type_count_++; }

  // Whether this function calls the predeclared recover function.
  bool
  calls_recover() const
  { return this->calls_recover_; }

  // Record that this function calls the predeclared recover function.
  // This is set during the lowering pass.
  void
  set_calls_recover()
  { this->calls_recover_ = true; }

  // Whether this is a recover thunk function.
  bool
  is_recover_thunk() const
  { return this->is_recover_thunk_; }

  // Record that this is a thunk built for a function which calls
  // recover.
  void
  set_is_recover_thunk()
  { this->is_recover_thunk_ = true; }

  // Whether this function already has a recover thunk.
  bool
  has_recover_thunk() const
  { return this->has_recover_thunk_; }

  // Record that this function already has a recover thunk.
  void
  set_has_recover_thunk()
  { this->has_recover_thunk_ = true; }

  // Record that this function is a thunk created for a defer
  // statement that calls the __go_set_defer_retaddr runtime function.
  void
  set_calls_defer_retaddr()
  { this->calls_defer_retaddr_ = true; }

  // Whether this is a type hash or equality function created by the
  // compiler.
  bool
  is_type_specific_function()
  { return this->is_type_specific_function_; }

  // Record that this function is a type hash or equality function
  // created by the compiler.
  void
  set_is_type_specific_function()
  { this->is_type_specific_function_ = true; }

  // Mark the function as going into a unique section.
  void
  set_in_unique_section()
  { this->in_unique_section_ = true; }

  // Swap with another function.  Used only for the thunk which calls
  // recover.
  void
  swap_for_recover(Function *);

  // Traverse the tree.
  int
  traverse(Traverse*);

  // Determine types in the function.
  void
  determine_types();

  // Return an expression for the function descriptor, given the named
  // object for this function.  This may only be called for functions
  // without a closure.  This will be an immutable struct with one
  // field that points to the function's code.
  Expression*
  descriptor(Gogo*, Named_object*);

  // Set the descriptor for this function.  This is used when a
  // function declaration is followed by a function definition.
  void
  set_descriptor(Expression* descriptor)
  {
    go_assert(this->descriptor_ == NULL);
    this->descriptor_ = descriptor;
  }

  // Return the backend representation.
  Bfunction*
  get_or_make_decl(Gogo*, Named_object*);

  // Return the function's decl after it has been built.
  Bfunction*
  get_decl() const;

  // Set the function decl to hold a backend representation of the function
  // code.
  void
  build(Gogo*, Named_object*);

  // Get the statement that assigns values to this function's result struct.
  Bstatement*
  return_value(Gogo*, Named_object*, Location) const;

  // Get an expression for the variable holding the defer stack.
  Expression*
  defer_stack(Location);

  // Export the function.
  void
  export_func(Export*, const std::string& name) const;

  // Export a function with a type.
  static void
  export_func_with_type(Export*, const std::string& name,
			const Function_type*);

  // Import a function.
  static void
  import_func(Import*, std::string* pname, Typed_identifier** receiver,
	      Typed_identifier_list** pparameters,
	      Typed_identifier_list** presults, bool* is_varargs);

 private:
  // Type for mapping from label names to Label objects.
  typedef Unordered_map(std::string, Label*) Labels;

  void
  build_defer_wrapper(Gogo*, Named_object*, Bstatement**, Bstatement**);

  typedef std::vector<std::pair<Named_object*,
				Location> > Closure_fields;

  // The function's type.
  Function_type* type_;
  // The enclosing function.  This is NULL when there isn't one, which
  // is the normal case.
  Named_object* enclosing_;
  // The result variables, if any.
  Results* results_;
  // If there is a closure, this is the list of variables which appear
  // in the closure.  This is created by the parser, and then resolved
  // to a real type when we lower parse trees.
  Closure_fields closure_fields_;
  // The closure variable, passed as a parameter using the static
  // chain parameter.  Normally NULL.
  Named_object* closure_var_;
  // The outer block of statements in the function.
  Block* block_;
  // The source location of the start of the function.
  Location location_;
  // Labels defined or referenced in the function.
  Labels labels_;
  // The number of local types defined in this function.
  unsigned int local_type_count_;
  // The assembler name: this is the name that will be put in the object file.
  // Set by the go:linkname compiler directive.  This is normally empty.
  std::string asm_name_;
  // The function descriptor, if any.
  Expression* descriptor_;
  // The function decl.
  Bfunction* fndecl_;
  // The defer stack variable.  A pointer to this variable is used to
  // distinguish the defer stack for one function from another.  This
  // is NULL unless we actually need a defer stack.
  Temporary_statement* defer_stack_;
  // Pragmas for this function.  This is a set of GOPRAGMA bits.
  unsigned int pragmas_;
  // True if this function is sink-named.  No code is generated.
  bool is_sink_ : 1;
  // True if the result variables are named.
  bool results_are_named_ : 1;
  // True if this function is a stub method created for an unnamed
  // type.
  bool is_unnamed_type_stub_method_ : 1;
  // True if this function calls the predeclared recover function.
  bool calls_recover_ : 1;
  // True if this a thunk built for a function which calls recover.
  bool is_recover_thunk_ : 1;
  // True if this function already has a recover thunk.
  bool has_recover_thunk_ : 1;
  // True if this is a thunk built for a defer statement that calls
  // the __go_set_defer_retaddr runtime function.
  bool calls_defer_retaddr_ : 1;
  // True if this is a function built by the compiler to as a hash or
  // equality function for some type.
  bool is_type_specific_function_ : 1;
  // True if this function should be put in a unique section.  This is
  // turned on for field tracking.
  bool in_unique_section_ : 1;
};

// A snapshot of the current binding state.

class Bindings_snapshot
{
 public:
  Bindings_snapshot(const Block*, Location);

  // Report any errors appropriate for a goto from the current binding
  // state of B to this one.
  void
  check_goto_from(const Block* b, Location);

  // Report any errors appropriate for a goto from this binding state
  // to the current state of B.
  void
  check_goto_to(const Block* b);

 private:
  bool
  check_goto_block(Location, const Block*, const Block*, size_t*);

  void
  check_goto_defs(Location, const Block*, size_t, size_t);

  // The current block.
  const Block* block_;
  // The number of names currently defined in each open block.
  // Element 0 is this->block_, element 1 is
  // this->block_->enclosing(), etc.
  std::vector<size_t> counts_;
  // The location where this snapshot was taken.
  Location location_;
};

// A function declaration.

class Function_declaration
{
 public:
  Function_declaration(Function_type* fntype, Location location)
    : fntype_(fntype), location_(location), asm_name_(), descriptor_(NULL),
      fndecl_(NULL), pragmas_(0)
  { }

  Function_type*
  type() const
  { return this->fntype_; }

  Location
  location() const
  { return this->location_; }

  const std::string&
  asm_name() const
  { return this->asm_name_; }

  // Set the assembler name.
  void
  set_asm_name(const std::string& asm_name)
  { this->asm_name_ = asm_name; }

  // Set the pragmas for this function.
  void
  set_pragmas(unsigned int pragmas)
  {
    this->pragmas_ = pragmas;
  }

  // Return an expression for the function descriptor, given the named
  // object for this function.  This may only be called for functions
  // without a closure.  This will be an immutable struct with one
  // field that points to the function's code.
  Expression*
  descriptor(Gogo*, Named_object*);

  // Return true if we have created a descriptor for this declaration.
  bool
  has_descriptor() const
  { return this->descriptor_ != NULL; }

  // Return a backend representation.
  Bfunction*
  get_or_make_decl(Gogo*, Named_object*);

  // If there is a descriptor, build it into the backend
  // representation.
  void
  build_backend_descriptor(Gogo*);

  // Export a function declaration.
  void
  export_func(Export* exp, const std::string& name) const
  { Function::export_func_with_type(exp, name, this->fntype_); }

  // Check that the types used in this declaration's signature are defined.
  void
  check_types() const;

 private:
  // The type of the function.
  Function_type* fntype_;
  // The location of the declaration.
  Location location_;
  // The assembler name: this is the name to use in references to the
  // function.  This is normally empty.
  std::string asm_name_;
  // The function descriptor, if any.
  Expression* descriptor_;
  // The function decl if needed.
  Bfunction* fndecl_;
  // Pragmas for this function.  This is a set of GOPRAGMA bits.
  unsigned int pragmas_;
};

// A variable.

class Variable
{
 public:
  Variable(Type*, Expression*, bool is_global, bool is_parameter,
	   bool is_receiver, Location);

  // Get the type of the variable.
  Type*
  type();

  Type*
  type() const;

  // Return whether the type is defined yet.
  bool
  has_type() const;

  // Get the initial value.
  Expression*
  init() const
  { return this->init_; }

  // Return whether there are any preinit statements.
  bool
  has_pre_init() const
  { return this->preinit_ != NULL; }

  // Return the preinit statements if any.
  Block*
  preinit() const
  { return this->preinit_; }

  // Return whether this is a global variable.
  bool
  is_global() const
  { return this->is_global_; }

  // Return whether this is a function parameter.
  bool
  is_parameter() const
  { return this->is_parameter_; }

  // Return whether this is a closure (static chain) parameter.
  bool
  is_closure() const
  { return this->is_closure_; }

  // Change this parameter to be a closure.
  void
  set_is_closure()
  {
    this->is_closure_ = true;
  }

  // Return whether this is the receiver parameter of a method.
  bool
  is_receiver() const
  { return this->is_receiver_; }

  // Change this parameter to be a receiver.  This is used when
  // creating the thunks created for functions which call recover.
  void
  set_is_receiver()
  {
    go_assert(this->is_parameter_);
    this->is_receiver_ = true;
  }

  // Change this parameter to not be a receiver.  This is used when
  // creating the thunks created for functions which call recover.
  void
  set_is_not_receiver()
  {
    go_assert(this->is_parameter_);
    this->is_receiver_ = false;
  }

  // Return whether this is the varargs parameter of a function.
  bool
  is_varargs_parameter() const
  { return this->is_varargs_parameter_; }

  // Whether this variable's address is taken.
  bool
  is_address_taken() const
  { return this->is_address_taken_; }

  // Whether this variable should live in the heap.
  bool
  is_in_heap() const
  {
    return this->is_address_taken_ 
      && this->escapes_
      && !this->is_global_;
  }

  // Note that something takes the address of this variable.
  void
  set_address_taken()
  { this->is_address_taken_ = true; }

  // Return whether the address is taken but does not escape.
  bool
  is_non_escaping_address_taken() const
  { return this->is_non_escaping_address_taken_; }

  // Note that something takes the address of this variable such that
  // the address does not escape the function.
  void
  set_non_escaping_address_taken()
  { this->is_non_escaping_address_taken_ = true; }

  // Return whether this variable escapes the function it is declared in.
  bool
  escapes()
  { return this->escapes_; }

  // Note that this variable does not escape the function it is declared in.
  void
  set_does_not_escape()
  { this->escapes_ = false; }

  // Get the source location of the variable's declaration.
  Location
  location() const
  { return this->location_; }

  // Record that this is the varargs parameter of a function.
  void
  set_is_varargs_parameter()
  {
    go_assert(this->is_parameter_);
    this->is_varargs_parameter_ = true;
  }

  // Return whether the variable has been used.
  bool
  is_used() const
  { return this->is_used_; }

  // Mark that the variable has been used.
  void
  set_is_used()
  { this->is_used_ = true; }

  // Clear the initial value; used for error handling.
  void
  clear_init()
  { this->init_ = NULL; }

  // Set the initial value; used for converting shortcuts.
  void
  set_init(Expression* init)
  { this->init_ = init; }

  // Get the preinit block, a block of statements to be run before the
  // initialization expression.
  Block*
  preinit_block(Gogo*);

  // Add a statement to be run before the initialization expression.
  // This is only used for global variables.
  void
  add_preinit_statement(Gogo*, Statement*);

  // Lower the initialization expression after parsing is complete.
  void
  lower_init_expression(Gogo*, Named_object*, Statement_inserter*);

  // Flatten the initialization expression after ordering evaluations.
  void
  flatten_init_expression(Gogo*, Named_object*, Statement_inserter*);

  // A special case: the init value is used only to determine the
  // type.  This is used if the variable is defined using := with the
  // comma-ok form of a map index or a receive expression.  The init
  // value is actually the map index expression or receive expression.
  // We use this because we may not know the right type at parse time.
  void
  set_type_from_init_tuple()
  { this->type_from_init_tuple_ = true; }

  // Another special case: the init value is used only to determine
  // the type.  This is used if the variable is defined using := with
  // a range clause.  The init value is the range expression.  The
  // type of the variable is the index type of the range expression
  // (i.e., the first value returned by a range).
  void
  set_type_from_range_index()
  { this->type_from_range_index_ = true; }

  // Another special case: like set_type_from_range_index, but the
  // type is the value type of the range expression (i.e., the second
  // value returned by a range).
  void
  set_type_from_range_value()
  { this->type_from_range_value_ = true; }

  // Another special case: the init value is used only to determine
  // the type.  This is used if the variable is defined using := with
  // a case in a select statement.  The init value is the channel.
  // The type of the variable is the channel's element type.
  void
  set_type_from_chan_element()
  { this->type_from_chan_element_ = true; }

  // After we lower the select statement, we once again set the type
  // from the initialization expression.
  void
  clear_type_from_chan_element()
  {
    go_assert(this->type_from_chan_element_);
    this->type_from_chan_element_ = false;
  }

  // TRUE if this variable was created for a type switch clause.
  bool
  is_type_switch_var() const
  { return this->is_type_switch_var_; }

  // Note that this variable was created for a type switch clause.
  void
  set_is_type_switch_var()
  { this->is_type_switch_var_ = true; }

  // Mark the variable as going into a unique section.
  void
  set_in_unique_section()
  {
    go_assert(this->is_global_);
    this->in_unique_section_ = true;
  }

  // Traverse the initializer expression.
  int
  traverse_expression(Traverse*, unsigned int traverse_mask);

  // Determine the type of the variable if necessary.
  void
  determine_type();

  // Get the backend representation of the variable.
  Bvariable*
  get_backend_variable(Gogo*, Named_object*, const Package*,
		       const std::string&);

  // Get the initial value of the variable.  This may only
  // be called if has_pre_init() returns false.
  Bexpression*
  get_init(Gogo*, Named_object* function);

  // Return a series of statements which sets the value of the
  // variable in DECL.  This should only be called is has_pre_init()
  // returns true.  DECL may be NULL for a sink variable.
  Bstatement*
  get_init_block(Gogo*, Named_object* function, Bvariable* decl);

  // Export the variable.
  void
  export_var(Export*, const std::string& name) const;

  // Import a variable.
  static void
  import_var(Import*, std::string* pname, Type** ptype);

 private:
  // The type of a tuple.
  Type*
  type_from_tuple(Expression*, bool) const;

  // The type of a range.
  Type*
  type_from_range(Expression*, bool, bool) const;

  // The element type of a channel.
  Type*
  type_from_chan_element(Expression*, bool) const;

  // The variable's type.  This may be NULL if the type is set from
  // the expression.
  Type* type_;
  // The initial value.  This may be NULL if the variable should be
  // initialized to the default value for the type.
  Expression* init_;
  // Statements to run before the init statement.
  Block* preinit_;
  // Location of variable definition.
  Location location_;
  // Backend representation.
  Bvariable* backend_;
  // Whether this is a global variable.
  bool is_global_ : 1;
  // Whether this is a function parameter.
  bool is_parameter_ : 1;
  // Whether this is a closure parameter.
  bool is_closure_ : 1;
  // Whether this is the receiver parameter of a method.
  bool is_receiver_ : 1;
  // Whether this is the varargs parameter of a function.
  bool is_varargs_parameter_ : 1;
  // Whether this variable is ever referenced.
  bool is_used_ : 1;
  // Whether something takes the address of this variable.  For a
  // local variable this implies that the variable has to be on the
  // heap if it escapes from its function.
  bool is_address_taken_ : 1;
  // Whether something takes the address of this variable such that
  // the address does not escape the function.
  bool is_non_escaping_address_taken_ : 1;
  // True if we have seen this variable in a traversal.
  bool seen_ : 1;
  // True if we have lowered the initialization expression.
  bool init_is_lowered_ : 1;
  // True if we have flattened the initialization expression.
  bool init_is_flattened_ : 1;
  // True if init is a tuple used to set the type.
  bool type_from_init_tuple_ : 1;
  // True if init is a range clause and the type is the index type.
  bool type_from_range_index_ : 1;
  // True if init is a range clause and the type is the value type.
  bool type_from_range_value_ : 1;
  // True if init is a channel and the type is the channel's element type.
  bool type_from_chan_element_ : 1;
  // True if this is a variable created for a type switch case.
  bool is_type_switch_var_ : 1;
  // True if we have determined types.
  bool determined_type_ : 1;
  // True if this variable should be put in a unique section.  This is
  // used for field tracking.
  bool in_unique_section_ : 1;
  // Whether this variable escapes the function it is created in.  This is
  // true until shown otherwise.
  bool escapes_ : 1;
};

// A variable which is really the name for a function return value, or
// part of one.

class Result_variable
{
 public:
  Result_variable(Type* type, Function* function, int index,
		  Location location)
    : type_(type), function_(function), index_(index), location_(location),
      backend_(NULL), is_address_taken_(false),
      is_non_escaping_address_taken_(false), escapes_(true)
  { }

  // Get the type of the result variable.
  Type*
  type() const
  { return this->type_; }

  // Get the function that this is associated with.
  Function*
  function() const
  { return this->function_; }

  // Index in the list of function results.
  int
  index() const
  { return this->index_; }

  // The location of the variable definition.
  Location
  location() const
  { return this->location_; }

  // Whether this variable's address is taken.
  bool
  is_address_taken() const
  { return this->is_address_taken_; }

  // Note that something takes the address of this variable.
  void
  set_address_taken()
  { this->is_address_taken_ = true; }

  // Return whether the address is taken but does not escape.
  bool
  is_non_escaping_address_taken() const
  { return this->is_non_escaping_address_taken_; }

  // Note that something takes the address of this variable such that
  // the address does not escape the function.
  void
  set_non_escaping_address_taken()
  { this->is_non_escaping_address_taken_ = true; }
  
  // Return whether this variable escapes the function it is declared in.
  bool
  escapes()
  { return this->escapes_; }

  // Note that this variable does not escape the function it is declared in.
  void
  set_does_not_escape()
  { this->escapes_ = false; }

  // Whether this variable should live in the heap.
  bool
  is_in_heap() const
  {
    return this->is_address_taken_
      && this->escapes_;
  }

  // Set the function.  This is used when cloning functions which call
  // recover.
  void
  set_function(Function* function)
  { this->function_ = function; }

  // Get the backend representation of the variable.
  Bvariable*
  get_backend_variable(Gogo*, Named_object*, const std::string&);

 private:
  // Type of result variable.
  Type* type_;
  // Function with which this is associated.
  Function* function_;
  // Index in list of results.
  int index_;
  // Where the result variable is defined.
  Location location_;
  // Backend representation.
  Bvariable* backend_;
  // Whether something takes the address of this variable.
  bool is_address_taken_;
  // Whether something takes the address of this variable such that
  // the address does not escape the function.
  bool is_non_escaping_address_taken_;
  // Whether this variable escapes the function it is created in.  This is
  // true until shown otherwise.
  bool escapes_;
};

// The value we keep for a named constant.  This lets us hold a type
// and an expression.

class Named_constant
{
 public:
  Named_constant(Type* type, Expression* expr, int iota_value,
		 Location location)
    : type_(type), expr_(expr), iota_value_(iota_value), location_(location),
      lowering_(false), is_sink_(false), bconst_(NULL)
  { }

  Type*
  type() const
  { return this->type_; }

  Expression*
  expr() const
  { return this->expr_; }

  int
  iota_value() const
  { return this->iota_value_; }

  Location
  location() const
  { return this->location_; }

  // Whether we are lowering.
  bool
  lowering() const
  { return this->lowering_; }

  // Set that we are lowering.
  void
  set_lowering()
  { this->lowering_ = true; }

  // We are no longer lowering.
  void
  clear_lowering()
  { this->lowering_ = false; }

  bool
  is_sink() const
  { return this->is_sink_; }

  void
  set_is_sink()
  { this->is_sink_ = true; }

  // Traverse the expression.
  int
  traverse_expression(Traverse*);

  // Determine the type of the constant if necessary.
  void
  determine_type();

  // Indicate that we found and reported an error for this constant.
  void
  set_error();

  // Export the constant.
  void
  export_const(Export*, const std::string& name) const;

  // Import a constant.
  static void
  import_const(Import*, std::string*, Type**, Expression**);

  // Get the backend representation of the constant value.
  Bexpression*
  get_backend(Gogo*, Named_object*);

 private:
  // The type of the constant.
  Type* type_;
  // The expression for the constant.
  Expression* expr_;
  // If the predeclared constant iota is used in EXPR_, this is the
  // value it will have.  We do this because at parse time we don't
  // know whether the name "iota" will refer to the predeclared
  // constant or to something else.  We put in the right value in when
  // we lower.
  int iota_value_;
  // The location of the definition.
  Location location_;
  // Whether we are currently lowering this constant.
  bool lowering_;
  // Whether this constant is blank named and needs only type checking.
  bool is_sink_;
  // The backend representation of the constant value.
  Bexpression* bconst_;
};

// A type declaration.

class Type_declaration
{
 public:
  Type_declaration(Location location)
    : location_(location), in_function_(NULL), in_function_index_(0),
      methods_(), issued_warning_(false)
  { }

  // Return the location.
  Location
  location() const
  { return this->location_; }

  // Return the function in which this type is declared.  This will
  // return NULL for a type declared in global scope.
  Named_object*
  in_function(unsigned int* pindex)
  {
    *pindex = this->in_function_index_;
    return this->in_function_;
  }

  // Set the function in which this type is declared.
  void
  set_in_function(Named_object* f, unsigned int index)
  {
    this->in_function_ = f;
    this->in_function_index_ = index;
  }

  // Add a method to this type.  This is used when methods are defined
  // before the type.
  Named_object*
  add_method(const std::string& name, Function* function);

  // Add a method declaration to this type.
  Named_object*
  add_method_declaration(const std::string& name, Package*,
			 Function_type* type, Location location);

  // Return whether any methods were defined.
  bool
  has_methods() const;

  // Return the methods.
  const std::vector<Named_object*>*
  methods() const
  { return &this->methods_; }

  // Define methods when the real type is known.
  void
  define_methods(Named_type*);

  // This is called if we are trying to use this type.  It returns
  // true if we should issue a warning.
  bool
  using_type();

 private:
  // The location of the type declaration.
  Location location_;
  // If this type is declared in a function, a pointer back to the
  // function in which it is defined.
  Named_object* in_function_;
  // The index of this type in IN_FUNCTION_.
  unsigned int in_function_index_;
  // Methods defined before the type is defined.
  std::vector<Named_object*> methods_;
  // True if we have issued a warning about a use of this type
  // declaration when it is undefined.
  bool issued_warning_;
};

// An unknown object.  These are created by the parser for forward
// references to names which have not been seen before.  In a correct
// program, these will always point to a real definition by the end of
// the parse.  Because they point to another Named_object, these may
// only be referenced by Unknown_expression objects.

class Unknown_name
{
 public:
  Unknown_name(Location location)
    : location_(location), real_named_object_(NULL)
  { }

  // Return the location where this name was first seen.
  Location
  location() const
  { return this->location_; }

  // Return the real named object that this points to, or NULL if it
  // was never resolved.
  Named_object*
  real_named_object() const
  { return this->real_named_object_; }

  // Set the real named object that this points to.
  void
  set_real_named_object(Named_object* no);

 private:
  // The location where this name was first seen.
  Location location_;
  // The real named object when it is known.
  Named_object*
  real_named_object_;
};

// A named object named.  This is the result of a declaration.  We
// don't use a superclass because they all have to be handled
// differently.

class Named_object
{
 public:
  enum Classification
  {
    // An uninitialized Named_object.  We should never see this.
    NAMED_OBJECT_UNINITIALIZED,
    // An erroneous name.  This indicates a parse error, to avoid
    // later errors about undefined references.
    NAMED_OBJECT_ERRONEOUS,
    // An unknown name.  This is used for forward references.  In a
    // correct program, these will all be resolved by the end of the
    // parse.
    NAMED_OBJECT_UNKNOWN,
    // A const.
    NAMED_OBJECT_CONST,
    // A type.
    NAMED_OBJECT_TYPE,
    // A forward type declaration.
    NAMED_OBJECT_TYPE_DECLARATION,
    // A var.
    NAMED_OBJECT_VAR,
    // A result variable in a function.
    NAMED_OBJECT_RESULT_VAR,
    // The blank identifier--the special variable named _.
    NAMED_OBJECT_SINK,
    // A func.
    NAMED_OBJECT_FUNC,
    // A forward func declaration.
    NAMED_OBJECT_FUNC_DECLARATION,
    // A package.
    NAMED_OBJECT_PACKAGE
  };

  // Return the classification.
  Classification
  classification() const
  { return this->classification_; }

  // Classifiers.

  bool
  is_erroneous() const
  { return this->classification_ == NAMED_OBJECT_ERRONEOUS; }

  bool
  is_unknown() const
  { return this->classification_ == NAMED_OBJECT_UNKNOWN; }

  bool
  is_const() const
  { return this->classification_ == NAMED_OBJECT_CONST; }

  bool
  is_type() const
  { return this->classification_ == NAMED_OBJECT_TYPE; }

  bool
  is_type_declaration() const
  { return this->classification_ == NAMED_OBJECT_TYPE_DECLARATION; }

  bool
  is_variable() const
  { return this->classification_ == NAMED_OBJECT_VAR; }

  bool
  is_result_variable() const
  { return this->classification_ == NAMED_OBJECT_RESULT_VAR; }

  bool
  is_sink() const
  { return this->classification_ == NAMED_OBJECT_SINK; }

  bool
  is_function() const
  { return this->classification_ == NAMED_OBJECT_FUNC; }

  bool
  is_function_declaration() const
  { return this->classification_ == NAMED_OBJECT_FUNC_DECLARATION; }

  bool
  is_package() const
  { return this->classification_ == NAMED_OBJECT_PACKAGE; }

  // Creators.

  static Named_object*
  make_erroneous_name(const std::string& name)
  { return new Named_object(name, NULL, NAMED_OBJECT_ERRONEOUS); }

  static Named_object*
  make_unknown_name(const std::string& name, Location);

  static Named_object*
  make_constant(const Typed_identifier&, const Package*, Expression*,
		int iota_value);

  static Named_object*
  make_type(const std::string&, const Package*, Type*, Location);

  static Named_object*
  make_type_declaration(const std::string&, const Package*, Location);

  static Named_object*
  make_variable(const std::string&, const Package*, Variable*);

  static Named_object*
  make_result_variable(const std::string&, Result_variable*);

  static Named_object*
  make_sink();

  static Named_object*
  make_function(const std::string&, const Package*, Function*);

  static Named_object*
  make_function_declaration(const std::string&, const Package*, Function_type*,
			    Location);

  static Named_object*
  make_package(const std::string& alias, Package* package);

  // Getters.

  Unknown_name*
  unknown_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_UNKNOWN);
    return this->u_.unknown_value;
  }

  const Unknown_name*
  unknown_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_UNKNOWN);
    return this->u_.unknown_value;
  }

  Named_constant*
  const_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_CONST);
    return this->u_.const_value;
  }

  const Named_constant*
  const_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_CONST);
    return this->u_.const_value;
  }

  Named_type*
  type_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_TYPE);
    return this->u_.type_value;
  }

  const Named_type*
  type_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_TYPE);
    return this->u_.type_value;
  }

  Type_declaration*
  type_declaration_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_TYPE_DECLARATION);
    return this->u_.type_declaration;
  }

  const Type_declaration*
  type_declaration_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_TYPE_DECLARATION);
    return this->u_.type_declaration;
  }

  Variable*
  var_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_VAR);
    return this->u_.var_value;
  }

  const Variable*
  var_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_VAR);
    return this->u_.var_value;
  }

  Result_variable*
  result_var_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_RESULT_VAR);
    return this->u_.result_var_value;
  }

  const Result_variable*
  result_var_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_RESULT_VAR);
    return this->u_.result_var_value;
  }

  Function*
  func_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_FUNC);
    return this->u_.func_value;
  }

  const Function*
  func_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_FUNC);
    return this->u_.func_value;
  }

  Function_declaration*
  func_declaration_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_FUNC_DECLARATION);
    return this->u_.func_declaration_value;
  }

  const Function_declaration*
  func_declaration_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_FUNC_DECLARATION);
    return this->u_.func_declaration_value;
  }

  Package*
  package_value()
  {
    go_assert(this->classification_ == NAMED_OBJECT_PACKAGE);
    return this->u_.package_value;
  }

  const Package*
  package_value() const
  {
    go_assert(this->classification_ == NAMED_OBJECT_PACKAGE);
    return this->u_.package_value;
  }

  const std::string&
  name() const
  { return this->name_; }

  // Return the name to use in an error message.  The difference is
  // that if this Named_object is defined in a different package, this
  // will return PACKAGE.NAME.
  std::string
  message_name() const;

  const Package*
  package() const
  { return this->package_; }

  // Resolve an unknown value if possible.  This returns the same
  // Named_object or a new one.
  Named_object*
  resolve()
  {
    Named_object* ret = this;
    if (this->is_unknown())
      {
	Named_object* r = this->unknown_value()->real_named_object();
	if (r != NULL)
	  ret = r;
      }
    return ret;
  }

  const Named_object*
  resolve() const
  {
    const Named_object* ret = this;
    if (this->is_unknown())
      {
	const Named_object* r = this->unknown_value()->real_named_object();
	if (r != NULL)
	  ret = r;
      }
    return ret;
  }

  // The location where this object was defined or referenced.
  Location
  location() const;

  // Convert a variable to the backend representation.
  Bvariable*
  get_backend_variable(Gogo*, Named_object* function);

  // Return the external identifier for this object.
  std::string
  get_id(Gogo*);

  // Get the backend representation of this object.
  void
  get_backend(Gogo*, std::vector<Bexpression*>&, std::vector<Btype*>&,
              std::vector<Bfunction*>&);

  // Define a type declaration.
  void
  set_type_value(Named_type*);

  // Define a function declaration.
  void
  set_function_value(Function*);

  // Declare an unknown name as a type declaration.
  void
  declare_as_type();

  // Export this object.
  void
  export_named_object(Export*) const;

  // Mark this named object as an invalid redefinition of another object.
  void
  set_is_redefinition()
  { this->is_redefinition_ = true; }

  // Return whether or not this object is a invalid redefinition of another
  // object.
  bool
  is_redefinition() const
  { return this->is_redefinition_; }

 private:
  Named_object(const std::string&, const Package*, Classification);

  // The name of the object.
  std::string name_;
  // The package that this object is in.  This is NULL if it is in the
  // file we are compiling.
  const Package* package_;
  // The type of object this is.
  Classification classification_;
  // The real data.
  union
  {
    Unknown_name* unknown_value;
    Named_constant* const_value;
    Named_type* type_value;
    Type_declaration* type_declaration;
    Variable* var_value;
    Result_variable* result_var_value;
    Function* func_value;
    Function_declaration* func_declaration_value;
    Package* package_value;
  } u_;
  // True if this object is an invalid redefinition of another object.
  bool is_redefinition_;
};

// A binding contour.  This binds names to objects.

class Bindings
{
 public:
  // Type for mapping from names to objects.
  typedef Unordered_map(std::string, Named_object*) Contour;

  Bindings(Bindings* enclosing);

  // Add an erroneous name.
  Named_object*
  add_erroneous_name(const std::string& name)
  { return this->add_named_object(Named_object::make_erroneous_name(name)); }

  // Add an unknown name.
  Named_object*
  add_unknown_name(const std::string& name, Location location)
  {
    return this->add_named_object(Named_object::make_unknown_name(name,
								  location));
  }

  // Add a constant.
  Named_object*
  add_constant(const Typed_identifier& tid, const Package* package,
	       Expression* expr, int iota_value)
  {
    return this->add_named_object(Named_object::make_constant(tid, package,
							      expr,
							      iota_value));
  }

  // Add a type.
  Named_object*
  add_type(const std::string& name, const Package* package, Type* type,
	   Location location)
  {
    return this->add_named_object(Named_object::make_type(name, package, type,
							  location));
  }

  // Add a named type.  This is used for builtin types, and to add an
  // imported type to the global scope.
  Named_object*
  add_named_type(Named_type* named_type);

  // Add a type declaration.
  Named_object*
  add_type_declaration(const std::string& name, const Package* package,
		       Location location)
  {
    Named_object* no = Named_object::make_type_declaration(name, package,
							   location);
    return this->add_named_object(no);
  }

  // Add a variable.
  Named_object*
  add_variable(const std::string& name, const Package* package,
	       Variable* variable)
  {
    return this->add_named_object(Named_object::make_variable(name, package,
							      variable));
  }

  // Add a result variable.
  Named_object*
  add_result_variable(const std::string& name, Result_variable* result)
  {
    return this->add_named_object(Named_object::make_result_variable(name,
								     result));
  }

  // Add a function.
  Named_object*
  add_function(const std::string& name, const Package*, Function* function);

  // Add a function declaration.
  Named_object*
  add_function_declaration(const std::string& name, const Package* package,
			   Function_type* type, Location location);

  // Add a package.  The location is the location of the import
  // statement.
  Named_object*
  add_package(const std::string& alias, Package* package)
  {
    Named_object* no = Named_object::make_package(alias, package);
    return this->add_named_object(no);
  }

  // Define a type which was already declared.
  void
  define_type(Named_object*, Named_type*);

  // Add a method to the list of objects.  This is not added to the
  // lookup table.
  void
  add_method(Named_object*);

  // Add a named object to this binding.
  Named_object*
  add_named_object(Named_object* no)
  { return this->add_named_object_to_contour(&this->bindings_, no); }

  // Clear all names in file scope from the bindings.
  void
  clear_file_scope(Gogo*);

  // Look up a name in this binding contour and in any enclosing
  // binding contours.  This returns NULL if the name is not found.
  Named_object*
  lookup(const std::string&) const;

  // Look up a name in this binding contour without looking in any
  // enclosing binding contours.  Returns NULL if the name is not found.
  Named_object*
  lookup_local(const std::string&) const;

  // Remove a name.
  void
  remove_binding(Named_object*);

  // Mark all variables as used.  This is used for some types of parse
  // error.
  void
  mark_locals_used();

  // Traverse the tree.  See the Traverse class.
  int
  traverse(Traverse*, bool is_global);

  // Iterate over definitions.  This does not include things which
  // were only declared.

  typedef std::vector<Named_object*>::const_iterator
    const_definitions_iterator;

  const_definitions_iterator
  begin_definitions() const
  { return this->named_objects_.begin(); }

  const_definitions_iterator
  end_definitions() const
  { return this->named_objects_.end(); }

  // Return the number of definitions.
  size_t
  size_definitions() const
  { return this->named_objects_.size(); }

  // Return whether there are no definitions.
  bool
  empty_definitions() const
  { return this->named_objects_.empty(); }

  // Iterate over declarations.  This is everything that has been
  // declared, which includes everything which has been defined.

  typedef Contour::const_iterator const_declarations_iterator;

  const_declarations_iterator
  begin_declarations() const
  { return this->bindings_.begin(); }

  const_declarations_iterator
  end_declarations() const
  { return this->bindings_.end(); }

  // Return the number of declarations.
  size_t
  size_declarations() const
  { return this->bindings_.size(); }

  // Return whether there are no declarations.
  bool
  empty_declarations() const
  { return this->bindings_.empty(); }

  // Return the first declaration.
  Named_object*
  first_declaration()
  { return this->bindings_.empty() ? NULL : this->bindings_.begin()->second; }

 private:
  Named_object*
  add_named_object_to_contour(Contour*, Named_object*);

  Named_object*
  new_definition(Named_object*, Named_object*);

  // Enclosing bindings.
  Bindings* enclosing_;
  // The list of objects.
  std::vector<Named_object*> named_objects_;
  // The mapping from names to objects.
  Contour bindings_;
};

// A label.

class Label
{
 public:
  Label(const std::string& name)
    : name_(name), location_(Linemap::unknown_location()), snapshot_(NULL),
      refs_(), is_used_(false), blabel_(NULL), depth_(DEPTH_UNKNOWN)
  { }

  // Return the label's name.
  const std::string&
  name() const
  { return this->name_; }

  // Return whether the label has been defined.
  bool
  is_defined() const
  { return !Linemap::is_unknown_location(this->location_); }

  // Return whether the label has been used.
  bool
  is_used() const
  { return this->is_used_; }

  // Record that the label is used.
  void
  set_is_used()
  { this->is_used_ = true; }

  // Return whether this label is looping.
  bool
  looping() const
  { return this->depth_ == DEPTH_LOOPING; }

  // Set this label as looping.
  void
  set_looping()
  { this->depth_ = DEPTH_LOOPING; }

  // Return whether this label is nonlooping.
  bool
  nonlooping() const
  { return this->depth_ == DEPTH_NONLOOPING; }

  // Set this label as nonlooping.
  void
  set_nonlooping()
  { this->depth_ = DEPTH_NONLOOPING; }

  // Return the location of the definition.
  Location
  location() const
  { return this->location_; }

  // Return the bindings snapshot.
  Bindings_snapshot*
  snapshot() const
  { return this->snapshot_; }

  // Add a snapshot of a goto which refers to this label.
  void
  add_snapshot_ref(Bindings_snapshot* snapshot)
  {
    go_assert(Linemap::is_unknown_location(this->location_));
    this->refs_.push_back(snapshot);
  }

  // Return the list of snapshots of goto statements which refer to
  // this label.
  const std::vector<Bindings_snapshot*>&
  refs() const
  { return this->refs_; }

  // Clear the references.
  void
  clear_refs();

  // Define the label at LOCATION with the given bindings snapshot.
  void
  define(Location location, Bindings_snapshot* snapshot)
  {
    if (this->is_dummy_label())
      return;
    go_assert(Linemap::is_unknown_location(this->location_)
              && this->snapshot_ == NULL);
    this->location_ = location;
    this->snapshot_ = snapshot;
  }

  // Return the backend representation for this label.
  Blabel*
  get_backend_label(Translate_context*);

  // Return an expression for the address of this label.  This is used
  // to get the return address of a deferred function to see whether
  // the function may call recover.
  Bexpression*
  get_addr(Translate_context*, Location location);

  // Return a dummy label, representing any instance of the blank label.
  static Label*
  create_dummy_label();

  // Return TRUE if this is a dummy label.
  bool
  is_dummy_label() const
  { return this->name_ == "_"; }

  // A classification of a label's looping depth.
  enum Loop_depth
  {
    DEPTH_UNKNOWN,
    // A label never jumped to.
    DEPTH_NONLOOPING,
    // A label jumped to.
    DEPTH_LOOPING
  };

 private:
  // The name of the label.
  std::string name_;
  // The location of the definition.  This is 0 if the label has not
  // yet been defined.
  Location location_;
  // A snapshot of the set of bindings defined at this label, used to
  // issue errors about invalid goto statements.
  Bindings_snapshot* snapshot_;
  // A list of snapshots of goto statements which refer to this label.
  std::vector<Bindings_snapshot*> refs_;
  // Whether the label has been used.
  bool is_used_;
  // The backend representation.
  Blabel* blabel_;
  // The looping depth of this label, for escape analysis.
  Loop_depth depth_;
};

// An unnamed label.  These are used when lowering loops.

class Unnamed_label
{
 public:
  Unnamed_label(Location location)
    : location_(location), derived_from_(NULL), blabel_(NULL)
  { }

  // Get the location where the label is defined.
  Location
  location() const
  { return this->location_; }

  // Set the location where the label is defined.
  void
  set_location(Location location)
  { this->location_ = location; }

  // Get the top level statement this unnamed label is derived from.
  Statement*
  derived_from() const
  { return this->derived_from_; }

  // Set the top level statement this unnamed label is derived from.
  void
  set_derived_from(Statement* s)
  { this->derived_from_ = s; }

  // Return a statement which defines this label.
  Bstatement*
  get_definition(Translate_context*);

  // Return a goto to this label from LOCATION.
  Bstatement*
  get_goto(Translate_context*, Location location);

 private:
  // Return the backend representation.
  Blabel*
  get_blabel(Translate_context*);

  // The location where the label is defined.
  Location location_;
  // The top-level statement this unnamed label was derived/lowered from.
  // This is NULL is this label is not the top-level of a lowered statement.
  Statement* derived_from_;
  // The backend representation of this label.
  Blabel* blabel_;
};

// An alias for an imported package.

class Package_alias
{
 public:
  Package_alias(Location location)
      : location_(location), used_(0)
  { }

  // The location of the import statement.
  Location
  location()
  { return this->location_; }

  // How many symbols from the package were used under this alias.
  size_t
  used() const
  { return this->used_; }

  // Note that some symbol was used under this alias.
  void
  note_usage()
  { this->used_++; }

 private:
  // The location of the import statement.
  Location location_;
  // The amount of times some name from this package was used under this alias.
  size_t used_;
};

// An imported package.

class Package
{
 public:
  Package(const std::string& pkgpath, const std::string& pkgpath_symbol,
	  Location location);

  // Get the package path used for all symbols exported from this
  // package.
  const std::string&
  pkgpath() const
  { return this->pkgpath_; }

  // Return the package path to use for a symbol name.
  std::string
  pkgpath_symbol() const;

  // Set the package path symbol.
  void
  set_pkgpath_symbol(const std::string&);

  // Return the location of the most recent import statement.
  Location
  location() const
  { return this->location_; }

  // Return whether we know the name of this package yet.
  bool
  has_package_name() const
  { return !this->package_name_.empty(); }

  // The name that this package uses in its package clause.  This may
  // be different from the name in the associated Named_object if the
  // import statement used an alias.
  const std::string&
  package_name() const
  {
    go_assert(!this->package_name_.empty());
    return this->package_name_;
  }

  // The priority of this package.  The init function of packages with
  // lower priority must be run before the init function of packages
  // with higher priority.
  int
  priority() const
  { return this->priority_; }

  // Set the priority.
  void
  set_priority(int priority);

  // Return the bindings.
  Bindings*
  bindings()
  { return this->bindings_; }

  // Type used to map import names to package aliases.
  typedef std::map<std::string, Package_alias*> Aliases;

  // Return the set of package aliases.
  const Aliases&
  aliases() const
  { return this->aliases_; }

  // Note that some symbol from this package was used and qualified by ALIAS.
  // For dot imports, the ALIAS should be ".PACKAGE_NAME".
  void
  note_usage(const std::string& alias) const;

  // Note that USAGE might be a fake usage of this package.
  void
  note_fake_usage(Expression* usage) const
  { this->fake_uses_.insert(usage); }

  // Forget a given USAGE of this package.
  void
  forget_usage(Expression* usage) const;

  // Clear the used field for the next file.
  void
  clear_used();

  // Look up a name in the package.  Returns NULL if the name is not
  // found.
  Named_object*
  lookup(const std::string& name) const
  { return this->bindings_->lookup(name); }

  // Set the name of the package.
  void
  set_package_name(const std::string& name, Location);

  // Set the location of the package.  This is used to record the most
  // recent import location.
  void
  set_location(Location location)
  { this->location_ = location; }

  // Add a package name as an ALIAS for this package.
  Package_alias*
  add_alias(const std::string& alias, Location);

  // Add a constant to the package.
  Named_object*
  add_constant(const Typed_identifier& tid, Expression* expr)
  { return this->bindings_->add_constant(tid, this, expr, 0); }

  // Add a type to the package.
  Named_object*
  add_type(const std::string& name, Type* type, Location location)
  { return this->bindings_->add_type(name, this, type, location); }

  // Add a type declaration to the package.
  Named_object*
  add_type_declaration(const std::string& name, Location location)
  { return this->bindings_->add_type_declaration(name, this, location); }

  // Add a variable to the package.
  Named_object*
  add_variable(const std::string& name, Variable* variable)
  { return this->bindings_->add_variable(name, this, variable); }

  // Add a function declaration to the package.
  Named_object*
  add_function_declaration(const std::string& name, Function_type* type,
			   Location loc)
  { return this->bindings_->add_function_declaration(name, this, type, loc); }

  // Determine types of constants.
  void
  determine_types();

 private:
  // The package path for type reflection data.
  std::string pkgpath_;
  // The package path for symbol names.
  std::string pkgpath_symbol_;
  // The name that this package uses in the package clause.  This may
  // be the empty string if it is not yet known.
  std::string package_name_;
  // The names in this package.
  Bindings* bindings_;
  // The priority of this package.  A package has a priority higher
  // than the priority of all of the packages that it imports.  This
  // is used to run init functions in the right order.
  int priority_;
  // The location of the most recent import statement.
  Location location_;
  // The set of aliases associated with this package.
  Aliases aliases_;
  // A set of possibly fake uses of this package. This is mutable because we
  // can track fake uses of a package even if we have a const pointer to it.
  mutable std::set<Expression*> fake_uses_;
};

// Return codes for the traversal functions.  This is not an enum
// because we want to be able to declare traversal functions in other
// header files without including this one.

// Continue traversal as usual.
const int TRAVERSE_CONTINUE = -1;

// Exit traversal.
const int TRAVERSE_EXIT = 0;

// Continue traversal, but skip components of the current object.
// E.g., if this is returned by Traverse::statement, we do not
// traverse the expressions in the statement even if
// traverse_expressions is set in the traverse_mask.
const int TRAVERSE_SKIP_COMPONENTS = 1;

// This class is used when traversing the parse tree.  The caller uses
// a subclass which overrides functions as desired.

class Traverse
{
 public:
  // These bitmasks say what to traverse.
  static const unsigned int traverse_variables =    0x1;
  static const unsigned int traverse_constants =    0x2;
  static const unsigned int traverse_functions =    0x4;
  static const unsigned int traverse_blocks =       0x8;
  static const unsigned int traverse_statements =  0x10;
  static const unsigned int traverse_expressions = 0x20;
  static const unsigned int traverse_types =       0x40;

  Traverse(unsigned int traverse_mask)
    : traverse_mask_(traverse_mask), types_seen_(NULL), expressions_seen_(NULL)
  { }

  virtual ~Traverse();

  // The bitmask of what to traverse.
  unsigned int
  traverse_mask() const
  { return this->traverse_mask_; }

  // Record that we are going to traverse a type.  This returns true
  // if the type has already been seen in this traversal.  This is
  // required because types, unlike expressions, can form a circular
  // graph.
  bool
  remember_type(const Type*);

  // Record that we are going to see an expression.  This returns true
  // if the expression has already been seen in this traversal.  This
  // is only needed for cases where multiple expressions can point to
  // a single one.
  bool
  remember_expression(const Expression*);

  // These functions return one of the TRAVERSE codes defined above.

  // If traverse_variables is set in the mask, this is called for
  // every variable in the tree.
  virtual int
  variable(Named_object*);

  // If traverse_constants is set in the mask, this is called for
  // every named constant in the tree.  The bool parameter is true for
  // a global constant.
  virtual int
  constant(Named_object*, bool);

  // If traverse_functions is set in the mask, this is called for
  // every function in the tree.
  virtual int
  function(Named_object*);

  // If traverse_blocks is set in the mask, this is called for every
  // block in the tree.
  virtual int
  block(Block*);

  // If traverse_statements is set in the mask, this is called for
  // every statement in the tree.
  virtual int
  statement(Block*, size_t* index, Statement*);

  // If traverse_expressions is set in the mask, this is called for
  // every expression in the tree.
  virtual int
  expression(Expression**);

  // If traverse_types is set in the mask, this is called for every
  // type in the tree.
  virtual int
  type(Type*);

 private:
  // A hash table for types we have seen during this traversal.  Note
  // that this uses the default hash functions for pointers rather
  // than Type_hash_identical and Type_identical.  This is because for
  // traversal we care about seeing a specific type structure.  If
  // there are two separate instances of identical types, we want to
  // traverse both.
  typedef Unordered_set(const Type*) Types_seen;

  typedef Unordered_set(const Expression*) Expressions_seen;

  // Bitmask of what sort of objects to traverse.
  unsigned int traverse_mask_;
  // Types which have been seen in this traversal.
  Types_seen* types_seen_;
  // Expressions which have been seen in this traversal.
  Expressions_seen* expressions_seen_;
};

// A class which makes it easier to insert new statements before the
// current statement during a traversal.

class Statement_inserter
{
 public:
  // Empty constructor.
  Statement_inserter()
    : block_(NULL), pindex_(NULL), gogo_(NULL), var_(NULL)
  { }

  // Constructor for a statement in a block.
  Statement_inserter(Block* block, size_t *pindex)
    : block_(block), pindex_(pindex), gogo_(NULL), var_(NULL)
  { }

  // Constructor for a global variable.
  Statement_inserter(Gogo* gogo, Variable* var)
    : block_(NULL), pindex_(NULL), gogo_(gogo), var_(var)
  { go_assert(var->is_global()); }

  // We use the default copy constructor and assignment operator.

  // Insert S before the statement we are traversing, or before the
  // initialization expression of a global variable.
  void
  insert(Statement* s);

 private:
  // The block that the statement is in.
  Block* block_;
  // The index of the statement that we are traversing.
  size_t* pindex_;
  // The IR, needed when looking at an initializer expression for a
  // global variable.
  Gogo* gogo_;
  // The global variable, when looking at an initializer expression.
  Variable* var_;
};

// When translating the gogo IR into the backend data structure, this
// is the context we pass down the blocks and statements.

class Translate_context
{
 public:
  Translate_context(Gogo* gogo, Named_object* function, Block* block,
		    Bblock* bblock)
    : gogo_(gogo), backend_(gogo->backend()), function_(function),
      block_(block), bblock_(bblock), is_const_(false)
  { }

  // Accessors.

  Gogo*
  gogo()
  { return this->gogo_; }

  Backend*
  backend()
  { return this->backend_; }

  Named_object*
  function()
  { return this->function_; }

  Block*
  block()
  { return this->block_; }

  Bblock*
  bblock()
  { return this->bblock_; }

  bool
  is_const()
  { return this->is_const_; }

  // Make a constant context.
  void
  set_is_const()
  { this->is_const_ = true; }

 private:
  // The IR for the entire compilation unit.
  Gogo* gogo_;
  // The generator for the backend data structures.
  Backend* backend_;
  // The function we are currently translating.  NULL if not in a
  // function, e.g., the initializer of a global variable.
  Named_object* function_;
  // The block we are currently translating.  NULL if not in a
  // function.
  Block *block_;
  // The backend representation of the current block.  NULL if block_
  // is NULL.
  Bblock* bblock_;
  // Whether this is being evaluated in a constant context.  This is
  // used for type descriptor initializers.
  bool is_const_;
};

// Runtime error codes.  These must match the values in
// libgo/runtime/go-runtime-error.c.

// Slice index out of bounds: negative or larger than the length of
// the slice.
static const int RUNTIME_ERROR_SLICE_INDEX_OUT_OF_BOUNDS = 0;

// Array index out of bounds.
static const int RUNTIME_ERROR_ARRAY_INDEX_OUT_OF_BOUNDS = 1;

// String index out of bounds.
static const int RUNTIME_ERROR_STRING_INDEX_OUT_OF_BOUNDS = 2;

// Slice slice out of bounds: negative or larger than the length of
// the slice or high bound less than low bound.
static const int RUNTIME_ERROR_SLICE_SLICE_OUT_OF_BOUNDS = 3;

// Array slice out of bounds.
static const int RUNTIME_ERROR_ARRAY_SLICE_OUT_OF_BOUNDS = 4;

// String slice out of bounds.
static const int RUNTIME_ERROR_STRING_SLICE_OUT_OF_BOUNDS = 5;

// Dereference of nil pointer.  This is used when there is a
// dereference of a pointer to a very large struct or array, to ensure
// that a gigantic array is not used a proxy to access random memory
// locations.
static const int RUNTIME_ERROR_NIL_DEREFERENCE = 6;

// Slice length or capacity out of bounds in make: negative or
// overflow or length greater than capacity.
static const int RUNTIME_ERROR_MAKE_SLICE_OUT_OF_BOUNDS = 7;

// Map capacity out of bounds in make: negative or overflow.
static const int RUNTIME_ERROR_MAKE_MAP_OUT_OF_BOUNDS = 8;

// Channel capacity out of bounds in make: negative or overflow.
static const int RUNTIME_ERROR_MAKE_CHAN_OUT_OF_BOUNDS = 9;

// Division by zero.
static const int RUNTIME_ERROR_DIVISION_BY_ZERO = 10;

// This is used by some of the langhooks.
extern Gogo* go_get_gogo();

// Whether we have seen any errors.  FIXME: Replace with a backend
// interface.
extern bool saw_errors();

#endif // !defined(GO_GOGO_H)
