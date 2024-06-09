// statements.h -- Go frontend statements.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_STATEMENTS_H
#define GO_STATEMENTS_H

#include "operator.h"

class Gogo;
class Traverse;
class Statement_inserter;
class Block;
class Function;
class Unnamed_label;
class Export_function_body;
class Import_function_body;
class Assignment_statement;
class Temporary_statement;
class Variable_declaration_statement;
class Expression_statement;
class Block_statement;
class Return_statement;
class Thunk_statement;
class Defer_statement;
class Goto_statement;
class Goto_unnamed_statement;
class Label_statement;
class Unnamed_label_statement;
class If_statement;
class For_statement;
class For_range_statement;
class Switch_statement;
class Type_switch_statement;
class Send_statement;
class Select_statement;
class Variable;
class Named_object;
class Label;
class Translate_context;
class Expression;
class Expression_list;
class Struct_type;
class Call_expression;
class Map_index_expression;
class Receive_expression;
class Case_clauses;
class Type_case_clauses;
class Select_clauses;
class Typed_identifier_list;
class Bexpression;
class Bstatement;
class Bvariable;
class Ast_dump_context;

// A single statement.

class Statement
{
 public:
  // The types of statements.
  enum Statement_classification
  {
    STATEMENT_ERROR,
    STATEMENT_VARIABLE_DECLARATION,
    STATEMENT_TEMPORARY,
    STATEMENT_ASSIGNMENT,
    STATEMENT_EXPRESSION,
    STATEMENT_BLOCK,
    STATEMENT_GO,
    STATEMENT_DEFER,
    STATEMENT_RETURN,
    STATEMENT_BREAK_OR_CONTINUE,
    STATEMENT_GOTO,
    STATEMENT_GOTO_UNNAMED,
    STATEMENT_LABEL,
    STATEMENT_UNNAMED_LABEL,
    STATEMENT_IF,
    STATEMENT_CONSTANT_SWITCH,
    STATEMENT_SEND,
    STATEMENT_SELECT,

    // These statements types are created by the parser, but they
    // disappear during the lowering pass.
    STATEMENT_ASSIGNMENT_OPERATION,
    STATEMENT_TUPLE_ASSIGNMENT,
    STATEMENT_TUPLE_MAP_ASSIGNMENT,
    STATEMENT_TUPLE_RECEIVE_ASSIGNMENT,
    STATEMENT_TUPLE_TYPE_GUARD_ASSIGNMENT,
    STATEMENT_INCDEC,
    STATEMENT_FOR,
    STATEMENT_FOR_RANGE,
    STATEMENT_SWITCH,
    STATEMENT_TYPE_SWITCH
  };

  Statement(Statement_classification, Location);

  virtual ~Statement();

  // Make a variable declaration.
  static Statement*
  make_variable_declaration(Named_object*);

  // Make a statement which creates a temporary variable and
  // initializes it to an expression.  The block is used if the
  // temporary variable has to be explicitly destroyed; the variable
  // must still be added to the block.  References to the temporary
  // variable may be constructed using make_temporary_reference.
  // Either the type or the initialization expression may be NULL, but
  // not both.
  static Temporary_statement*
  make_temporary(Type*, Expression*, Location);

  // Make an assignment statement.
  static Assignment_statement*
  make_assignment(Expression*, Expression*, Location);

  // Make an assignment operation (+=, etc.).
  static Statement*
  make_assignment_operation(Operator, Expression*, Expression*,
			    Location);

  // Make a tuple assignment statement.
  static Statement*
  make_tuple_assignment(Expression_list*, Expression_list*, Location);

  // Make an assignment from a map index to a pair of variables.
  static Statement*
  make_tuple_map_assignment(Expression* val, Expression* present,
			    Expression*, Location);

  // Make an assignment from a nonblocking receive to a pair of
  // variables.
  static Statement*
  make_tuple_receive_assignment(Expression* val, Expression* closed,
				Expression* channel, Location);

  // Make an assignment from a type guard to a pair of variables.
  static Statement*
  make_tuple_type_guard_assignment(Expression* val, Expression* ok,
				   Expression* expr, Type* type,
				   Location);

  // Make an expression statement from an Expression.  IS_IGNORED is
  // true if the value is being explicitly ignored, as in an
  // assignment to _.
  static Statement*
  make_statement(Expression*, bool is_ignored);

  // Make a block statement from a Block.  This is an embedded list of
  // statements which may also include variable definitions.
  static Block_statement*
  make_block_statement(Block*, Location);

  // Make an increment statement.
  static Statement*
  make_inc_statement(Expression*);

  // Make a decrement statement.
  static Statement*
  make_dec_statement(Expression*);

  // Make a go statement.
  static Statement*
  make_go_statement(Call_expression* call, Location);

  // Make a defer statement.
  static Statement*
  make_defer_statement(Call_expression* call, Location);

  // Make a return statement.  FUNCTION is a backpointer to the
  // function that this statement is returning from.
  static Return_statement*
  make_return_statement(Named_object* function, Expression_list*, Location);

  // Make a statement that returns the result of a call expression.
  // If the call does not return any results, this just returns the
  // call expression as a statement, assuming that the function will
  // end immediately afterward.
  static Statement*
  make_return_from_call(Named_object* function, Call_expression*, Location);

  // Make a break statement.
  static Statement*
  make_break_statement(Unnamed_label* label, Location);

  // Make a continue statement.
  static Statement*
  make_continue_statement(Unnamed_label* label, Location);

  // Make a goto statement.
  static Statement*
  make_goto_statement(Label* label, Location);

  // Make a goto statement to an unnamed label.
  static Statement*
  make_goto_unnamed_statement(Unnamed_label* label, Location);

  // Make a label statement--where the label is defined.
  static Statement*
  make_label_statement(Label* label, Location);

  // Make an unnamed label statement--where the label is defined.
  static Statement*
  make_unnamed_label_statement(Unnamed_label* label);

  // Make an if statement.
  static Statement*
  make_if_statement(Expression* cond, Block* then_block, Block* else_block,
		    Location);

  // Make a switch statement.
  static Switch_statement*
  make_switch_statement(Expression* switch_val, Location);

  // Make a type switch statement.
  static Type_switch_statement*
  make_type_switch_statement(Expression*, Location);

  // Make a send statement.
  static Send_statement*
  make_send_statement(Expression* channel, Expression* val, Location);

  // Make a select statement.
  static Select_statement*
  make_select_statement(Location);

  // Make a for statement.
  static For_statement*
  make_for_statement(Block* init, Expression* cond, Block* post,
		     Location location);

  // Make a for statement with a range clause.
  static For_range_statement*
  make_for_range_statement(Expression* index_var, Expression* value_var,
			   Expression* range, Location);

  // Return the statement classification.
  Statement_classification
  classification() const
  { return this->classification_; }

  // Get the statement location.
  Location
  location() const
  { return this->location_; }

  // Traverse the tree.
  int
  traverse(Block*, size_t* index, Traverse*);

  // Traverse the contents of this statement--the expressions and
  // statements which it contains.
  int
  traverse_contents(Traverse*);

  // Lower a statement.  This is called immediately after parsing to
  // simplify statements for further processing.  It returns the same
  // Statement or a new one.  FUNCTION is the function containing this
  // statement.  BLOCK is the block containing this statement.
  // INSERTER can be used to insert new statements before this one.
  Statement*
  lower(Gogo* gogo, Named_object* function, Block* block,
	Statement_inserter* inserter)
  { return this->do_lower(gogo, function, block, inserter); }

  // Flatten a statement.  This is called immediately after the order of
  // evaluation rules are applied to statements.  It returns the same
  // Statement or a new one.  FUNCTION is the function containing this
  // statement.  BLOCK is the block containing this statement.
  // INSERTER can be used to insert new statements before this one.
  Statement*
  flatten(Gogo* gogo, Named_object* function, Block* block,
          Statement_inserter* inserter)
  { return this->do_flatten(gogo, function, block, inserter); }

  // Set type information for unnamed constants.
  void
  determine_types(Gogo*);

  // Check types in a statement.  This simply checks that any
  // expressions used by the statement have the right type.
  void
  check_types(Gogo* gogo)
  { this->do_check_types(gogo); }

  // Return the cost of this statement for inlining purposes.
  int
  inlining_cost()
  { return this->do_inlining_cost(); }

  // Export data for this statement to BODY.
  void
  export_statement(Export_function_body* efb)
  { this->do_export_statement(efb); }

  // Make implicit type conversions explicit.
  void
  add_conversions()
  { this->do_add_conversions(); }

  // Read a statement from export data.  The location should be used
  // for the returned statement.  Errors should be reported using the
  // Import_function_body's location method.
  static Statement*
  import_statement(Import_function_body*, Location);

  // Return whether this is a block statement.
  bool
  is_block_statement() const
  { return this->classification_ == STATEMENT_BLOCK; }

  // If this is an assignment statement, return it.  Otherwise return
  // NULL.
  Assignment_statement*
  assignment_statement()
  {
    return this->convert<Assignment_statement, STATEMENT_ASSIGNMENT>();
  }

  // If this is an temporary statement, return it.  Otherwise return
  // NULL.
  Temporary_statement*
  temporary_statement()
  {
    return this->convert<Temporary_statement, STATEMENT_TEMPORARY>();
  }

  // If this is a variable declaration statement, return it.
  // Otherwise return NULL.
  Variable_declaration_statement*
  variable_declaration_statement()
  {
    return this->convert<Variable_declaration_statement,
			 STATEMENT_VARIABLE_DECLARATION>();
  }

  // If this is an expression statement, return it.  Otherwise return
  // NULL.
  Expression_statement*
  expression_statement()
  {
    return this->convert<Expression_statement, STATEMENT_EXPRESSION>();
  }

  // If this is an block statement, return it.  Otherwise return
  // NULL.
  Block_statement*
  block_statement()
  { return this->convert<Block_statement, STATEMENT_BLOCK>(); }

  // If this is a return statement, return it.  Otherwise return NULL.
  Return_statement*
  return_statement()
  { return this->convert<Return_statement, STATEMENT_RETURN>(); }

  // If this is a thunk statement (a go or defer statement), return
  // it.  Otherwise return NULL.
  Thunk_statement*
  thunk_statement();

  // If this is a defer statement, return it.  Otherwise return NULL.
  Defer_statement*
  defer_statement()
  { return this->convert<Defer_statement, STATEMENT_DEFER>(); }

  // If this is a goto statement, return it.  Otherwise return NULL.
  Goto_statement*
  goto_statement()
  { return this->convert<Goto_statement, STATEMENT_GOTO>(); }

  // If this is a goto_unnamed statement, return it.  Otherwise return NULL.
  Goto_unnamed_statement*
  goto_unnamed_statement()
  { return this->convert<Goto_unnamed_statement, STATEMENT_GOTO_UNNAMED>(); }

  // If this is a label statement, return it.  Otherwise return NULL.
  Label_statement*
  label_statement()
  { return this->convert<Label_statement, STATEMENT_LABEL>(); }

  // If this is an unnamed_label statement, return it.  Otherwise return NULL.
  Unnamed_label_statement*
  unnamed_label_statement()
  { return this->convert<Unnamed_label_statement, STATEMENT_UNNAMED_LABEL>(); }

  // If this is an if statement, return it.  Otherwise return NULL.
  If_statement*
  if_statement()
  { return this->convert<If_statement, STATEMENT_IF>(); }

  // If this is a for statement, return it.  Otherwise return NULL.
  For_statement*
  for_statement()
  { return this->convert<For_statement, STATEMENT_FOR>(); }

  // If this is a for statement over a range clause, return it.
  // Otherwise return NULL.
  For_range_statement*
  for_range_statement()
  { return this->convert<For_range_statement, STATEMENT_FOR_RANGE>(); }

  // If this is a switch statement, return it.  Otherwise return NULL.
  Switch_statement*
  switch_statement()
  { return this->convert<Switch_statement, STATEMENT_SWITCH>(); }

  // If this is a type switch statement, return it.  Otherwise return
  // NULL.
  Type_switch_statement*
  type_switch_statement()
  { return this->convert<Type_switch_statement, STATEMENT_TYPE_SWITCH>(); }

  // If this is a send statement, return it.  Otherwise return NULL.
  Send_statement*
  send_statement()
  { return this->convert<Send_statement, STATEMENT_SEND>(); }

  // If this is a select statement, return it.  Otherwise return NULL.
  Select_statement*
  select_statement()
  { return this->convert<Select_statement, STATEMENT_SELECT>(); }

  // Return true if this statement may fall through--if after
  // executing this statement we may go on to execute the following
  // statement, if any.
  bool
  may_fall_through() const
  { return this->do_may_fall_through(); }

  // Convert the statement to the backend representation.
  Bstatement*
  get_backend(Translate_context*);

  // Dump AST representation of a statement to a dump context.
  void
  dump_statement(Ast_dump_context*) const;

 protected:
  // Implemented by child class: traverse the tree.
  virtual int
  do_traverse(Traverse*) = 0;

  // Implemented by the child class: lower this statement to a simpler
  // one.
  virtual Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*)
  { return this; }

  // Implemented by the child class: lower this statement to a simpler
  // one.
  virtual Statement*
  do_flatten(Gogo*, Named_object*, Block*, Statement_inserter*)
  { return this; }

  // Implemented by child class: set type information for unnamed
  // constants.  Any statement which includes an expression needs to
  // implement this.
  virtual void
  do_determine_types(Gogo*)
  { }

  // Implemented by child class: check types of expressions used in a
  // statement.
  virtual void
  do_check_types(Gogo*)
  { }

  // Implemented by child class: return the cost of this statement for
  // inlining.  The default cost is high, so we only need to define
  // this method for statements that can be inlined.
  virtual int
  do_inlining_cost()
  { return 0x100000; }

  // Implemented by child class: write export data for this statement
  // to the string.  This need only be implemented by classes that
  // implement do_inlining_cost with a reasonable value.
  virtual void
  do_export_statement(Export_function_body*)
  { go_unreachable(); }

  // Implemented by child class: return true if this statement may
  // fall through.
  virtual bool
  do_may_fall_through() const
  { return true; }

  // Implemented by child class: convert to backend representation.
  virtual Bstatement*
  do_get_backend(Translate_context*) = 0;

  // Implemented by child class: dump ast representation.
  virtual void
  do_dump_statement(Ast_dump_context*) const = 0;

  // Implemented by child class: make implicit conversions explicit.
  virtual void
  do_add_conversions()
  { }

  // Traverse an expression in a statement.
  int
  traverse_expression(Traverse*, Expression**);

  // Traverse an expression list in a statement.  The Expression_list
  // may be NULL.
  int
  traverse_expression_list(Traverse*, Expression_list*);

  // Traverse a type in a statement.
  int
  traverse_type(Traverse*, Type*);

  // For children to call when they detect that they are in error.
  void
  set_is_error();

  // For children to call to report an error conveniently.
  void
  report_error(const char*);

  // For children to return an error statement from lower().
  static Statement*
  make_error_statement(Location);

 private:
  // Convert to the desired statement classification, or return NULL.
  // This is a controlled dynamic cast.
  template<typename Statement_class, Statement_classification sc>
  Statement_class*
  convert()
  {
    return (this->classification_ == sc
	    ? static_cast<Statement_class*>(this)
	    : NULL);
  }

  template<typename Statement_class, Statement_classification sc>
  const Statement_class*
  convert() const
  {
    return (this->classification_ == sc
	    ? static_cast<const Statement_class*>(this)
	    : NULL);
  }

  // The statement classification.
  Statement_classification classification_;
  // The location in the input file of the start of this statement.
  Location location_;
};

// An assignment statement.

class Assignment_statement : public Statement
{
 public:
  Assignment_statement(Expression* lhs, Expression* rhs,
		       Location location)
    : Statement(STATEMENT_ASSIGNMENT, location),
      lhs_(lhs), rhs_(rhs), omit_write_barrier_(false)
  { }

  Expression*
  lhs() const
  { return this->lhs_; }

  Expression*
  rhs() const
  { return this->rhs_; }

  bool
  omit_write_barrier() const
  { return this->omit_write_barrier_; }

  void
  set_omit_write_barrier()
  { this->omit_write_barrier_ = true; }

  // Check if we can assign RHS to LHS.  If we can, return true.  If
  // we can't, report an error and return false.
  static bool
  check_assignment_types(Expression* lhs, Type* rhs_type, Location);

 protected:
  int
  do_traverse(Traverse* traverse);

  virtual Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  int
  do_inlining_cost()
  { return 1; }

  void
  do_export_statement(Export_function_body*);

  Statement*
  do_flatten(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

  void
  do_add_conversions();

 private:
  // Left hand side--the lvalue.
  Expression* lhs_;
  // Right hand side--the rvalue.
  Expression* rhs_;
  // True if we can omit a write barrier from this assignment.
  bool omit_write_barrier_;
};

// A statement which creates and initializes a temporary variable.

class Temporary_statement : public Statement
{
 public:
  Temporary_statement(Type* type, Expression* init, Location location)
    : Statement(STATEMENT_TEMPORARY, location),
      type_(type), init_(init), bvariable_(NULL), is_address_taken_(false),
      value_escapes_(false), assigned_(false), uses_(0)
  { }

  // Return the type of the temporary variable.
  Type*
  type() const;

  // Return the initializer if there is one.
  Expression*
  init() const
  { return this->init_; }

  // Set the initializer.
  void
  set_init(Expression* expr)
  { this->init_ = expr; }

  // Whether something takes the address of this temporary
  // variable.
  bool
  is_address_taken()
  { return this->is_address_taken_; }

  // Record that something takes the address of this temporary
  // variable.
  void
  set_is_address_taken()
  { this->is_address_taken_ = true; }

  // Whether the value escapes.
  bool
  value_escapes() const
  { return this->value_escapes_; }

  // Record that the value escapes.
  void
  set_value_escapes()
  { this->value_escapes_ = true; }

  // Whether this temporary variable is assigned (after initialization).
  bool
  assigned()
  { return this->assigned_; }

  // Record that this temporary variable is assigned.
  void
  set_assigned()
  { this->assigned_ = true; }

  // Number of uses of this temporary variable.
  int
  uses()
  { return this->uses_; }

  // Add one use of this temporary variable.
  void
  add_use()
  { this->uses_++; }

  // Return the temporary variable.  This should not be called until
  // after the statement itself has been converted.
  Bvariable*
  get_backend_variable(Translate_context*) const;

  // Import the declaration of a temporary.
  static Statement*
  do_import(Import_function_body*, Location);

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  int
  do_inlining_cost()
  { return 1; }

  void
  do_export_statement(Export_function_body*);

  Statement*
  do_flatten(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

  void
  do_add_conversions();

 private:
  // The type of the temporary variable.
  Type* type_;
  // The initial value of the temporary variable.  This may be NULL.
  Expression* init_;
  // The backend representation of the temporary variable.
  Bvariable* bvariable_;
  // True if something takes the address of this temporary variable.
  bool is_address_taken_;
  // True if the value assigned to this temporary variable escapes.
  // This is used for select statements.
  bool value_escapes_;
  // True if this temporary variable is assigned (after initialization).
  bool assigned_;
  // Number of uses of this temporary variable.
  int uses_;
};

// A variable declaration.  This marks the point in the code where a
// variable is declared.  The Variable is also attached to a Block.

class Variable_declaration_statement : public Statement
{
 public:
  Variable_declaration_statement(Named_object* var);

  // The variable being declared.
  Named_object*
  var()
  { return this->var_; }

  // Import a variable declaration.
  static Statement*
  do_import(Import_function_body*, Location);

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  int
  do_inlining_cost()
  { return 1; }

  void
  do_export_statement(Export_function_body*);

  Statement*
  do_flatten(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

  void
  do_add_conversions();

 private:
  Named_object* var_;
};

// A return statement.

class Return_statement : public Statement
{
 public:
  Return_statement(Named_object* function, Expression_list* vals,
		   Location location)
    : Statement(STATEMENT_RETURN, location),
      function_(function), vals_(vals), types_are_determined_(false),
      is_lowered_(false)
  { }

  // The list of values being returned.  This may be NULL.
  const Expression_list*
  vals() const
  { return this->vals_; }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->traverse_expression_list(traverse, this->vals_); }

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  bool
  do_may_fall_through() const
  { return false; }

  int
  do_inlining_cost()
  { return 1; }

  void
  do_export_statement(Export_function_body*);

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // A backpointer to the function we are returning from.
  Named_object* function_;
  // Return values.  This may be NULL.
  Expression_list* vals_;
  // True if types have been determined.
  bool types_are_determined_;
  // True if this statement has been lowered.
  bool is_lowered_;
};

// An expression statement.

class Expression_statement : public Statement
{
 public:
  Expression_statement(Expression* expr, bool is_ignored);

  Expression*
  expr()
  { return this->expr_; }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->traverse_expression(traverse, &this->expr_); }

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  bool
  do_may_fall_through() const;

  int
  do_inlining_cost()
  { return 0; }

  void
  do_export_statement(Export_function_body*);

  Bstatement*
  do_get_backend(Translate_context* context);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Expression* expr_;
  // Whether the value of this expression is being explicitly ignored.
  bool is_ignored_;
};

// A block statement--a list of statements which may include variable
// definitions.

class Block_statement : public Statement
{
 public:
  Block_statement(Block* block, Location location)
    : Statement(STATEMENT_BLOCK, location),
      block_(block), is_lowered_for_statement_(false)
  { }

  // Return the actual block.
  Block*
  block() const
  { return this->block_; }

  void
  set_is_lowered_for_statement()
  { this->is_lowered_for_statement_ = true; }

  bool
  is_lowered_for_statement()
  { return this->is_lowered_for_statement_; }

  // Export a block for a block statement.
  static void
  export_block(Export_function_body*, Block*, bool is_lowered_for_statement);

  // Import a block statement, returning the block.
  // *IS_LOWERED_FOR_STATEMENT reports whether this block statement
  // was lowered from a for statement.
  static Block*
  do_import(Import_function_body*, Location, bool* is_lowered_for_statement);

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->block_->traverse(traverse); }

  void
  do_determine_types(Gogo* gogo)
  { this->block_->determine_types(gogo); }

  int
  do_inlining_cost()
  { return 0; }

  void
  do_export_statement(Export_function_body*);

  bool
  do_may_fall_through() const
  { return this->block_->may_fall_through(); }

  Bstatement*
  do_get_backend(Translate_context* context);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Block* block_;
  // True if this block statement represents a lowered for statement.
  bool is_lowered_for_statement_;
};

// A send statement.

class Send_statement : public Statement
{
 public:
  Send_statement(Expression* channel, Expression* val,
		 Location location)
    : Statement(STATEMENT_SEND, location),
      channel_(channel), val_(val)
  { }

  Expression*
  channel()
  { return this->channel_; }

  Expression*
  val()
  { return this->val_; }

 protected:
  int
  do_traverse(Traverse* traverse);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  Statement*
  do_flatten(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

  void
  do_add_conversions();

 private:
  // The channel on which to send the value.
  Expression* channel_;
  // The value to send.
  Expression* val_;
};

// Select_clauses holds the clauses of a select statement.  This is
// built by the parser.

class Select_clauses
{
 public:
  Select_clauses()
    : clauses_()
  { }

  // Add a new clause.  IS_SEND is true if this is a send clause,
  // false for a receive clause.  For a send clause CHANNEL is the
  // channel and VAL is the value to send.  For a receive clause
  // CHANNEL is the channel, VAL is either NULL or a Var_expression
  // for the variable to set, and CLOSED is either NULL or a
  // Var_expression to set to whether the channel is closed.  If VAL
  // is NULL, VAR may be a variable to be initialized with the
  // received value, and CLOSEDVAR may be a variable to be initialized
  // with whether the channel is closed.  IS_DEFAULT is true if this
  // is the default clause.  STATEMENTS is the list of statements to
  // execute.
  void
  add(bool is_send, Expression* channel, Expression* val, Expression* closed,
      Named_object* var, Named_object* closedvar, bool is_default,
      Block* statements, Location location)
  {
    this->clauses_.push_back(Select_clause(is_send, channel, val, closed, var,
					   closedvar, is_default, statements,
					   location));
  }

  size_t
  size() const
  { return this->clauses_.size(); }

  bool
  has_default() const;

  // Traverse the select clauses.
  int
  traverse(Traverse*);

  // Lower statements.
  void
  lower(Gogo*, Named_object*, Block*, Temporary_statement*,
	Temporary_statement*, int* send_count, int* recv_count);

  // Determine types.
  void
  determine_types(Gogo*);

  // Check types.
  void
  check_types();

  // Whether the select clauses may fall through to the statement
  // which follows the overall select statement.
  bool
  may_fall_through() const;

  // Convert to the backend representation.
  Bstatement*
  get_backend(Translate_context*, Temporary_statement* index,
	      Unnamed_label* break_label, Location);

  // Dump AST representation.
  void
  dump_clauses(Ast_dump_context*) const;

  // A single clause.
  class Select_clause
  {
   public:
    Select_clause()
      : channel_(NULL), val_(NULL), closed_(NULL), var_(NULL),
	closedvar_(NULL), statements_(NULL), is_send_(false),
	is_default_(false)
    { }

    Select_clause(bool is_send, Expression* channel, Expression* val,
		  Expression* closed, Named_object* var,
		  Named_object* closedvar, bool is_default, Block* statements,
		  Location location)
      : channel_(channel), val_(val), closed_(closed), var_(var),
	closedvar_(closedvar), statements_(statements), case_index_(0),
	location_(location), is_send_(is_send), is_default_(is_default),
	is_lowered_(false), is_case_index_set_(false)
    { go_assert(is_default ? channel == NULL : channel != NULL); }

    // Traverse the select clause.
    int
    traverse(Traverse*);

    // Lower statements.
    void
    lower(Gogo*, Named_object*, Block*, Temporary_statement*, int,
	  Temporary_statement*);

    // Determine types.
    void
    determine_types(Gogo*);

    // Check types.
    void
    check_types();

    // Return true if this is the default clause.
    bool
    is_default() const
    { return this->is_default_; }

    // Return the channel.  This will return NULL for the default
    // clause.
    Expression*
    channel() const
    { return this->channel_; }

    // Return true for a send, false for a receive.
    bool
    is_send() const
    {
      go_assert(!this->is_default_);
      return this->is_send_;
    }

    // Return the value to send or the lvalue to receive into.
    Expression*
    val() const
    { return this->val_; }

    // Return the lvalue to set to whether the channel is closed
    // on a receive.
    Expression*
    closed() const
    { return this->closed_; }

    // Return the variable to initialize, for "case a := <-ch".
    Named_object*
    var() const
    { return this->var_; }

    // Return the variable to initialize to whether the channel
    // is closed, for "case a, c := <-ch".
    Named_object*
    closedvar() const
    { return this->closedvar_; }

    // Return the statements.
    Block*
    statements() const
    { return this->statements_; }

    // Return the location.
    Location
    location() const
    { return this->location_; }

    // Return the case index for this clause.
    int
    case_index() const
    {
      go_assert(this->is_case_index_set_);
      return this->case_index_;
    }

    // Set the case index.
    void
    set_case_index(int i)
    {
      go_assert(!this->is_case_index_set_);
      this->case_index_ = i;
      this->is_case_index_set_ = true;
    }

    // Whether this clause may fall through to the statement which
    // follows the overall select statement.
    bool
    may_fall_through() const;

    // Convert the statements to the backend representation.
    Bstatement*
    get_statements_backend(Translate_context*);

    // Dump AST representation.
    void
    dump_clause(Ast_dump_context*) const;

   private:
    void
    lower_send(Gogo*, Block*, Expression*, Expression*);

    void
    lower_recv(Gogo*, Named_object*, Block*, Expression*, Expression*,
	       Temporary_statement*);

    void
    set_case(Gogo*, Block*, Expression*, Expression*, Expression*);

    // The channel.
    Expression* channel_;
    // The value to send or the lvalue to receive into.
    Expression* val_;
    // The lvalue to set to whether the channel is closed on a
    // receive.
    Expression* closed_;
    // The variable to initialize, for "case a := <-ch".
    Named_object* var_;
    // The variable to initialize to whether the channel is closed,
    // for "case a, c := <-ch".
    Named_object* closedvar_;
    // The statements to execute.
    Block* statements_;
    // The index of this clause in the switch statement.  If
    // runtime.selectgo returns this index, this clause has been
    // chosen.
    int case_index_;
    // The location of this clause.
    Location location_;
    // Whether this is a send or a receive.
    bool is_send_;
    // Whether this is the default.
    bool is_default_;
    // Whether this has been lowered.
    bool is_lowered_;
    // Whether the case index has been set.
    bool is_case_index_set_;
  };

  Select_clause&
  at(size_t i)
  { return this->clauses_.at(i); }

 private:
  typedef std::vector<Select_clause> Clauses;

  Clauses clauses_;
};

// A select statement.

class Select_statement : public Statement
{
 public:
  Select_statement(Location location)
    : Statement(STATEMENT_SELECT, location),
      clauses_(NULL), index_(NULL), break_label_(NULL), is_lowered_(false)
  { }

  // Add the clauses.
  void
  add_clauses(Select_clauses* clauses)
  {
    go_assert(this->clauses_ == NULL);
    this->clauses_ = clauses;
  }

  // Return the break label for this select statement.
  Unnamed_label*
  break_label();

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->clauses_->traverse(traverse); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  void
  do_determine_types(Gogo* gogo)
  { this->clauses_->determine_types(gogo); }

  void
  do_check_types(Gogo*)
  { this->clauses_->check_types(); }

  bool
  do_may_fall_through() const;

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // Lower a one-case select statement.
  Statement*
  lower_one_case(Gogo*, Block*);

  // Lower a two-case select statement with one defualt case.
  Statement*
  lower_two_case(Gogo*, Block*);

  // The select clauses.
  Select_clauses* clauses_;
  // A temporary that holds the index value returned by selectgo.
  Temporary_statement* index_;
  // The break label.
  Unnamed_label* break_label_;
  // Whether this statement has been lowered.
  bool is_lowered_;
};

// A statement which requires a thunk: go or defer.

class Thunk_statement : public Statement
{
 public:
  Thunk_statement(Statement_classification, Call_expression*,
		  Location);

  // Return the call expression.
  Expression*
  call() const
  { return this->call_; }

  // Simplify a go or defer statement so that it only uses a single
  // parameter.
  bool
  simplify_statement(Gogo*, Named_object*, Block*);

 protected:
  int
  do_traverse(Traverse* traverse);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  // Return the function and argument for the call.
  bool
  get_fn_and_arg(Expression** pfn, Expression** parg);

 private:
  // Return whether this is a simple go statement.
  bool
  is_simple(Function_type*) const;

  // Return whether the thunk function is a constant.
  bool
  is_constant_function() const;

  // Build the struct to use for a complex case.
  Struct_type*
  build_struct(Function_type* fntype);

  // Build the thunk.
  void
  build_thunk(Gogo*, const std::string&, Struct_type*);

  // Set the name to use for thunk field N.
  void
  thunk_field_param(int n, char* buf, size_t buflen);

  // The function call to be executed in a separate thread (go) or
  // later (defer).
  Expression* call_;
};

// A go statement.

class Go_statement : public Thunk_statement
{
 public:
  Go_statement(Call_expression* call, Location location)
    : Thunk_statement(STATEMENT_GO, call, location)
  { }

 protected:
  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;
};

// A defer statement.

class Defer_statement : public Thunk_statement
{
 public:
  Defer_statement(Call_expression* call, Location location)
    : Thunk_statement(STATEMENT_DEFER, call, location),
      on_stack_(false)
  { }

  void
  set_on_stack()
  { this->on_stack_ = true; }

 protected:
  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  static Type*
  defer_struct_type();

  bool on_stack_;
};

// A goto statement.

class Goto_statement : public Statement
{
 public:
  Goto_statement(Label* label, Location location)
    : Statement(STATEMENT_GOTO, location),
      label_(label)
  { }

  // Return the label being jumped to.
  Label*
  label() const
  { return this->label_; }

  // Import a goto statement.
  static Statement*
  do_import(Import_function_body*, Location);

 protected:
  int
  do_traverse(Traverse*);

  void
  do_check_types(Gogo*);

  bool
  do_may_fall_through() const
  { return false; }

  Bstatement*
  do_get_backend(Translate_context*);

  int
  do_inlining_cost()
  { return 5; }

  void
  do_export_statement(Export_function_body*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Label* label_;
};

// A goto statement to an unnamed label.

class Goto_unnamed_statement : public Statement
{
 public:
  Goto_unnamed_statement(Unnamed_label* label, Location location)
    : Statement(STATEMENT_GOTO_UNNAMED, location),
      label_(label)
  { }

  Unnamed_label*
  unnamed_label() const
  { return this->label_; }

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_may_fall_through() const
  { return false; }

  Bstatement*
  do_get_backend(Translate_context* context);

  int
  do_inlining_cost()
  { return 5; }

  void
  do_export_statement(Export_function_body*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Unnamed_label* label_;
};

// A label statement.

class Label_statement : public Statement
{
 public:
  Label_statement(Label* label, Location location)
    : Statement(STATEMENT_LABEL, location),
      label_(label)
  { }

  // Return the label itself.
  Label*
  label() const
  { return this->label_; }

  // Import a label or unnamed label.
  static Statement*
  do_import(Import_function_body*, Location);

 protected:
  int
  do_traverse(Traverse*);

  Bstatement*
  do_get_backend(Translate_context*);

  int
  do_inlining_cost()
  { return 1; }

  void
  do_export_statement(Export_function_body*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // The label.
  Label* label_;
};

// An unnamed label statement.

class Unnamed_label_statement : public Statement
{
 public:
  Unnamed_label_statement(Unnamed_label* label);

 protected:
  int
  do_traverse(Traverse*);

  Bstatement*
  do_get_backend(Translate_context* context);

  int
  do_inlining_cost()
  { return 1; }

  void
  do_export_statement(Export_function_body*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // The label.
  Unnamed_label* label_;
};

// An if statement.

class If_statement : public Statement
{
 public:
  If_statement(Expression* cond, Block* then_block, Block* else_block,
	       Location location)
    : Statement(STATEMENT_IF, location),
      cond_(cond), then_block_(then_block), else_block_(else_block)
  { }

  Expression*
  condition() const
  { return this->cond_; }

  Block*
  then_block() const
  { return this->then_block_; }

  Block*
  else_block() const
  { return this->else_block_; }

  // Import an if statement.
  static Statement*
  do_import(Import_function_body*, Location);

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  int
  do_inlining_cost()
  { return 5; }

  void
  do_export_statement(Export_function_body*);

  bool
  do_may_fall_through() const;

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Expression* cond_;
  Block* then_block_;
  Block* else_block_;
};

// A for statement.

class For_statement : public Statement
{
 public:
  For_statement(Block* init, Expression* cond, Block* post,
		Location location)
    : Statement(STATEMENT_FOR, location),
      init_(init), cond_(cond), post_(post), statements_(NULL),
      break_label_(NULL), continue_label_(NULL)
  { }

  // Add the statements.
  void
  add_statements(Block* statements)
  {
    go_assert(this->statements_ == NULL);
    this->statements_ = statements;
  }

  // Return the break label for this for statement.
  Unnamed_label*
  break_label();

  // Return the continue label for this for statement.
  Unnamed_label*
  continue_label();

  // Set the break and continue labels for this statement.
  void
  set_break_continue_labels(Unnamed_label* break_label,
			    Unnamed_label* continue_label);

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  bool
  do_may_fall_through() const;

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // The initialization statements.  This may be NULL.
  Block* init_;
  // The condition.  This may be NULL.
  Expression* cond_;
  // The statements to run after each iteration.  This may be NULL.
  Block* post_;
  // The statements in the loop itself.
  Block* statements_;
  // The break label, if needed.
  Unnamed_label* break_label_;
  // The continue label, if needed.
  Unnamed_label* continue_label_;
};

// A for statement over a range clause.

class For_range_statement : public Statement
{
 public:
  For_range_statement(Expression* index_var, Expression* value_var,
		      Expression* range, Location location)
    : Statement(STATEMENT_FOR_RANGE, location),
      index_var_(index_var), value_var_(value_var), range_(range),
      statements_(NULL), break_label_(NULL), continue_label_(NULL)
  { }

  // Add the statements.
  void
  add_statements(Block* statements)
  {
    go_assert(this->statements_ == NULL);
    this->statements_ = statements;
  }

  // Return the break label for this for statement.
  Unnamed_label*
  break_label();

  // Return the continue label for this for statement.
  Unnamed_label*
  continue_label();

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Expression*
  make_range_ref(Named_object*, Temporary_statement*, Location);

  Call_expression*
  call_builtin(Gogo*, const char* funcname, Expression* arg, Location);

  void
  lower_range_array(Gogo*, Block*, Block*, Named_object*, Temporary_statement*,
		    Temporary_statement*, Temporary_statement*,
		    Block**, Expression**, Block**, Block**);

  void
  lower_range_slice(Gogo*, Block*, Block*, Named_object*, Temporary_statement*,
		    Temporary_statement*, Temporary_statement*,
		    Block**, Expression**, Block**, Block**);

  void
  lower_range_string(Gogo*, Block*, Block*, Named_object*, Temporary_statement*,
		     Temporary_statement*, Temporary_statement*,
		     Block**, Expression**, Block**, Block**);

  void
  lower_range_map(Gogo*, Map_type*, Block*, Block*, Named_object*,
		  Temporary_statement*, Temporary_statement*,
		  Temporary_statement*, Block**, Expression**, Block**,
		  Block**);

  void
  lower_range_channel(Gogo*, Block*, Block*, Named_object*,
		      Temporary_statement*, Temporary_statement*,
		      Temporary_statement*, Block**, Expression**, Block**,
		      Block**);

  Statement*
  lower_map_range_clear(Gogo*, Type*, Block*, Expression*, Named_object*,
                        Temporary_statement*, Location);

  Statement*
  lower_array_range_clear(Gogo*, Type*, Expression*, Block*,
                          Named_object*, Temporary_statement*,
                          Location);

  // The variable which is set to the index value.
  Expression* index_var_;
  // The variable which is set to the element value.  This may be
  // NULL.
  Expression* value_var_;
  // The expression we are ranging over.
  Expression* range_;
  // The statements in the block.
  Block* statements_;
  // The break label, if needed.
  Unnamed_label* break_label_;
  // The continue label, if needed.
  Unnamed_label* continue_label_;
};

// Class Case_clauses holds the clauses of a switch statement.  This
// is built by the parser.

class Case_clauses
{
 public:
  Case_clauses()
    : clauses_()
  { }

  // Add a new clause.  CASES is a list of case expressions; it may be
  // NULL.  IS_DEFAULT is true if this is the default case.
  // STATEMENTS is a block of statements.  IS_FALLTHROUGH is true if
  // after the statements the case clause should fall through to the
  // next clause.
  void
  add(Expression_list* cases, bool is_default, Block* statements,
      bool is_fallthrough, Location location)
  {
    this->clauses_.push_back(Case_clause(cases, is_default, statements,
					 is_fallthrough, location));
  }

  // Return whether there are no clauses.
  bool
  empty() const
  { return this->clauses_.empty(); }

  // Traverse the case clauses.
  int
  traverse(Traverse*);

  // Lower for a nonconstant switch.
  void
  lower(Gogo*, Block*, Temporary_statement*, Unnamed_label*) const;

  // Determine types of expressions.  The Type parameter is the type
  // of the switch value.
  void
  determine_types(Gogo*, Type*);

  // Check types.  The Type parameter is the type of the switch value.
  bool
  check_types(Type*);

  // Return true if all the clauses are constant values.
  bool
  is_constant() const;

  // Return true if these clauses may fall through to the statements
  // following the switch statement.
  bool
  may_fall_through() const;

  // Return the body of a SWITCH_EXPR when all the clauses are
  // constants.
  void
  get_backend(Translate_context*, Unnamed_label* break_label,
	      std::vector<std::vector<Bexpression*> >* all_cases,
	      std::vector<Bstatement*>* all_statements) const;

  // Dump the AST representation to a dump context.
  void
  dump_clauses(Ast_dump_context*) const;

 private:
  // For a constant switch we need to keep a record of constants we
  // have already seen.
  class Hash_integer_value;
  class Eq_integer_value;
  typedef Unordered_set_hash(Expression*, Hash_integer_value,
			     Eq_integer_value) Case_constants;

  // One case clause.
  class Case_clause
  {
   public:
    Case_clause()
      : cases_(NULL), statements_(NULL), is_default_(false),
	is_fallthrough_(false), location_(Linemap::unknown_location())
    { }

    Case_clause(Expression_list* cases, bool is_default, Block* statements,
		bool is_fallthrough, Location location)
      : cases_(cases), statements_(statements), is_default_(is_default),
	is_fallthrough_(is_fallthrough), location_(location)
    { }

    // Whether this clause falls through to the next clause.
    bool
    is_fallthrough() const
    { return this->is_fallthrough_; }

    // Whether this is the default.
    bool
    is_default() const
    { return this->is_default_; }

    // The location of this clause.
    Location
    location() const
    { return this->location_; }

    // Traversal.
    int
    traverse(Traverse*);

    // Lower for a nonconstant switch.
    void
    lower(Gogo*, Block*, Temporary_statement*, Unnamed_label*,
	  Unnamed_label*) const;

    // Determine types.
    void
    determine_types(Gogo*, Type*);

    // Check types.
    bool
    check_types(Type*);

    // Return true if all the case expressions are constant.
    bool
    is_constant() const;

    // Return true if this clause may fall through to execute the
    // statements following the switch statement.  This is not the
    // same as whether this clause falls through to the next clause.
    bool
    may_fall_through() const;

    // Convert the case values and statements to the backend
    // representation.
    Bstatement*
    get_backend(Translate_context*, Unnamed_label* break_label,
		Case_constants*, std::vector<Bexpression*>* cases) const;

    // Dump the AST representation to a dump context.
    void
    dump_clause(Ast_dump_context*) const;

   private:
    // The list of case expressions.
    Expression_list* cases_;
    // The statements to execute.
    Block* statements_;
    // Whether this is the default case.
    bool is_default_;
    // Whether this falls through after the statements.
    bool is_fallthrough_;
    // The location of this case clause.
    Location location_;
  };

  friend class Case_clause;

  // The type of the list of clauses.
  typedef std::vector<Case_clause> Clauses;

  // All the case clauses.
  Clauses clauses_;
};

// A switch statement.

class Switch_statement : public Statement
{
 public:
  Switch_statement(Expression* val, Location location)
    : Statement(STATEMENT_SWITCH, location),
      val_(val), clauses_(NULL), break_label_(NULL)
  { }

  // Add the clauses.
  void
  add_clauses(Case_clauses* clauses)
  {
    go_assert(this->clauses_ == NULL);
    this->clauses_ = clauses;
  }

  // Return the break label for this switch statement.
  Unnamed_label*
  break_label();

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

  bool
  do_may_fall_through() const;

 private:
  // The value to switch on.  This may be NULL.
  Expression* val_;
  // The case clauses.
  Case_clauses* clauses_;
  // The break label, if needed.
  Unnamed_label* break_label_;
};

// Class Type_case_clauses holds the clauses of a type switch
// statement.  This is built by the parser.

class Type_case_clauses
{
 public:
  Type_case_clauses()
    : clauses_()
  { }

  // Add a new clause.  TYPE is the type for this clause; it may be
  // NULL.  IS_FALLTHROUGH is true if this falls through to the next
  // clause; in this case STATEMENTS will be NULL.  IS_DEFAULT is true
  // if this is the default case.  STATEMENTS is a block of
  // statements; it may be NULL.
  void
  add(Type* type, bool is_fallthrough, bool is_default, Block* statements,
      Location location)
  {
    this->clauses_.push_back(Type_case_clause(type, is_fallthrough, is_default,
					      statements, location));
  }

  // Return whether there are no clauses.
  bool
  empty() const
  { return this->clauses_.empty(); }

  // Traverse the type case clauses.
  int
  traverse(Traverse*);

  // Check for duplicates.
  void
  check_duplicates() const;

  // Determine types of expressions.
  void
  determine_types(Gogo*);

  // Check types.
  bool
  check_types(Type*);

  // Lower to if and goto statements.
  void
  lower(Gogo*, Block*, Temporary_statement* descriptor_temp,
	Unnamed_label* break_label) const;

  // Return true if these clauses may fall through to the statements
  // following the switch statement.
  bool
  may_fall_through() const;

  // Dump the AST representation to a dump context.
  void
  dump_clauses(Ast_dump_context*) const;

 private:
  // One type case clause.
  class Type_case_clause
  {
   public:
    Type_case_clause()
      : type_(NULL), statements_(NULL), is_default_(false),
	location_(Linemap::unknown_location())
    { }

    Type_case_clause(Type* type, bool is_fallthrough, bool is_default,
		     Block* statements, Location location)
      : type_(type), statements_(statements), is_fallthrough_(is_fallthrough),
	is_default_(is_default), location_(location)
    { }

    // The type.
    Type*
    type() const
    { return this->type_; }

    // Whether this is the default.
    bool
    is_default() const
    { return this->is_default_; }

    // The location of this type clause.
    Location
    location() const
    { return this->location_; }

    // Traversal.
    int
    traverse(Traverse*);

    // Determine types.
    void
    determine_types(Gogo*);

    // Check types.
    bool
    check_types(Type*);

    // Lower to if and goto statements.
    void
    lower(Gogo*, Block*, Temporary_statement* descriptor_temp,
	  Unnamed_label* break_label, Unnamed_label** stmts_label) const;

    // Return true if this clause may fall through to execute the
    // statements following the switch statement.  This is not the
    // same as whether this clause falls through to the next clause.
    bool
    may_fall_through() const;

    // Dump the AST representation to a dump context.
    void
    dump_clause(Ast_dump_context*) const;

   private:
    // The type for this type clause.
    Type* type_;
    // The statements to execute.
    Block* statements_;
    // Whether this falls through--this is true for "case T1, T2".
    bool is_fallthrough_;
    // Whether this is the default case.
    bool is_default_;
    // The location of this type case clause.
    Location location_;
  };

  friend class Type_case_clause;

  // The type of the list of type clauses.
  typedef std::vector<Type_case_clause> Type_clauses;

  // All the type case clauses.
  Type_clauses clauses_;
};

// A type switch statement.

class Type_switch_statement : public Statement
{
 public:
  Type_switch_statement(Expression* expr, Location location)
    : Statement(STATEMENT_TYPE_SWITCH, location),
      expr_(expr), clauses_(NULL), break_label_(NULL)
  { }

  // Add the clauses.
  void
  add_clauses(Type_case_clauses* clauses)
  {
    go_assert(this->clauses_ == NULL);
    this->clauses_ = clauses;
  }

  // Return the break label for this type switch statement.
  Unnamed_label*
  break_label();

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types(Gogo*);

  void
  do_check_types(Gogo*);

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

  bool
  do_may_fall_through() const;

 private:
  // The expression we are switching on.
  Expression* expr_;
  // The type case clauses.
  Type_case_clauses* clauses_;
  // The break label, if needed.
  Unnamed_label* break_label_;
};

#endif // !defined(GO_STATEMENTS_H)
