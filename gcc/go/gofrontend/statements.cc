// statements.cc -- Go frontend statements.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "intl.h"
#include "tree.h"
#include "gimple.h"
#include "convert.h"
#include "tree-iterator.h"
#include "tree-flow.h"
#include "real.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "go-c.h"
#include "types.h"
#include "expressions.h"
#include "gogo.h"
#include "statements.h"

// Class Statement.

Statement::Statement(Statement_classification classification,
		     source_location location)
  : classification_(classification), location_(location)
{
}

Statement::~Statement()
{
}

// Traverse the tree.  The work of walking the components is handled
// by the subclasses.

int
Statement::traverse(Block* block, size_t* pindex, Traverse* traverse)
{
  if (this->classification_ == STATEMENT_ERROR)
    return TRAVERSE_CONTINUE;

  unsigned int traverse_mask = traverse->traverse_mask();

  if ((traverse_mask & Traverse::traverse_statements) != 0)
    {
      int t = traverse->statement(block, pindex, this);
      if (t == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
      else if (t == TRAVERSE_SKIP_COMPONENTS)
	return TRAVERSE_CONTINUE;
    }

  // No point in checking traverse_mask here--a statement may contain
  // other blocks or statements, and if we got here we always want to
  // walk them.
  return this->do_traverse(traverse);
}

// Traverse the contents of a statement.

int
Statement::traverse_contents(Traverse* traverse)
{
  return this->do_traverse(traverse);
}

// Traverse assignments.

bool
Statement::traverse_assignments(Traverse_assignments* tassign)
{
  if (this->classification_ == STATEMENT_ERROR)
    return false;
  return this->do_traverse_assignments(tassign);
}

// Traverse an expression in a statement.  This is a helper function
// for child classes.

int
Statement::traverse_expression(Traverse* traverse, Expression** expr)
{
  if ((traverse->traverse_mask()
       & (Traverse::traverse_types | Traverse::traverse_expressions)) == 0)
    return TRAVERSE_CONTINUE;
  return Expression::traverse(expr, traverse);
}

// Traverse an expression list in a statement.  This is a helper
// function for child classes.

int
Statement::traverse_expression_list(Traverse* traverse,
				    Expression_list* expr_list)
{
  if (expr_list == NULL)
    return TRAVERSE_CONTINUE;
  if ((traverse->traverse_mask()
       & (Traverse::traverse_types | Traverse::traverse_expressions)) == 0)
    return TRAVERSE_CONTINUE;
  return expr_list->traverse(traverse);
}

// Traverse a type in a statement.  This is a helper function for
// child classes.

int
Statement::traverse_type(Traverse* traverse, Type* type)
{
  if ((traverse->traverse_mask()
       & (Traverse::traverse_types | Traverse::traverse_expressions)) == 0)
    return TRAVERSE_CONTINUE;
  return Type::traverse(type, traverse);
}

// Set type information for unnamed constants.  This is really done by
// the child class.

void
Statement::determine_types()
{
  this->do_determine_types();
}

// If this is a thunk statement, return it.

Thunk_statement*
Statement::thunk_statement()
{
  Thunk_statement* ret = this->convert<Thunk_statement, STATEMENT_GO>();
  if (ret == NULL)
    ret = this->convert<Thunk_statement, STATEMENT_DEFER>();
  return ret;
}

// Get a tree for a Statement.  This is really done by the child
// class.

tree
Statement::get_tree(Translate_context* context)
{
  if (this->classification_ == STATEMENT_ERROR)
    return error_mark_node;

  return this->do_get_tree(context);
}

// Build tree nodes and set locations.

tree
Statement::build_stmt_1(int tree_code_value, tree node)
{
  tree ret = build1(static_cast<tree_code>(tree_code_value),
		    void_type_node, node);
  SET_EXPR_LOCATION(ret, this->location_);
  return ret;
}

// Note that this statement is erroneous.  This is called by children
// when they discover an error.

void
Statement::set_is_error()
{
  this->classification_ = STATEMENT_ERROR;
}

// For children to call to report an error conveniently.

void
Statement::report_error(const char* msg)
{
  error_at(this->location_, "%s", msg);
  this->set_is_error();
}

// An error statement, used to avoid crashing after we report an
// error.

class Error_statement : public Statement
{
 public:
  Error_statement(source_location location)
    : Statement(STATEMENT_ERROR, location)
  { }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }
};

// Make an error statement.

Statement*
Statement::make_error_statement(source_location location)
{
  return new Error_statement(location);
}

// Class Variable_declaration_statement.

Variable_declaration_statement::Variable_declaration_statement(
    Named_object* var)
  : Statement(STATEMENT_VARIABLE_DECLARATION, var->var_value()->location()),
    var_(var)
{
}

// We don't actually traverse the variable here; it was traversed
// while traversing the Block.

int
Variable_declaration_statement::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Traverse the assignments in a variable declaration.  Note that this
// traversal is different from the usual traversal.

bool
Variable_declaration_statement::do_traverse_assignments(
    Traverse_assignments* tassign)
{
  tassign->initialize_variable(this->var_);
  return true;
}

// Return the tree for a variable declaration.

tree
Variable_declaration_statement::do_get_tree(Translate_context* context)
{
  tree val = this->var_->get_tree(context->gogo(), context->function());
  if (val == error_mark_node || TREE_TYPE(val) == error_mark_node)
    return error_mark_node;
  Variable* variable = this->var_->var_value();

  tree init = variable->get_init_tree(context->gogo(), context->function());
  if (init == error_mark_node)
    return error_mark_node;

  // If this variable lives on the heap, we need to allocate it now.
  if (!variable->is_in_heap())
    {
      DECL_INITIAL(val) = init;
      return this->build_stmt_1(DECL_EXPR, val);
    }
  else
    {
      gcc_assert(TREE_CODE(val) == INDIRECT_REF);
      tree decl = TREE_OPERAND(val, 0);
      gcc_assert(TREE_CODE(decl) == VAR_DECL);
      tree type = TREE_TYPE(decl);
      gcc_assert(POINTER_TYPE_P(type));
      tree size = TYPE_SIZE_UNIT(TREE_TYPE(type));
      tree space = context->gogo()->allocate_memory(variable->type(), size,
						    this->location());
      space = fold_convert(TREE_TYPE(decl), space);
      DECL_INITIAL(decl) = space;
      return build2(COMPOUND_EXPR, void_type_node,
		    this->build_stmt_1(DECL_EXPR, decl),
		    build2(MODIFY_EXPR, void_type_node, val, init));
    }
}

// Make a variable declaration.

Statement*
Statement::make_variable_declaration(Named_object* var)
{
  return new Variable_declaration_statement(var);
}

// Class Temporary_statement.

// Return the type of the temporary variable.

Type*
Temporary_statement::type() const
{
  return this->type_ != NULL ? this->type_ : this->init_->type();
}

// Return the tree for the temporary variable.

tree
Temporary_statement::get_decl() const
{
  if (this->decl_ == NULL)
    {
      gcc_assert(saw_errors());
      return error_mark_node;
    }
  return this->decl_;
}

// Traversal.

int
Temporary_statement::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL
      && this->traverse_type(traverse, this->type_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->init_ == NULL)
    return TRAVERSE_CONTINUE;
  else
    return this->traverse_expression(traverse, &this->init_);
}

// Traverse assignments.

bool
Temporary_statement::do_traverse_assignments(Traverse_assignments* tassign)
{
  if (this->init_ == NULL)
    return false;
  tassign->value(&this->init_, true, true);
  return true;
}

// Determine types.

void
Temporary_statement::do_determine_types()
{
  if (this->type_ != NULL && this->type_->is_abstract())
    this->type_ = this->type_->make_non_abstract_type();

  if (this->init_ != NULL)
    {
      if (this->type_ == NULL)
	this->init_->determine_type_no_context();
      else
	{
	  Type_context context(this->type_, false);
	  this->init_->determine_type(&context);
	}
    }

  if (this->type_ == NULL)
    {
      this->type_ = this->init_->type();
      gcc_assert(!this->type_->is_abstract());
    }
}

// Check types.

void
Temporary_statement::do_check_types(Gogo*)
{
  if (this->type_ != NULL && this->init_ != NULL)
    {
      std::string reason;
      if (!Type::are_assignable(this->type_, this->init_->type(), &reason))
	{
	  if (reason.empty())
	    error_at(this->location(), "incompatible types in assignment");
	  else
	    error_at(this->location(), "incompatible types in assignment (%s)",
		     reason.c_str());
	  this->set_is_error();
	}
    }
}

// Return a tree.

tree
Temporary_statement::do_get_tree(Translate_context* context)
{
  gcc_assert(this->decl_ == NULL_TREE);
  tree type_tree = this->type()->get_tree(context->gogo());
  tree init_tree = (this->init_ == NULL
		    ? NULL_TREE
		    : this->init_->get_tree(context));
  if (type_tree == error_mark_node || init_tree == error_mark_node)
    {
      this->decl_ = error_mark_node;
      return error_mark_node;
    }
  // We can only use create_tmp_var if the type is not addressable.
  if (!TREE_ADDRESSABLE(type_tree))
    {
      this->decl_ = create_tmp_var(type_tree, "GOTMP");
      DECL_SOURCE_LOCATION(this->decl_) = this->location();
    }
  else
    {
      gcc_assert(context->function() != NULL && context->block() != NULL);
      tree decl = build_decl(this->location(), VAR_DECL,
			     create_tmp_var_name("GOTMP"),
			     type_tree);
      DECL_ARTIFICIAL(decl) = 1;
      DECL_IGNORED_P(decl) = 1;
      TREE_USED(decl) = 1;
      gcc_assert(current_function_decl != NULL_TREE);
      DECL_CONTEXT(decl) = current_function_decl;

      // We have to add this variable to the block so that it winds up
      // in a BIND_EXPR.
      tree block_tree = context->block_tree();
      gcc_assert(block_tree != NULL_TREE);
      DECL_CHAIN(decl) = BLOCK_VARS(block_tree);
      BLOCK_VARS(block_tree) = decl;

      this->decl_ = decl;
    }
  if (init_tree != NULL_TREE)
    DECL_INITIAL(this->decl_) =
      Expression::convert_for_assignment(context, this->type(),
					 this->init_->type(), init_tree,
					 this->location());
  if (this->is_address_taken_)
    TREE_ADDRESSABLE(this->decl_) = 1;
  return this->build_stmt_1(DECL_EXPR, this->decl_);
}

// Make and initialize a temporary variable in BLOCK.

Temporary_statement*
Statement::make_temporary(Type* type, Expression* init,
			  source_location location)
{
  return new Temporary_statement(type, init, location);
}

// An assignment statement.

class Assignment_statement : public Statement
{
 public:
  Assignment_statement(Expression* lhs, Expression* rhs,
		       source_location location)
    : Statement(STATEMENT_ASSIGNMENT, location),
      lhs_(lhs), rhs_(rhs)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*);

  void
  do_determine_types();

  void
  do_check_types(Gogo*);

  tree
  do_get_tree(Translate_context*);

 private:
  // Left hand side--the lvalue.
  Expression* lhs_;
  // Right hand side--the rvalue.
  Expression* rhs_;
};

// Traversal.

int
Assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->lhs_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->rhs_);
}

bool
Assignment_statement::do_traverse_assignments(Traverse_assignments* tassign)
{
  tassign->assignment(&this->lhs_, &this->rhs_);
  return true;
}

// Set types for the assignment.

void
Assignment_statement::do_determine_types()
{
  this->lhs_->determine_type_no_context();
  Type_context context(this->lhs_->type(), false);
  this->rhs_->determine_type(&context);
}

// Check types for an assignment.

void
Assignment_statement::do_check_types(Gogo*)
{
  // The left hand side must be either addressable, a map index
  // expression, or the blank identifier.
  if (!this->lhs_->is_addressable()
      && this->lhs_->map_index_expression() == NULL
      && !this->lhs_->is_sink_expression())
    {
      if (!this->lhs_->type()->is_error_type())
	this->report_error(_("invalid left hand side of assignment"));
      return;
    }

  Type* lhs_type = this->lhs_->type();
  Type* rhs_type = this->rhs_->type();
  std::string reason;
  if (!Type::are_assignable(lhs_type, rhs_type, &reason))
    {
      if (reason.empty())
	error_at(this->location(), "incompatible types in assignment");
      else
	error_at(this->location(), "incompatible types in assignment (%s)",
		 reason.c_str());
      this->set_is_error();
    }

  if (lhs_type->is_error_type()
      || rhs_type->is_error_type()
      || lhs_type->is_undefined()
      || rhs_type->is_undefined())
    {
      // Make sure we get the error for an undefined type.
      lhs_type->base();
      rhs_type->base();
      this->set_is_error();
    }
}

// Build a tree for an assignment statement.

tree
Assignment_statement::do_get_tree(Translate_context* context)
{
  tree rhs_tree = this->rhs_->get_tree(context);

  if (this->lhs_->is_sink_expression())
    return rhs_tree;

  tree lhs_tree = this->lhs_->get_tree(context);

  if (lhs_tree == error_mark_node || rhs_tree == error_mark_node)
    return error_mark_node;

  rhs_tree = Expression::convert_for_assignment(context, this->lhs_->type(),
						this->rhs_->type(), rhs_tree,
						this->location());
  if (rhs_tree == error_mark_node)
    return error_mark_node;

  return fold_build2_loc(this->location(), MODIFY_EXPR, void_type_node,
			 lhs_tree, rhs_tree);
}

// Make an assignment statement.

Statement*
Statement::make_assignment(Expression* lhs, Expression* rhs,
			   source_location location)
{
  return new Assignment_statement(lhs, rhs, location);
}

// The Move_ordered_evals class is used to find any subexpressions of
// an expression that have an evaluation order dependency.  It creates
// temporary variables to hold them.

class Move_ordered_evals : public Traverse
{
 public:
  Move_ordered_evals(Block* block)
    : Traverse(traverse_expressions),
      block_(block)
  { }

 protected:
  int
  expression(Expression**);

 private:
  // The block where new temporary variables should be added.
  Block* block_;
};

int
Move_ordered_evals::expression(Expression** pexpr)
{
  // We have to look at subexpressions first.
  if ((*pexpr)->traverse_subexpressions(this) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if ((*pexpr)->must_eval_in_order())
    {
      source_location loc = (*pexpr)->location();
      Temporary_statement* temp = Statement::make_temporary(NULL, *pexpr, loc);
      this->block_->add_statement(temp);
      *pexpr = Expression::make_temporary_reference(temp, loc);
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// An assignment operation statement.

class Assignment_operation_statement : public Statement
{
 public:
  Assignment_operation_statement(Operator op, Expression* lhs, Expression* rhs,
				 source_location location)
    : Statement(STATEMENT_ASSIGNMENT_OPERATION, location),
      op_(op), lhs_(lhs), rhs_(rhs)
  { }

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  // The operator (OPERATOR_PLUSEQ, etc.).
  Operator op_;
  // Left hand side.
  Expression* lhs_;
  // Right hand side.
  Expression* rhs_;
};

// Traversal.

int
Assignment_operation_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->lhs_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->rhs_);
}

// Lower an assignment operation statement to a regular assignment
// statement.

Statement*
Assignment_operation_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  // We have to evaluate the left hand side expression only once.  We
  // do this by moving out any expression with side effects.
  Block* b = new Block(enclosing, loc);
  Move_ordered_evals moe(b);
  this->lhs_->traverse_subexpressions(&moe);

  Expression* lval = this->lhs_->copy();

  Operator op;
  switch (this->op_)
    {
    case OPERATOR_PLUSEQ:
      op = OPERATOR_PLUS;
      break;
    case OPERATOR_MINUSEQ:
      op = OPERATOR_MINUS;
      break;
    case OPERATOR_OREQ:
      op = OPERATOR_OR;
      break;
    case OPERATOR_XOREQ:
      op = OPERATOR_XOR;
      break;
    case OPERATOR_MULTEQ:
      op = OPERATOR_MULT;
      break;
    case OPERATOR_DIVEQ:
      op = OPERATOR_DIV;
      break;
    case OPERATOR_MODEQ:
      op = OPERATOR_MOD;
      break;
    case OPERATOR_LSHIFTEQ:
      op = OPERATOR_LSHIFT;
      break;
    case OPERATOR_RSHIFTEQ:
      op = OPERATOR_RSHIFT;
      break;
    case OPERATOR_ANDEQ:
      op = OPERATOR_AND;
      break;
    case OPERATOR_BITCLEAREQ:
      op = OPERATOR_BITCLEAR;
      break;
    default:
      gcc_unreachable();
    }

  Expression* binop = Expression::make_binary(op, lval, this->rhs_, loc);
  Statement* s = Statement::make_assignment(this->lhs_, binop, loc);
  if (b->statements()->empty())
    {
      delete b;
      return s;
    }
  else
    {
      b->add_statement(s);
      return Statement::make_block_statement(b, loc);
    }
}

// Make an assignment operation statement.

Statement*
Statement::make_assignment_operation(Operator op, Expression* lhs,
				     Expression* rhs, source_location location)
{
  return new Assignment_operation_statement(op, lhs, rhs, location);
}

// A tuple assignment statement.  This differs from an assignment
// statement in that the right-hand-side expressions are evaluated in
// parallel.

class Tuple_assignment_statement : public Statement
{
 public:
  Tuple_assignment_statement(Expression_list* lhs, Expression_list* rhs,
			     source_location location)
    : Statement(STATEMENT_TUPLE_ASSIGNMENT, location),
      lhs_(lhs), rhs_(rhs)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  // Left hand side--a list of lvalues.
  Expression_list* lhs_;
  // Right hand side--a list of rvalues.
  Expression_list* rhs_;
};

// Traversal.

int
Tuple_assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression_list(traverse, this->lhs_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression_list(traverse, this->rhs_);
}

// Lower a tuple assignment.  We use temporary variables to split it
// up into a set of single assignments.

Statement*
Tuple_assignment_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  Block* b = new Block(enclosing, loc);
  
  // First move out any subexpressions on the left hand side.  The
  // right hand side will be evaluated in the required order anyhow.
  Move_ordered_evals moe(b);
  for (Expression_list::const_iterator plhs = this->lhs_->begin();
       plhs != this->lhs_->end();
       ++plhs)
    (*plhs)->traverse_subexpressions(&moe);

  std::vector<Temporary_statement*> temps;
  temps.reserve(this->lhs_->size());

  Expression_list::const_iterator prhs = this->rhs_->begin();
  for (Expression_list::const_iterator plhs = this->lhs_->begin();
       plhs != this->lhs_->end();
       ++plhs, ++prhs)
    {
      gcc_assert(prhs != this->rhs_->end());

      if ((*plhs)->is_error_expression()
	  || (*plhs)->type()->is_error_type()
	  || (*prhs)->is_error_expression()
	  || (*prhs)->type()->is_error_type())
	continue;

      if ((*plhs)->is_sink_expression())
	{
	  b->add_statement(Statement::make_statement(*prhs));
	  continue;
	}

      Temporary_statement* temp = Statement::make_temporary((*plhs)->type(),
							    *prhs, loc);
      b->add_statement(temp);
      temps.push_back(temp);

    }
  gcc_assert(prhs == this->rhs_->end());

  prhs = this->rhs_->begin();
  std::vector<Temporary_statement*>::const_iterator ptemp = temps.begin();
  for (Expression_list::const_iterator plhs = this->lhs_->begin();
       plhs != this->lhs_->end();
       ++plhs, ++prhs)
    {
      if ((*plhs)->is_error_expression()
	  || (*plhs)->type()->is_error_type()
	  || (*prhs)->is_error_expression()
	  || (*prhs)->type()->is_error_type())
	continue;

      if ((*plhs)->is_sink_expression())
	continue;

      Expression* ref = Expression::make_temporary_reference(*ptemp, loc);
      Statement* s = Statement::make_assignment(*plhs, ref, loc);
      b->add_statement(s);
      ++ptemp;
    }
  gcc_assert(ptemp == temps.end());

  return Statement::make_block_statement(b, loc);
}

// Make a tuple assignment statement.

Statement*
Statement::make_tuple_assignment(Expression_list* lhs, Expression_list* rhs,
				 source_location location)
{
  return new Tuple_assignment_statement(lhs, rhs, location);
}

// A tuple assignment from a map index expression.
//   v, ok = m[k]

class Tuple_map_assignment_statement : public Statement
{
public:
  Tuple_map_assignment_statement(Expression* val, Expression* present,
				 Expression* map_index,
				 source_location location)
    : Statement(STATEMENT_TUPLE_MAP_ASSIGNMENT, location),
      val_(val), present_(present), map_index_(map_index)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  // Lvalue which receives the value from the map.
  Expression* val_;
  // Lvalue which receives whether the key value was present.
  Expression* present_;
  // The map index expression.
  Expression* map_index_;
};

// Traversal.

int
Tuple_map_assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT
      || this->traverse_expression(traverse, &this->present_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->map_index_);
}

// Lower a tuple map assignment.

Statement*
Tuple_map_assignment_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  Map_index_expression* map_index = this->map_index_->map_index_expression();
  if (map_index == NULL)
    {
      this->report_error(_("expected map index on right hand side"));
      return Statement::make_error_statement(loc);
    }
  Map_type* map_type = map_index->get_map_type();
  if (map_type == NULL)
    return Statement::make_error_statement(loc);

  Block* b = new Block(enclosing, loc);

  // Move out any subexpressions to make sure that functions are
  // called in the required order.
  Move_ordered_evals moe(b);
  this->val_->traverse_subexpressions(&moe);
  this->present_->traverse_subexpressions(&moe);

  // Copy the key value into a temporary so that we can take its
  // address without pushing the value onto the heap.

  // var key_temp KEY_TYPE = MAP_INDEX
  Temporary_statement* key_temp =
    Statement::make_temporary(map_type->key_type(), map_index->index(), loc);
  b->add_statement(key_temp);

  // var val_temp VAL_TYPE
  Temporary_statement* val_temp =
    Statement::make_temporary(map_type->val_type(), NULL, loc);
  b->add_statement(val_temp);

  // var present_temp bool
  Temporary_statement* present_temp =
    Statement::make_temporary(Type::lookup_bool_type(), NULL, loc);
  b->add_statement(present_temp);

  // func mapaccess2(hmap map[k]v, key *k, val *v) bool
  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("hmap", map_type, bloc));
  Type* pkey_type = Type::make_pointer_type(map_type->key_type());
  param_types->push_back(Typed_identifier("key", pkey_type, bloc));
  Type* pval_type = Type::make_pointer_type(map_type->val_type());
  param_types->push_back(Typed_identifier("val", pval_type, bloc));

  Typed_identifier_list* ret_types = new Typed_identifier_list();
  ret_types->push_back(Typed_identifier("", Type::lookup_bool_type(), bloc));

  Function_type* fntype = Type::make_function_type(NULL, param_types,
						   ret_types, bloc);
  Named_object* mapaccess2 =
    Named_object::make_function_declaration("mapaccess2", NULL, fntype, bloc);
  mapaccess2->func_declaration_value()->set_asm_name("runtime.mapaccess2");

  // present_temp = mapaccess2(MAP, &key_temp, &val_temp)
  Expression* func = Expression::make_func_reference(mapaccess2, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(map_index->map());
  Expression* ref = Expression::make_temporary_reference(key_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  ref = Expression::make_temporary_reference(val_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  Expression* call = Expression::make_call(func, params, false, loc);

  ref = Expression::make_temporary_reference(present_temp, loc);
  Statement* s = Statement::make_assignment(ref, call, loc);
  b->add_statement(s);

  // val = val_temp
  ref = Expression::make_temporary_reference(val_temp, loc);
  s = Statement::make_assignment(this->val_, ref, loc);
  b->add_statement(s);

  // present = present_temp
  ref = Expression::make_temporary_reference(present_temp, loc);
  s = Statement::make_assignment(this->present_, ref, loc);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Make a map assignment statement which returns a pair of values.

Statement*
Statement::make_tuple_map_assignment(Expression* val, Expression* present,
				     Expression* map_index,
				     source_location location)
{
  return new Tuple_map_assignment_statement(val, present, map_index, location);
}

// Assign a pair of entries to a map.
//   m[k] = v, p

class Map_assignment_statement : public Statement
{
 public:
  Map_assignment_statement(Expression* map_index,
			   Expression* val, Expression* should_set,
			   source_location location)
    : Statement(STATEMENT_MAP_ASSIGNMENT, location),
      map_index_(map_index), val_(val), should_set_(should_set)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  // A reference to the map index which should be set or deleted.
  Expression* map_index_;
  // The value to add to the map.
  Expression* val_;
  // Whether or not to add the value.
  Expression* should_set_;
};

// Traverse a map assignment.

int
Map_assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->map_index_) == TRAVERSE_EXIT
      || this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->should_set_);
}

// Lower a map assignment to a function call.

Statement*
Map_assignment_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  Map_index_expression* map_index = this->map_index_->map_index_expression();
  if (map_index == NULL)
    {
      this->report_error(_("expected map index on left hand side"));
      return Statement::make_error_statement(loc);
    }
  Map_type* map_type = map_index->get_map_type();
  if (map_type == NULL)
    return Statement::make_error_statement(loc);

  Block* b = new Block(enclosing, loc);

  // Evaluate the map first to get order of evaluation right.
  // map_temp := m // we are evaluating m[k] = v, p
  Temporary_statement* map_temp = Statement::make_temporary(map_type,
							    map_index->map(),
							    loc);
  b->add_statement(map_temp);

  // var key_temp MAP_KEY_TYPE = k
  Temporary_statement* key_temp =
    Statement::make_temporary(map_type->key_type(), map_index->index(), loc);
  b->add_statement(key_temp);

  // var val_temp MAP_VAL_TYPE = v
  Temporary_statement* val_temp =
    Statement::make_temporary(map_type->val_type(), this->val_, loc);
  b->add_statement(val_temp);

  // func mapassign2(hmap map[k]v, key *k, val *v, p)
  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("hmap", map_type, bloc));
  Type* pkey_type = Type::make_pointer_type(map_type->key_type());
  param_types->push_back(Typed_identifier("key", pkey_type, bloc));
  Type* pval_type = Type::make_pointer_type(map_type->val_type());
  param_types->push_back(Typed_identifier("val", pval_type, bloc));
  param_types->push_back(Typed_identifier("p", Type::lookup_bool_type(), bloc));
  Function_type* fntype = Type::make_function_type(NULL, param_types,
						   NULL, bloc);
  Named_object* mapassign2 =
    Named_object::make_function_declaration("mapassign2", NULL, fntype, bloc);
  mapassign2->func_declaration_value()->set_asm_name("runtime.mapassign2");

  // mapassign2(map_temp, &key_temp, &val_temp, p)
  Expression* func = Expression::make_func_reference(mapassign2, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(Expression::make_temporary_reference(map_temp, loc));
  Expression* ref = Expression::make_temporary_reference(key_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  ref = Expression::make_temporary_reference(val_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  params->push_back(this->should_set_);
  Expression* call = Expression::make_call(func, params, false, loc);
  Statement* s = Statement::make_statement(call);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Make a statement which assigns a pair of entries to a map.

Statement*
Statement::make_map_assignment(Expression* map_index,
			       Expression* val, Expression* should_set,
			       source_location location)
{
  return new Map_assignment_statement(map_index, val, should_set, location);
}

// A tuple assignment from a receive statement.

class Tuple_receive_assignment_statement : public Statement
{
 public:
  Tuple_receive_assignment_statement(Expression* val, Expression* success,
				     Expression* channel,
				     source_location location)
    : Statement(STATEMENT_TUPLE_RECEIVE_ASSIGNMENT, location),
      val_(val), success_(success), channel_(channel)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  // Lvalue which receives the value from the channel.
  Expression* val_;
  // Lvalue which receives whether the read succeeded or failed.
  Expression* success_;
  // The channel on which we receive the value.
  Expression* channel_;
};

// Traversal.

int
Tuple_receive_assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT
      || this->traverse_expression(traverse, &this->success_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->channel_);
}

// Lower to a function call.

Statement*
Tuple_receive_assignment_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  Channel_type* channel_type = this->channel_->type()->channel_type();
  if (channel_type == NULL)
    {
      this->report_error(_("expected channel"));
      return Statement::make_error_statement(loc);
    }
  if (!channel_type->may_receive())
    {
      this->report_error(_("invalid receive on send-only channel"));
      return Statement::make_error_statement(loc);
    }

  Block* b = new Block(enclosing, loc);

  // Make sure that any subexpressions on the left hand side are
  // evaluated in the right order.
  Move_ordered_evals moe(b);
  this->val_->traverse_subexpressions(&moe);
  this->success_->traverse_subexpressions(&moe);

  // var val_temp ELEMENT_TYPE
  Temporary_statement* val_temp =
    Statement::make_temporary(channel_type->element_type(), NULL, loc);
  b->add_statement(val_temp);

  // var success_temp bool
  Temporary_statement* success_temp =
    Statement::make_temporary(Type::lookup_bool_type(), NULL, loc);
  b->add_statement(success_temp);

  // func chanrecv2(c chan T, val *T) bool
  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("c", channel_type, bloc));
  Type* pelement_type = Type::make_pointer_type(channel_type->element_type());
  param_types->push_back(Typed_identifier("val", pelement_type, bloc));

  Typed_identifier_list* ret_types = new Typed_identifier_list();
  ret_types->push_back(Typed_identifier("", Type::lookup_bool_type(), bloc));

  Function_type* fntype = Type::make_function_type(NULL, param_types,
						   ret_types, bloc);
  Named_object* chanrecv2 =
    Named_object::make_function_declaration("chanrecv2", NULL, fntype, bloc);
  chanrecv2->func_declaration_value()->set_asm_name("runtime.chanrecv2");

  // success_temp = chanrecv2(channel, &val_temp)
  Expression* func = Expression::make_func_reference(chanrecv2, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(this->channel_);
  Expression* ref = Expression::make_temporary_reference(val_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  Expression* call = Expression::make_call(func, params, false, loc);
  ref = Expression::make_temporary_reference(success_temp, loc);
  Statement* s = Statement::make_assignment(ref, call, loc);
  b->add_statement(s);

  // val = val_temp
  ref = Expression::make_temporary_reference(val_temp, loc);
  s = Statement::make_assignment(this->val_, ref, loc);
  b->add_statement(s);

  // success = success_temp
  ref = Expression::make_temporary_reference(success_temp, loc);
  s = Statement::make_assignment(this->success_, ref, loc);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Make a nonblocking receive statement.

Statement*
Statement::make_tuple_receive_assignment(Expression* val, Expression* success,
					 Expression* channel,
					 source_location location)
{
  return new Tuple_receive_assignment_statement(val, success, channel,
						location);
}

// An assignment to a pair of values from a type guard.  This is a
// conditional type guard.  v, ok = i.(type).

class Tuple_type_guard_assignment_statement : public Statement
{
 public:
  Tuple_type_guard_assignment_statement(Expression* val, Expression* ok,
					Expression* expr, Type* type,
					source_location location)
    : Statement(STATEMENT_TUPLE_TYPE_GUARD_ASSIGNMENT, location),
      val_(val), ok_(ok), expr_(expr), type_(type)
  { }

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  Call_expression*
  lower_to_empty_interface(const char*);

  Call_expression*
  lower_to_type(const char*);

  void
  lower_to_object_type(Block*, const char*);

  // The variable which recieves the converted value.
  Expression* val_;
  // The variable which receives the indication of success.
  Expression* ok_;
  // The expression being converted.
  Expression* expr_;
  // The type to which the expression is being converted.
  Type* type_;
};

// Traverse a type guard tuple assignment.

int
Tuple_type_guard_assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT
      || this->traverse_expression(traverse, &this->ok_) == TRAVERSE_EXIT
      || this->traverse_type(traverse, this->type_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->expr_);
}

// Lower to a function call.

Statement*
Tuple_type_guard_assignment_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  Type* expr_type = this->expr_->type();
  if (expr_type->interface_type() == NULL)
    {
      if (!expr_type->is_error_type() && !this->type_->is_error_type())
	this->report_error(_("type assertion only valid for interface types"));
      return Statement::make_error_statement(loc);
    }

  Block* b = new Block(enclosing, loc);

  // Make sure that any subexpressions on the left hand side are
  // evaluated in the right order.
  Move_ordered_evals moe(b);
  this->val_->traverse_subexpressions(&moe);
  this->ok_->traverse_subexpressions(&moe);

  bool expr_is_empty = expr_type->interface_type()->is_empty();
  Call_expression* call;
  if (this->type_->interface_type() != NULL)
    {
      if (this->type_->interface_type()->is_empty())
	call = this->lower_to_empty_interface(expr_is_empty
					      ? "ifaceE2E2"
					      : "ifaceI2E2");
      else
	call = this->lower_to_type(expr_is_empty ? "ifaceE2I2" : "ifaceI2I2");
    }
  else if (this->type_->points_to() != NULL)
    call = this->lower_to_type(expr_is_empty ? "ifaceE2T2P" : "ifaceI2T2P");
  else
    {
      this->lower_to_object_type(b, expr_is_empty ? "ifaceE2T2" : "ifaceI2T2");
      call = NULL;
    }

  if (call != NULL)
    {
      Expression* res = Expression::make_call_result(call, 0);
      Statement* s = Statement::make_assignment(this->val_, res, loc);
      b->add_statement(s);

      res = Expression::make_call_result(call, 1);
      s = Statement::make_assignment(this->ok_, res, loc);
      b->add_statement(s);
    }

  return Statement::make_block_statement(b, loc);
}

// Lower a conversion to an empty interface type.

Call_expression*
Tuple_type_guard_assignment_statement::lower_to_empty_interface(
    const char *fnname)
{
  source_location loc = this->location();

  // func FNNAME(interface) (empty, bool)
  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("i", this->expr_->type(), bloc));
  Typed_identifier_list* ret_types = new Typed_identifier_list();
  ret_types->push_back(Typed_identifier("ret", this->type_, bloc));
  ret_types->push_back(Typed_identifier("ok", Type::lookup_bool_type(), bloc));
  Function_type* fntype = Type::make_function_type(NULL, param_types,
						   ret_types, bloc);
  Named_object* fn =
    Named_object::make_function_declaration(fnname, NULL, fntype, bloc);
  std::string asm_name = "runtime.";
  asm_name += fnname;
  fn->func_declaration_value()->set_asm_name(asm_name);

  // val, ok = FNNAME(expr)
  Expression* func = Expression::make_func_reference(fn, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(this->expr_);
  return Expression::make_call(func, params, false, loc);
}

// Lower a conversion to a non-empty interface type or a pointer type.

Call_expression*
Tuple_type_guard_assignment_statement::lower_to_type(const char* fnname)
{
  source_location loc = this->location();

  // func FNNAME(*descriptor, interface) (interface, bool)
  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("inter",
					  Type::make_type_descriptor_ptr_type(),
					  bloc));
  param_types->push_back(Typed_identifier("i", this->expr_->type(), bloc));
  Typed_identifier_list* ret_types = new Typed_identifier_list();
  ret_types->push_back(Typed_identifier("ret", this->type_, bloc));
  ret_types->push_back(Typed_identifier("ok", Type::lookup_bool_type(), bloc));
  Function_type* fntype = Type::make_function_type(NULL, param_types,
						   ret_types, bloc);
  Named_object* fn =
    Named_object::make_function_declaration(fnname, NULL, fntype, bloc);
  std::string asm_name = "runtime.";
  asm_name += fnname;
  fn->func_declaration_value()->set_asm_name(asm_name);

  // val, ok = FNNAME(type_descriptor, expr)
  Expression* func = Expression::make_func_reference(fn, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(Expression::make_type_descriptor(this->type_, loc));
  params->push_back(this->expr_);
  return Expression::make_call(func, params, false, loc);
}

// Lower a conversion to a non-interface non-pointer type.

void
Tuple_type_guard_assignment_statement::lower_to_object_type(Block* b,
							    const char *fnname)
{
  source_location loc = this->location();

  // var val_temp TYPE
  Temporary_statement* val_temp = Statement::make_temporary(this->type_,
							    NULL, loc);
  b->add_statement(val_temp);

  // func FNNAME(*descriptor, interface, *T) bool
  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("inter",
					  Type::make_type_descriptor_ptr_type(),
					  bloc));
  param_types->push_back(Typed_identifier("i", this->expr_->type(), bloc));
  Type* ptype = Type::make_pointer_type(this->type_);
  param_types->push_back(Typed_identifier("v", ptype, bloc));
  Typed_identifier_list* ret_types = new Typed_identifier_list();
  ret_types->push_back(Typed_identifier("ok", Type::lookup_bool_type(), bloc));
  Function_type* fntype = Type::make_function_type(NULL, param_types,
						   ret_types, bloc);
  Named_object* fn =
    Named_object::make_function_declaration(fnname, NULL, fntype, bloc);
  std::string asm_name = "runtime.";
  asm_name += fnname;
  fn->func_declaration_value()->set_asm_name(asm_name);

  // ok = FNNAME(type_descriptor, expr, &val_temp)
  Expression* func = Expression::make_func_reference(fn, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(Expression::make_type_descriptor(this->type_, loc));
  params->push_back(this->expr_);
  Expression* ref = Expression::make_temporary_reference(val_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  Expression* call = Expression::make_call(func, params, false, loc);
  Statement* s = Statement::make_assignment(this->ok_, call, loc);
  b->add_statement(s);

  // val = val_temp
  ref = Expression::make_temporary_reference(val_temp, loc);
  s = Statement::make_assignment(this->val_, ref, loc);
  b->add_statement(s);
}

// Make an assignment from a type guard to a pair of variables.

Statement*
Statement::make_tuple_type_guard_assignment(Expression* val, Expression* ok,
					    Expression* expr, Type* type,
					    source_location location)
{
  return new Tuple_type_guard_assignment_statement(val, ok, expr, type,
						   location);
}

// An expression statement.

class Expression_statement : public Statement
{
 public:
  Expression_statement(Expression* expr)
    : Statement(STATEMENT_EXPRESSION, expr->location()),
      expr_(expr)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->traverse_expression(traverse, &this->expr_); }

  void
  do_determine_types()
  { this->expr_->determine_type_no_context(); }

  bool
  do_may_fall_through() const;

  tree
  do_get_tree(Translate_context* context)
  { return this->expr_->get_tree(context); }

 private:
  Expression* expr_;
};

// An expression statement may fall through unless it is a call to a
// function which does not return.

bool
Expression_statement::do_may_fall_through() const
{
  const Call_expression* call = this->expr_->call_expression();
  if (call != NULL)
    {
      const Expression* fn = call->fn();
      const Func_expression* fe = fn->func_expression();
      if (fe != NULL)
	{
	  const Named_object* no = fe->named_object();

	  Function_type* fntype;
	  if (no->is_function())
	    fntype = no->func_value()->type();
	  else if (no->is_function_declaration())
	    fntype = no->func_declaration_value()->type();
	  else
	    fntype = NULL;

	  // The builtin function panic does not return.
	  if (fntype != NULL && fntype->is_builtin() && no->name() == "panic")
	    return false;
	}
    }
  return true;
}

// Make an expression statement from an Expression.

Statement*
Statement::make_statement(Expression* expr)
{
  return new Expression_statement(expr);
}

// A block statement--a list of statements which may include variable
// definitions.

class Block_statement : public Statement
{
 public:
  Block_statement(Block* block, source_location location)
    : Statement(STATEMENT_BLOCK, location),
      block_(block)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->block_->traverse(traverse); }

  void
  do_determine_types()
  { this->block_->determine_types(); }

  bool
  do_may_fall_through() const
  { return this->block_->may_fall_through(); }

  tree
  do_get_tree(Translate_context* context)
  { return this->block_->get_tree(context); }

 private:
  Block* block_;
};

// Make a block statement.

Statement*
Statement::make_block_statement(Block* block, source_location location)
{
  return new Block_statement(block, location);
}

// An increment or decrement statement.

class Inc_dec_statement : public Statement
{
 public:
  Inc_dec_statement(bool is_inc, Expression* expr)
    : Statement(STATEMENT_INCDEC, expr->location()),
      expr_(expr), is_inc_(is_inc)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return this->traverse_expression(traverse, &this->expr_); }

  bool
  do_traverse_assignments(Traverse_assignments*)
  { gcc_unreachable(); }

  Statement*
  do_lower(Gogo*, Block*);

  tree
  do_get_tree(Translate_context*)
  { gcc_unreachable(); }

 private:
  // The l-value to increment or decrement.
  Expression* expr_;
  // Whether to increment or decrement.
  bool is_inc_;
};

// Lower to += or -=.

Statement*
Inc_dec_statement::do_lower(Gogo*, Block*)
{
  source_location loc = this->location();

  mpz_t oval;
  mpz_init_set_ui(oval, 1UL);
  Expression* oexpr = Expression::make_integer(&oval, NULL, loc);
  mpz_clear(oval);

  Operator op = this->is_inc_ ? OPERATOR_PLUSEQ : OPERATOR_MINUSEQ;
  return Statement::make_assignment_operation(op, this->expr_, oexpr, loc);
}

// Make an increment statement.

Statement*
Statement::make_inc_statement(Expression* expr)
{
  return new Inc_dec_statement(true, expr);
}

// Make a decrement statement.

Statement*
Statement::make_dec_statement(Expression* expr)
{
  return new Inc_dec_statement(false, expr);
}

// Class Thunk_statement.  This is the base class for go and defer
// statements.

const char* const Thunk_statement::thunk_field_fn = "fn";

const char* const Thunk_statement::thunk_field_receiver = "receiver";

// Constructor.

Thunk_statement::Thunk_statement(Statement_classification classification,
				 Call_expression* call,
				 source_location location)
    : Statement(classification, location),
      call_(call), struct_type_(NULL)
{
}

// Return whether this is a simple statement which does not require a
// thunk.

bool
Thunk_statement::is_simple(Function_type* fntype) const
{
  // We need a thunk to call a method, or to pass a variable number of
  // arguments.
  if (fntype->is_method() || fntype->is_varargs())
    return false;

  // A defer statement requires a thunk to set up for whether the
  // function can call recover.
  if (this->classification() == STATEMENT_DEFER)
    return false;

  // We can only permit a single parameter of pointer type.
  const Typed_identifier_list* parameters = fntype->parameters();
  if (parameters != NULL
      && (parameters->size() > 1
	  || (parameters->size() == 1
	      && parameters->begin()->type()->points_to() == NULL)))
    return false;

  // If the function returns multiple values, or returns a type other
  // than integer, floating point, or pointer, then it may get a
  // hidden first parameter, in which case we need the more
  // complicated approach.  This is true even though we are going to
  // ignore the return value.
  const Typed_identifier_list* results = fntype->results();
  if (results != NULL
      && (results->size() > 1
	  || (results->size() == 1
	      && !results->begin()->type()->is_basic_type()
	      && results->begin()->type()->points_to() == NULL)))
    return false;

  // If this calls something which is not a simple function, then we
  // need a thunk.
  Expression* fn = this->call_->call_expression()->fn();
  if (fn->bound_method_expression() != NULL
      || fn->interface_field_reference_expression() != NULL)
    return false;

  return true;
}

// Traverse a thunk statement.

int
Thunk_statement::do_traverse(Traverse* traverse)
{
  return this->traverse_expression(traverse, &this->call_);
}

// We implement traverse_assignment for a thunk statement because it
// effectively copies the function call.

bool
Thunk_statement::do_traverse_assignments(Traverse_assignments* tassign)
{
  Expression* fn = this->call_->call_expression()->fn();
  Expression* fn2 = fn;
  tassign->value(&fn2, true, false);
  return true;
}

// Determine types in a thunk statement.

void
Thunk_statement::do_determine_types()
{
  this->call_->determine_type_no_context();

  // Now that we know the types of the call, build the struct used to
  // pass parameters.
  Call_expression* ce = this->call_->call_expression();
  if (ce == NULL)
    return;
  Function_type* fntype = ce->get_function_type();
  if (fntype != NULL && !this->is_simple(fntype))
    this->struct_type_ = this->build_struct(fntype);
}

// Check types in a thunk statement.

void
Thunk_statement::do_check_types(Gogo*)
{
  Call_expression* ce = this->call_->call_expression();
  if (ce == NULL)
    {
      if (!this->call_->is_error_expression())
	this->report_error("expected call expression");
      return;
    }
  Function_type* fntype = ce->get_function_type();
  if (fntype != NULL && fntype->is_method())
    {
      Expression* fn = ce->fn();
      if (fn->bound_method_expression() == NULL
	  && fn->interface_field_reference_expression() == NULL)
	this->report_error(_("no object for method call"));
    }
}

// The Traverse class used to find and simplify thunk statements.

class Simplify_thunk_traverse : public Traverse
{
 public:
  Simplify_thunk_traverse(Gogo* gogo)
    : Traverse(traverse_blocks),
      gogo_(gogo)
  { }

  int
  block(Block*);

 private:
  Gogo* gogo_;
};

int
Simplify_thunk_traverse::block(Block* b)
{
  // The parser ensures that thunk statements always appear at the end
  // of a block.
  if (b->statements()->size() < 1)
    return TRAVERSE_CONTINUE;
  Thunk_statement* stat = b->statements()->back()->thunk_statement();
  if (stat == NULL)
    return TRAVERSE_CONTINUE;
  if (stat->simplify_statement(this->gogo_, b))
    return TRAVERSE_SKIP_COMPONENTS;
  return TRAVERSE_CONTINUE;
}

// Simplify all thunk statements.

void
Gogo::simplify_thunk_statements()
{
  Simplify_thunk_traverse thunk_traverse(this);
  this->traverse(&thunk_traverse);
}

// Simplify complex thunk statements into simple ones.  A complicated
// thunk statement is one which takes anything other than zero
// parameters or a single pointer parameter.  We rewrite it into code
// which allocates a struct, stores the parameter values into the
// struct, and does a simple go or defer statement which passes the
// struct to a thunk.  The thunk does the real call.

bool
Thunk_statement::simplify_statement(Gogo* gogo, Block* block)
{
  if (this->classification() == STATEMENT_ERROR)
    return false;
  if (this->call_->is_error_expression())
    return false;

  Call_expression* ce = this->call_->call_expression();
  Function_type* fntype = ce->get_function_type();
  if (fntype == NULL)
    {
      gcc_assert(saw_errors());
      this->set_is_error();
      return false;
    }
  if (this->is_simple(fntype))
    return false;

  Expression* fn = ce->fn();
  Bound_method_expression* bound_method = fn->bound_method_expression();
  Interface_field_reference_expression* interface_method =
    fn->interface_field_reference_expression();
  const bool is_method = bound_method != NULL || interface_method != NULL;

  source_location location = this->location();

  std::string thunk_name = Gogo::thunk_name();

  // Build the thunk.
  this->build_thunk(gogo, thunk_name, fntype);

  // Generate code to call the thunk.

  // Get the values to store into the struct which is the single
  // argument to the thunk.

  Expression_list* vals = new Expression_list();
  if (fntype->is_builtin())
    ;
  else if (!is_method)
    vals->push_back(fn);
  else if (interface_method != NULL)
    vals->push_back(interface_method->expr());
  else if (bound_method != NULL)
    {
      vals->push_back(bound_method->method());
      Expression* first_arg = bound_method->first_argument();

      // We always pass a pointer when calling a method.
      if (first_arg->type()->points_to() == NULL)
	first_arg = Expression::make_unary(OPERATOR_AND, first_arg, location);

      // If we are calling a method which was inherited from an
      // embedded struct, and the method did not get a stub, then the
      // first type may be wrong.
      Type* fatype = bound_method->first_argument_type();
      if (fatype != NULL)
	{
	  if (fatype->points_to() == NULL)
	    fatype = Type::make_pointer_type(fatype);
	  Type* unsafe = Type::make_pointer_type(Type::make_void_type());
	  first_arg = Expression::make_cast(unsafe, first_arg, location);
	  first_arg = Expression::make_cast(fatype, first_arg, location);
	}

      vals->push_back(first_arg);
    }
  else
    gcc_unreachable();

  if (ce->args() != NULL)
    {
      for (Expression_list::const_iterator p = ce->args()->begin();
	   p != ce->args()->end();
	   ++p)
	vals->push_back(*p);
    }

  // Build the struct.
  Expression* constructor =
    Expression::make_struct_composite_literal(this->struct_type_, vals,
					      location);

  // Allocate the initialized struct on the heap.
  constructor = Expression::make_heap_composite(constructor, location);

  // Look up the thunk.
  Named_object* named_thunk = gogo->lookup(thunk_name, NULL);
  gcc_assert(named_thunk != NULL && named_thunk->is_function());

  // Build the call.
  Expression* func = Expression::make_func_reference(named_thunk, NULL,
						     location);
  Expression_list* params = new Expression_list();
  params->push_back(constructor);
  Call_expression* call = Expression::make_call(func, params, false, location);

  // Build the simple go or defer statement.
  Statement* s;
  if (this->classification() == STATEMENT_GO)
    s = Statement::make_go_statement(call, location);
  else if (this->classification() == STATEMENT_DEFER)
    s = Statement::make_defer_statement(call, location);
  else
    gcc_unreachable();

  // The current block should end with the go statement.
  gcc_assert(block->statements()->size() >= 1);
  gcc_assert(block->statements()->back() == this);
  block->replace_statement(block->statements()->size() - 1, s);

  // We already ran the determine_types pass, so we need to run it now
  // for the new statement.
  s->determine_types();

  // Sanity check.
  gogo->check_types_in_block(block);

  // Return true to tell the block not to keep looking at statements.
  return true;
}

// Set the name to use for thunk parameter N.

void
Thunk_statement::thunk_field_param(int n, char* buf, size_t buflen)
{
  snprintf(buf, buflen, "a%d", n);
}

// Build a new struct type to hold the parameters for a complicated
// thunk statement.  FNTYPE is the type of the function call.

Struct_type*
Thunk_statement::build_struct(Function_type* fntype)
{
  source_location location = this->location();

  Struct_field_list* fields = new Struct_field_list();

  Call_expression* ce = this->call_->call_expression();
  Expression* fn = ce->fn();

  Interface_field_reference_expression* interface_method =
    fn->interface_field_reference_expression();
  if (interface_method != NULL)
    {
      // If this thunk statement calls a method on an interface, we
      // pass the interface object to the thunk.
      Typed_identifier tid(Thunk_statement::thunk_field_fn,
			   interface_method->expr()->type(),
			   location);
      fields->push_back(Struct_field(tid));
    }
  else if (!fntype->is_builtin())
    {
      // The function to call.
      Typed_identifier tid(Go_statement::thunk_field_fn, fntype, location);
      fields->push_back(Struct_field(tid));
    }
  else if (ce->is_recover_call())
    {
      // The predeclared recover function has no argument.  However,
      // we add an argument when building recover thunks.  Handle that
      // here.
      fields->push_back(Struct_field(Typed_identifier("can_recover",
						      Type::lookup_bool_type(),
						      location)));
    }

  if (fn->bound_method_expression() != NULL)
    {
      gcc_assert(fntype->is_method());
      Type* rtype = fntype->receiver()->type();
      // We always pass the receiver as a pointer.
      if (rtype->points_to() == NULL)
	rtype = Type::make_pointer_type(rtype);
      Typed_identifier tid(Thunk_statement::thunk_field_receiver, rtype,
			   location);
      fields->push_back(Struct_field(tid));
    }

  const Expression_list* args = ce->args();
  if (args != NULL)
    {
      int i = 0;
      for (Expression_list::const_iterator p = args->begin();
	   p != args->end();
	   ++p, ++i)
	{
	  char buf[50];
	  this->thunk_field_param(i, buf, sizeof buf);
	  fields->push_back(Struct_field(Typed_identifier(buf, (*p)->type(),
							  location)));
	}
    }

  return Type::make_struct_type(fields, location);
}

// Build the thunk we are going to call.  This is a brand new, albeit
// artificial, function.

void
Thunk_statement::build_thunk(Gogo* gogo, const std::string& thunk_name,
			     Function_type* fntype)
{
  source_location location = this->location();

  Call_expression* ce = this->call_->call_expression();

  bool may_call_recover = false;
  if (this->classification() == STATEMENT_DEFER)
    {
      Func_expression* fn = ce->fn()->func_expression();
      if (fn == NULL)
	may_call_recover = true;
      else
	{
	  const Named_object* no = fn->named_object();
	  if (!no->is_function())
	    may_call_recover = true;
	  else
	    may_call_recover = no->func_value()->calls_recover();
	}
    }

  // Build the type of the thunk.  The thunk takes a single parameter,
  // which is a pointer to the special structure we build.
  const char* const parameter_name = "__go_thunk_parameter";
  Typed_identifier_list* thunk_parameters = new Typed_identifier_list();
  Type* pointer_to_struct_type = Type::make_pointer_type(this->struct_type_);
  thunk_parameters->push_back(Typed_identifier(parameter_name,
					       pointer_to_struct_type,
					       location));

  Typed_identifier_list* thunk_results = NULL;
  if (may_call_recover)
    {
      // When deferring a function which may call recover, add a
      // return value, to disable tail call optimizations which will
      // break the way we check whether recover is permitted.
      thunk_results = new Typed_identifier_list();
      thunk_results->push_back(Typed_identifier("", Type::lookup_bool_type(),
						location));
    }

  Function_type* thunk_type = Type::make_function_type(NULL, thunk_parameters,
						       thunk_results,
						       location);

  // Start building the thunk.
  Named_object* function = gogo->start_function(thunk_name, thunk_type, true,
						location);

  // For a defer statement, start with a call to
  // __go_set_defer_retaddr.  */
  Label* retaddr_label = NULL; 
  if (may_call_recover)
    {
      retaddr_label = gogo->add_label_reference("retaddr");
      Expression* arg = Expression::make_label_addr(retaddr_label, location);
      Expression_list* args = new Expression_list();
      args->push_back(arg);

      static Named_object* set_defer_retaddr;
      if (set_defer_retaddr == NULL)
	{
	  const source_location bloc = BUILTINS_LOCATION;
	  Typed_identifier_list* param_types = new Typed_identifier_list();
	  Type *voidptr_type = Type::make_pointer_type(Type::make_void_type());
	  param_types->push_back(Typed_identifier("r", voidptr_type, bloc));

	  Typed_identifier_list* result_types = new Typed_identifier_list();
	  result_types->push_back(Typed_identifier("",
						   Type::lookup_bool_type(),
						   bloc));

	  Function_type* t = Type::make_function_type(NULL, param_types,
						      result_types, bloc);
	  set_defer_retaddr =
	    Named_object::make_function_declaration("__go_set_defer_retaddr",
						    NULL, t, bloc);
	  const char* n = "__go_set_defer_retaddr";
	  set_defer_retaddr->func_declaration_value()->set_asm_name(n);
	}

      Expression* fn = Expression::make_func_reference(set_defer_retaddr,
						       NULL, location);
      Expression* call = Expression::make_call(fn, args, false, location);

      // This is a hack to prevent the middle-end from deleting the
      // label.
      gogo->start_block(location);
      gogo->add_statement(Statement::make_goto_statement(retaddr_label,
							 location));
      Block* then_block = gogo->finish_block(location);
      then_block->determine_types();

      Statement* s = Statement::make_if_statement(call, then_block, NULL,
						  location);
      s->determine_types();
      gogo->add_statement(s);
    }

  // Get a reference to the parameter.
  Named_object* named_parameter = gogo->lookup(parameter_name, NULL);
  gcc_assert(named_parameter != NULL && named_parameter->is_variable());

  // Build the call.  Note that the field names are the same as the
  // ones used in build_struct.
  Expression* thunk_parameter = Expression::make_var_reference(named_parameter,
							       location);
  thunk_parameter = Expression::make_unary(OPERATOR_MULT, thunk_parameter,
					   location);

  Bound_method_expression* bound_method = ce->fn()->bound_method_expression();
  Interface_field_reference_expression* interface_method =
    ce->fn()->interface_field_reference_expression();

  Expression* func_to_call;
  unsigned int next_index;
  if (!fntype->is_builtin())
    {
      func_to_call = Expression::make_field_reference(thunk_parameter,
						      0, location);
      next_index = 1;
    }
  else
    {
      gcc_assert(bound_method == NULL && interface_method == NULL);
      func_to_call = ce->fn();
      next_index = 0;
    }

  if (bound_method != NULL)
    {
      Expression* r = Expression::make_field_reference(thunk_parameter, 1,
						       location);
      // The main program passes in a function pointer from the
      // interface expression, so here we can make a bound method in
      // all cases.
      func_to_call = Expression::make_bound_method(r, func_to_call,
						   location);
      next_index = 2;
    }
  else if (interface_method != NULL)
    {
      // The main program passes the interface object.
      const std::string& name(interface_method->name());
      func_to_call = Expression::make_interface_field_reference(func_to_call,
								name,
								location);
    }

  Expression_list* call_params = new Expression_list();
  const Struct_field_list* fields = this->struct_type_->fields();
  Struct_field_list::const_iterator p = fields->begin();
  for (unsigned int i = 0; i < next_index; ++i)
    ++p;
  bool is_recover_call = ce->is_recover_call();
  Expression* recover_arg = NULL;
  for (; p != fields->end(); ++p, ++next_index)
    {
      Expression* thunk_param = Expression::make_var_reference(named_parameter,
							       location);
      thunk_param = Expression::make_unary(OPERATOR_MULT, thunk_param,
					   location);
      Expression* param = Expression::make_field_reference(thunk_param,
							   next_index,
							   location);
      if (!is_recover_call)
	call_params->push_back(param);
      else
	{
	  gcc_assert(call_params->empty());
	  recover_arg = param;
	}
    }

  if (call_params->empty())
    {
      delete call_params;
      call_params = NULL;
    }

  Expression* call = Expression::make_call(func_to_call, call_params, false,
					   location);
  // We need to lower in case this is a builtin function.
  call = call->lower(gogo, function, -1);
  Call_expression* call_ce = call->call_expression();
  if (call_ce != NULL && may_call_recover)
    call_ce->set_is_deferred();

  Statement* call_statement = Statement::make_statement(call);

  // We already ran the determine_types pass, so we need to run it
  // just for this statement now.
  call_statement->determine_types();

  // Sanity check.
  call->check_types(gogo);

  if (call_ce != NULL && recover_arg != NULL)
    call_ce->set_recover_arg(recover_arg);

  gogo->add_statement(call_statement);

  // If this is a defer statement, the label comes immediately after
  // the call.
  if (may_call_recover)
    {
      gogo->add_label_definition("retaddr", location);

      Expression_list* vals = new Expression_list();
      vals->push_back(Expression::make_boolean(false, location));
      const Typed_identifier_list* results =
	function->func_value()->type()->results();
      gogo->add_statement(Statement::make_return_statement(results, vals,
							  location));
    }

  // That is all the thunk has to do.
  gogo->finish_function(location);
}

// Get the function and argument trees.

void
Thunk_statement::get_fn_and_arg(Translate_context* context, tree* pfn,
				tree* parg)
{
  if (this->call_->is_error_expression())
    {
      *pfn = error_mark_node;
      *parg = error_mark_node;
      return;
    }

  Call_expression* ce = this->call_->call_expression();

  Expression* fn = ce->fn();
  *pfn = fn->get_tree(context);

  const Expression_list* args = ce->args();
  if (args == NULL || args->empty())
    *parg = null_pointer_node;
  else
    {
      gcc_assert(args->size() == 1);
      *parg = args->front()->get_tree(context);
    }
}

// Class Go_statement.

tree
Go_statement::do_get_tree(Translate_context* context)
{
  tree fn_tree;
  tree arg_tree;
  this->get_fn_and_arg(context, &fn_tree, &arg_tree);

  static tree go_fndecl;

  tree fn_arg_type = NULL_TREE;
  if (go_fndecl == NULL_TREE)
    {
      // Only build FN_ARG_TYPE if we need it.
      tree subargtypes = tree_cons(NULL_TREE, ptr_type_node, void_list_node);
      tree subfntype = build_function_type(ptr_type_node, subargtypes);
      fn_arg_type = build_pointer_type(subfntype);
    }

  return Gogo::call_builtin(&go_fndecl,
			    this->location(),
			    "__go_go",
			    2,
			    void_type_node,
			    fn_arg_type,
			    fn_tree,
			    ptr_type_node,
			    arg_tree);
}

// Make a go statement.

Statement*
Statement::make_go_statement(Call_expression* call, source_location location)
{
  return new Go_statement(call, location);
}

// Class Defer_statement.

tree
Defer_statement::do_get_tree(Translate_context* context)
{
  source_location loc = this->location();

  tree fn_tree;
  tree arg_tree;
  this->get_fn_and_arg(context, &fn_tree, &arg_tree);
  if (fn_tree == error_mark_node || arg_tree == error_mark_node)
    return error_mark_node;

  static tree defer_fndecl;

  tree fn_arg_type = NULL_TREE;
  if (defer_fndecl == NULL_TREE)
    {
      // Only build FN_ARG_TYPE if we need it.
      tree subargtypes = tree_cons(NULL_TREE, ptr_type_node, void_list_node);
      tree subfntype = build_function_type(ptr_type_node, subargtypes);
      fn_arg_type = build_pointer_type(subfntype);
    }

  tree defer_stack = context->function()->func_value()->defer_stack(loc);

  return Gogo::call_builtin(&defer_fndecl,
			    loc,
			    "__go_defer",
			    3,
			    void_type_node,
			    ptr_type_node,
			    defer_stack,
			    fn_arg_type,
			    fn_tree,
			    ptr_type_node,
			    arg_tree);
}

// Make a defer statement.

Statement*
Statement::make_defer_statement(Call_expression* call,
				source_location location)
{
  return new Defer_statement(call, location);
}

// Class Return_statement.

// Traverse assignments.  We treat each return value as a top level
// RHS in an expression.

bool
Return_statement::do_traverse_assignments(Traverse_assignments* tassign)
{
  Expression_list* vals = this->vals_;
  if (vals != NULL)
    {
      for (Expression_list::iterator p = vals->begin();
	   p != vals->end();
	   ++p)
	tassign->value(&*p, true, true);
    }
  return true;
}

// Lower a return statement.  If we are returning a function call
// which returns multiple values which match the current function,
// split up the call's results.  If the function has named result
// variables, and the return statement lists explicit values, then
// implement it by assigning the values to the result variables and
// changing the statement to not list any values.  This lets
// panic/recover work correctly.

Statement*
Return_statement::do_lower(Gogo*, Block* enclosing)
{
  if (this->vals_ == NULL)
    return this;

  const Typed_identifier_list* results = this->results_;
  if (results == NULL || results->empty())
    return this;

  // If the current function has multiple return values, and we are
  // returning a single call expression, split up the call expression.
  size_t results_count = results->size();
  if (results_count > 1
      && this->vals_->size() == 1
      && this->vals_->front()->call_expression() != NULL)
    {
      Call_expression* call = this->vals_->front()->call_expression();
      size_t count = results->size();
      Expression_list* vals = new Expression_list;
      for (size_t i = 0; i < count; ++i)
	vals->push_back(Expression::make_call_result(call, i));
      delete this->vals_;
      this->vals_ = vals;
    }

  if (results->front().name().empty())
    return this;

  if (results_count != this->vals_->size())
    {
      // Presumably an error which will be reported in check_types.
      return this;
    }

  // Assign to named return values and then return them.

  source_location loc = this->location();
  const Block* top = enclosing;
  while (top->enclosing() != NULL)
    top = top->enclosing();

  const Bindings *bindings = top->bindings();
  Block* b = new Block(enclosing, loc);

  Expression_list* lhs = new Expression_list();
  Expression_list* rhs = new Expression_list();

  Expression_list::const_iterator pe = this->vals_->begin();
  int i = 1;
  for (Typed_identifier_list::const_iterator pr = results->begin();
       pr != results->end();
       ++pr, ++pe, ++i)
    {
      Named_object* rv = bindings->lookup_local(pr->name());
      if (rv == NULL || !rv->is_result_variable())
	{
	  // Presumably an error.
	  delete b;
	  delete lhs;
	  delete rhs;
	  return this;
	}

      Expression* e = *pe;

      // Check types now so that we give a good error message.  The
      // result type is known.  We determine the expression type
      // early.

      Type *rvtype = rv->result_var_value()->type();
      Type_context type_context(rvtype, false);
      e->determine_type(&type_context);

      std::string reason;
      if (Type::are_assignable(rvtype, e->type(), &reason))
	{
	  Expression* ve = Expression::make_var_reference(rv, e->location());
	  lhs->push_back(ve);
	  rhs->push_back(e);
	}
      else
	{
	  if (reason.empty())
	    error_at(e->location(), "incompatible type for return value %d", i);
	  else
	    error_at(e->location(),
		     "incompatible type for return value %d (%s)",
		     i, reason.c_str());
	}
    }
  gcc_assert(lhs->size() == rhs->size());

  if (lhs->empty())
    ;
  else if (lhs->size() == 1)
    {
      b->add_statement(Statement::make_assignment(lhs->front(), rhs->front(),
						  loc));
      delete lhs;
      delete rhs;
    }
  else
    b->add_statement(Statement::make_tuple_assignment(lhs, rhs, loc));

  b->add_statement(Statement::make_return_statement(this->results_, NULL,
						    loc));

  return Statement::make_block_statement(b, loc);
}

// Determine types.

void
Return_statement::do_determine_types()
{
  if (this->vals_ == NULL)
    return;
  const Typed_identifier_list* results = this->results_;

  Typed_identifier_list::const_iterator pt;
  if (results != NULL)
    pt = results->begin();
  for (Expression_list::iterator pe = this->vals_->begin();
       pe != this->vals_->end();
       ++pe)
    {
      if (results == NULL || pt == results->end())
	(*pe)->determine_type_no_context();
      else
	{
	  Type_context context(pt->type(), false);
	  (*pe)->determine_type(&context);
	  ++pt;
	}
    }
}

// Check types.

void
Return_statement::do_check_types(Gogo*)
{
  if (this->vals_ == NULL)
    return;

  const Typed_identifier_list* results = this->results_;
  if (results == NULL)
    {
      this->report_error(_("return with value in function "
			   "with no return type"));
      return;
    }

  int i = 1;
  Typed_identifier_list::const_iterator pt = results->begin();
  for (Expression_list::const_iterator pe = this->vals_->begin();
       pe != this->vals_->end();
       ++pe, ++pt, ++i)
    {
      if (pt == results->end())
	{
	  this->report_error(_("too many values in return statement"));
	  return;
	}
      std::string reason;
      if (!Type::are_assignable(pt->type(), (*pe)->type(), &reason))
	{
	  if (reason.empty())
	    error_at(this->location(),
		     "incompatible type for return value %d",
		     i);
	  else
	    error_at(this->location(),
		     "incompatible type for return value %d (%s)",
		     i, reason.c_str());
	  this->set_is_error();
	}
      else if (pt->type()->is_error_type()
	       || (*pe)->type()->is_error_type()
	       || pt->type()->is_undefined()
	       || (*pe)->type()->is_undefined())
	{
	  // Make sure we get the error for an undefined type.
	  pt->type()->base();
	  (*pe)->type()->base();
	  this->set_is_error();
	}
    }

  if (pt != results->end())
    this->report_error(_("not enough values in return statement"));
}

// Build a RETURN_EXPR tree.

tree
Return_statement::do_get_tree(Translate_context* context)
{
  Function* function = context->function()->func_value();
  tree fndecl = function->get_decl();
  if (fndecl == error_mark_node || DECL_RESULT(fndecl) == error_mark_node)
    return error_mark_node;

  const Typed_identifier_list* results = this->results_;

  if (this->vals_ == NULL)
    {
      tree stmt_list = NULL_TREE;
      tree retval = function->return_value(context->gogo(),
					   context->function(),
					   this->location(),
					   &stmt_list);
      tree set;
      if (retval == NULL_TREE)
	set = NULL_TREE;
      else if (retval == error_mark_node)
	return error_mark_node;
      else
	set = fold_build2_loc(this->location(), MODIFY_EXPR, void_type_node,
			      DECL_RESULT(fndecl), retval);
      append_to_statement_list(this->build_stmt_1(RETURN_EXPR, set),
			       &stmt_list);
      return stmt_list;
    }
  else if (this->vals_->size() == 1)
    {
      gcc_assert(!VOID_TYPE_P(TREE_TYPE(TREE_TYPE(fndecl))));
      tree val = (*this->vals_->begin())->get_tree(context);
      gcc_assert(results != NULL && results->size() == 1);
      val = Expression::convert_for_assignment(context,
					       results->begin()->type(),
					       (*this->vals_->begin())->type(),
					       val, this->location());
      if (val == error_mark_node)
	return error_mark_node;
      tree set = build2(MODIFY_EXPR, void_type_node,
			DECL_RESULT(fndecl), val);
      SET_EXPR_LOCATION(set, this->location());
      return this->build_stmt_1(RETURN_EXPR, set);
    }
  else
    {
      gcc_assert(!VOID_TYPE_P(TREE_TYPE(TREE_TYPE(fndecl))));
      tree stmt_list = NULL_TREE;
      tree rettype = TREE_TYPE(DECL_RESULT(fndecl));
      tree retvar = create_tmp_var(rettype, "RESULT");
      gcc_assert(results != NULL && results->size() == this->vals_->size());
      Expression_list::const_iterator pv = this->vals_->begin();
      Typed_identifier_list::const_iterator pr = results->begin();
      for (tree field = TYPE_FIELDS(rettype);
	   field != NULL_TREE;
	   ++pv, ++pr, field = DECL_CHAIN(field))
	{
	  gcc_assert(pv != this->vals_->end());
	  tree val = (*pv)->get_tree(context);
	  val = Expression::convert_for_assignment(context, pr->type(),
						   (*pv)->type(), val,
						   this->location());
	  if (val == error_mark_node)
	    return error_mark_node;
	  tree set = build2(MODIFY_EXPR, void_type_node,
			    build3(COMPONENT_REF, TREE_TYPE(field),
				   retvar, field, NULL_TREE),
			    val);
	  SET_EXPR_LOCATION(set, this->location());
	  append_to_statement_list(set, &stmt_list);
	}
      tree set = build2(MODIFY_EXPR, void_type_node, DECL_RESULT(fndecl),
			retvar);
      append_to_statement_list(this->build_stmt_1(RETURN_EXPR, set),
			       &stmt_list);
      return stmt_list;
    }
}

// Make a return statement.

Statement*
Statement::make_return_statement(const Typed_identifier_list* results,
				 Expression_list* vals,
				 source_location location)
{
  return new Return_statement(results, vals, location);
}

// A break or continue statement.

class Bc_statement : public Statement
{
 public:
  Bc_statement(bool is_break, Unnamed_label* label, source_location location)
    : Statement(STATEMENT_BREAK_OR_CONTINUE, location),
      label_(label), is_break_(is_break)
  { }

  bool
  is_break() const
  { return this->is_break_; }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  bool
  do_may_fall_through() const
  { return false; }

  tree
  do_get_tree(Translate_context*)
  { return this->label_->get_goto(this->location()); }

 private:
  // The label that this branches to.
  Unnamed_label* label_;
  // True if this is "break", false if it is "continue".
  bool is_break_;
};

// Make a break statement.

Statement*
Statement::make_break_statement(Unnamed_label* label, source_location location)
{
  return new Bc_statement(true, label, location);
}

// Make a continue statement.

Statement*
Statement::make_continue_statement(Unnamed_label* label,
				   source_location location)
{
  return new Bc_statement(false, label, location);
}

// A goto statement.

class Goto_statement : public Statement
{
 public:
  Goto_statement(Label* label, source_location location)
    : Statement(STATEMENT_GOTO, location),
      label_(label)
  { }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  void
  do_check_types(Gogo*);

  bool
  do_may_fall_through() const
  { return false; }

  tree
  do_get_tree(Translate_context*);

 private:
  Label* label_;
};

// Check types for a label.  There aren't any types per se, but we use
// this to give an error if the label was never defined.

void
Goto_statement::do_check_types(Gogo*)
{
  if (!this->label_->is_defined())
    {
      error_at(this->location(), "reference to undefined label %qs",
	       Gogo::message_name(this->label_->name()).c_str());
      this->set_is_error();
    }
}

// Return the tree for the goto statement.

tree
Goto_statement::do_get_tree(Translate_context*)
{
  return this->build_stmt_1(GOTO_EXPR, this->label_->get_decl());
}

// Make a goto statement.

Statement*
Statement::make_goto_statement(Label* label, source_location location)
{
  return new Goto_statement(label, location);
}

// A goto statement to an unnamed label.

class Goto_unnamed_statement : public Statement
{
 public:
  Goto_unnamed_statement(Unnamed_label* label, source_location location)
    : Statement(STATEMENT_GOTO_UNNAMED, location),
      label_(label)
  { }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  bool
  do_may_fall_through() const
  { return false; }

  tree
  do_get_tree(Translate_context*)
  { return this->label_->get_goto(this->location()); }

 private:
  Unnamed_label* label_;
};

// Make a goto statement to an unnamed label.

Statement*
Statement::make_goto_unnamed_statement(Unnamed_label* label,
				       source_location location)
{
  return new Goto_unnamed_statement(label, location);
}

// Class Label_statement.

// Traversal.

int
Label_statement::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Return a tree defining this label.

tree
Label_statement::do_get_tree(Translate_context*)
{
  return this->build_stmt_1(LABEL_EXPR, this->label_->get_decl());
}

// Make a label statement.

Statement*
Statement::make_label_statement(Label* label, source_location location)
{
  return new Label_statement(label, location);
}

// An unnamed label statement.

class Unnamed_label_statement : public Statement
{
 public:
  Unnamed_label_statement(Unnamed_label* label)
    : Statement(STATEMENT_UNNAMED_LABEL, label->location()),
      label_(label)
  { }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  tree
  do_get_tree(Translate_context*)
  { return this->label_->get_definition(); }

 private:
  // The label.
  Unnamed_label* label_;
};

// Make an unnamed label statement.

Statement*
Statement::make_unnamed_label_statement(Unnamed_label* label)
{
  return new Unnamed_label_statement(label);
}

// An if statement.

class If_statement : public Statement
{
 public:
  If_statement(Expression* cond, Block* then_block, Block* else_block,
	       source_location location)
    : Statement(STATEMENT_IF, location),
      cond_(cond), then_block_(then_block), else_block_(else_block)
  { }

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types();

  void
  do_check_types(Gogo*);

  bool
  do_may_fall_through() const;

  tree
  do_get_tree(Translate_context*);

 private:
  Expression* cond_;
  Block* then_block_;
  Block* else_block_;
};

// Traversal.

int
If_statement::do_traverse(Traverse* traverse)
{
  if (this->cond_ != NULL)
    {
      if (this->traverse_expression(traverse, &this->cond_) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->then_block_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->else_block_ != NULL)
    {
      if (this->else_block_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

void
If_statement::do_determine_types()
{
  if (this->cond_ != NULL)
    {
      Type_context context(Type::lookup_bool_type(), false);
      this->cond_->determine_type(&context);
    }
  this->then_block_->determine_types();
  if (this->else_block_ != NULL)
    this->else_block_->determine_types();
}

// Check types.

void
If_statement::do_check_types(Gogo*)
{
  if (this->cond_ != NULL)
    {
      Type* type = this->cond_->type();
      if (type->is_error_type())
	this->set_is_error();
      else if (!type->is_boolean_type())
	this->report_error(_("expected boolean expression"));
    }
}

// Whether the overall statement may fall through.

bool
If_statement::do_may_fall_through() const
{
  return (this->else_block_ == NULL
	  || this->then_block_->may_fall_through()
	  || this->else_block_->may_fall_through());
}

// Get tree.

tree
If_statement::do_get_tree(Translate_context* context)
{
  gcc_assert(this->cond_ == NULL
	     || this->cond_->type()->is_boolean_type()
	     || this->cond_->type()->is_error_type());
  tree cond_tree = (this->cond_ == NULL
		    ? boolean_true_node
		    : this->cond_->get_tree(context));
  tree then_tree = this->then_block_->get_tree(context);
  tree else_tree = (this->else_block_ == NULL
		    ? NULL_TREE
		    : this->else_block_->get_tree(context));
  if (cond_tree == error_mark_node
      || then_tree == error_mark_node
      || else_tree == error_mark_node)
    return error_mark_node;
  tree ret = build3(COND_EXPR, void_type_node, cond_tree, then_tree,
		    else_tree);
  SET_EXPR_LOCATION(ret, this->location());
  return ret;
}

// Make an if statement.

Statement*
Statement::make_if_statement(Expression* cond, Block* then_block,
			     Block* else_block, source_location location)
{
  return new If_statement(cond, then_block, else_block, location);
}

// Class Case_clauses::Case_clause.

// Traversal.

int
Case_clauses::Case_clause::traverse(Traverse* traverse)
{
  if (this->cases_ != NULL
      && (traverse->traverse_mask()
	  & (Traverse::traverse_types | Traverse::traverse_expressions)) != 0)
    {
      if (this->cases_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->statements_ != NULL)
    {
      if (this->statements_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Check whether all the case expressions are integer constants.

bool
Case_clauses::Case_clause::is_constant() const
{
  if (this->cases_ != NULL)
    {
      for (Expression_list::const_iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	if (!(*p)->is_constant() || (*p)->type()->integer_type() == NULL)
	  return false;
    }
  return true;
}

// Lower a case clause for a nonconstant switch.  VAL_TEMP is the
// value we are switching on; it may be NULL.  If START_LABEL is not
// NULL, it goes at the start of the statements, after the condition
// test.  We branch to FINISH_LABEL at the end of the statements.

void
Case_clauses::Case_clause::lower(Block* b, Temporary_statement* val_temp,
				 Unnamed_label* start_label,
				 Unnamed_label* finish_label) const
{
  source_location loc = this->location_;
  Unnamed_label* next_case_label;
  if (this->cases_ == NULL || this->cases_->empty())
    {
      gcc_assert(this->is_default_);
      next_case_label = NULL;
    }
  else
    {
      Expression* cond = NULL;

      for (Expression_list::const_iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	{
	  Expression* this_cond;
	  if (val_temp == NULL)
	    this_cond = *p;
	  else
	    {
	      Expression* ref = Expression::make_temporary_reference(val_temp,
								     loc);
	      this_cond = Expression::make_binary(OPERATOR_EQEQ, ref, *p, loc);
	    }

	  if (cond == NULL)
	    cond = this_cond;
	  else
	    cond = Expression::make_binary(OPERATOR_OROR, cond, this_cond, loc);
	}

      Block* then_block = new Block(b, loc);
      next_case_label = new Unnamed_label(UNKNOWN_LOCATION);
      Statement* s = Statement::make_goto_unnamed_statement(next_case_label,
							    loc);
      then_block->add_statement(s);

      // if !COND { goto NEXT_CASE_LABEL }
      cond = Expression::make_unary(OPERATOR_NOT, cond, loc);
      s = Statement::make_if_statement(cond, then_block, NULL, loc);
      b->add_statement(s);
    }

  if (start_label != NULL)
    b->add_statement(Statement::make_unnamed_label_statement(start_label));

  if (this->statements_ != NULL)
    b->add_statement(Statement::make_block_statement(this->statements_, loc));

  Statement* s = Statement::make_goto_unnamed_statement(finish_label, loc);
  b->add_statement(s);

  if (next_case_label != NULL)
    b->add_statement(Statement::make_unnamed_label_statement(next_case_label));
}

// Determine types.

void
Case_clauses::Case_clause::determine_types(Type* type)
{
  if (this->cases_ != NULL)
    {
      Type_context case_context(type, false);
      for (Expression_list::iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	(*p)->determine_type(&case_context);
    }
  if (this->statements_ != NULL)
    this->statements_->determine_types();
}

// Check types.  Returns false if there was an error.

bool
Case_clauses::Case_clause::check_types(Type* type)
{
  if (this->cases_ != NULL)
    {
      for (Expression_list::iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	{
	  if (!Type::are_assignable(type, (*p)->type(), NULL)
	      && !Type::are_assignable((*p)->type(), type, NULL))
	    {
	      error_at((*p)->location(),
		       "type mismatch between switch value and case clause");
	      return false;
	    }
	}
    }
  return true;
}

// Return true if this clause may fall through to the following
// statements.  Note that this is not the same as whether the case
// uses the "fallthrough" keyword.

bool
Case_clauses::Case_clause::may_fall_through() const
{
  if (this->statements_ == NULL)
    return true;
  return this->statements_->may_fall_through();
}

// Build up the body of a SWITCH_EXPR.

void
Case_clauses::Case_clause::get_constant_tree(Translate_context* context,
					     Unnamed_label* break_label,
					     Case_constants* case_constants,
					     tree* stmt_list) const
{
  if (this->cases_ != NULL)
    {
      for (Expression_list::const_iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	{
	  Type* itype;
	  mpz_t ival;
	  mpz_init(ival);
	  if (!(*p)->integer_constant_value(true, ival, &itype))
	    {
	      // Something went wrong.  This can happen with a
	      // negative constant and an unsigned switch value.
	      gcc_assert(saw_errors());
	      continue;
	    }
	  gcc_assert(itype != NULL);
	  tree type_tree = itype->get_tree(context->gogo());
	  tree val = Expression::integer_constant_tree(ival, type_tree);
	  mpz_clear(ival);

	  if (val != error_mark_node)
	    {
	      gcc_assert(TREE_CODE(val) == INTEGER_CST);

	      std::pair<Case_constants::iterator, bool> ins =
		case_constants->insert(val);
	      if (!ins.second)
		{
		  // Value was already present.
		  warning_at(this->location_, 0,
			     "duplicate case value will never match");
		  continue;
		}

	      tree label = create_artificial_label(this->location_);
	      append_to_statement_list(build3(CASE_LABEL_EXPR, void_type_node,
					      val, NULL_TREE, label),
				       stmt_list);
	    }
	}
    }

  if (this->is_default_)
    {
      tree label = create_artificial_label(this->location_);
      append_to_statement_list(build3(CASE_LABEL_EXPR, void_type_node,
				      NULL_TREE, NULL_TREE, label),
			       stmt_list);
    }

  if (this->statements_ != NULL)
    {
      tree block_tree = this->statements_->get_tree(context);
      if (block_tree != error_mark_node)
	append_to_statement_list(block_tree, stmt_list);
    }

  if (!this->is_fallthrough_)
    append_to_statement_list(break_label->get_goto(this->location_), stmt_list);
}

// Class Case_clauses.

// Traversal.

int
Case_clauses::traverse(Traverse* traverse)
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (p->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Check whether all the case expressions are constant.

bool
Case_clauses::is_constant() const
{
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    if (!p->is_constant())
      return false;
  return true;
}

// Lower case clauses for a nonconstant switch.

void
Case_clauses::lower(Block* b, Temporary_statement* val_temp,
		    Unnamed_label* break_label) const
{
  // The default case.
  const Case_clause* default_case = NULL;

  // The label for the fallthrough of the previous case.
  Unnamed_label* last_fallthrough_label = NULL;

  // The label for the start of the default case.  This is used if the
  // case before the default case falls through.
  Unnamed_label* default_start_label = NULL;

  // The label for the end of the default case.  This normally winds
  // up as BREAK_LABEL, but it will be different if the default case
  // falls through.
  Unnamed_label* default_finish_label = NULL;

  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      // The label to use for the start of the statements for this
      // case.  This is NULL unless the previous case falls through.
      Unnamed_label* start_label = last_fallthrough_label;

      // The label to jump to after the end of the statements for this
      // case.
      Unnamed_label* finish_label = break_label;

      last_fallthrough_label = NULL;
      if (p->is_fallthrough() && p + 1 != this->clauses_.end())
	{
	  finish_label = new Unnamed_label(p->location());
	  last_fallthrough_label = finish_label;
	}

      if (!p->is_default())
	p->lower(b, val_temp, start_label, finish_label);
      else
	{
	  // We have to move the default case to the end, so that we
	  // only use it if all the other tests fail.
	  default_case = &*p;
	  default_start_label = start_label;
	  default_finish_label = finish_label;
	}
    }

  if (default_case != NULL)
    default_case->lower(b, val_temp, default_start_label,
			default_finish_label);
      
}

// Determine types.

void
Case_clauses::determine_types(Type* type)
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->determine_types(type);
}

// Check types.  Returns false if there was an error.

bool
Case_clauses::check_types(Type* type)
{
  bool ret = true;
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (!p->check_types(type))
	ret = false;
    }
  return ret;
}

// Return true if these clauses may fall through to the statements
// following the switch statement.

bool
Case_clauses::may_fall_through() const
{
  bool found_default = false;
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (p->may_fall_through() && !p->is_fallthrough())
	return true;
      if (p->is_default())
	found_default = true;
    }
  return !found_default;
}

// Return a tree when all case expressions are constants.

tree
Case_clauses::get_constant_tree(Translate_context* context,
				Unnamed_label* break_label) const
{
  Case_constants case_constants;
  tree stmt_list = NULL_TREE;
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->get_constant_tree(context, break_label, &case_constants,
			 &stmt_list);
  return stmt_list;
}

// A constant switch statement.  A Switch_statement is lowered to this
// when all the cases are constants.

class Constant_switch_statement : public Statement
{
 public:
  Constant_switch_statement(Expression* val, Case_clauses* clauses,
			    Unnamed_label* break_label,
			    source_location location)
    : Statement(STATEMENT_CONSTANT_SWITCH, location),
      val_(val), clauses_(clauses), break_label_(break_label)
  { }

 protected:
  int
  do_traverse(Traverse*);

  void
  do_determine_types();

  void
  do_check_types(Gogo*);

  bool
  do_may_fall_through() const;

  tree
  do_get_tree(Translate_context*);

 private:
  // The value to switch on.
  Expression* val_;
  // The case clauses.
  Case_clauses* clauses_;
  // The break label, if needed.
  Unnamed_label* break_label_;
};

// Traversal.

int
Constant_switch_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->clauses_->traverse(traverse);
}

// Determine types.

void
Constant_switch_statement::do_determine_types()
{
  this->val_->determine_type_no_context();
  this->clauses_->determine_types(this->val_->type());
}

// Check types.

void
Constant_switch_statement::do_check_types(Gogo*)
{
  if (!this->clauses_->check_types(this->val_->type()))
    this->set_is_error();
}

// Return whether this switch may fall through.

bool
Constant_switch_statement::do_may_fall_through() const
{
  if (this->clauses_ == NULL)
    return true;

  // If we have a break label, then some case needed it.  That implies
  // that the switch statement as a whole can fall through.
  if (this->break_label_ != NULL)
    return true;

  return this->clauses_->may_fall_through();
}

// Convert to GENERIC.

tree
Constant_switch_statement::do_get_tree(Translate_context* context)
{
  tree switch_val_tree = this->val_->get_tree(context);

  Unnamed_label* break_label = this->break_label_;
  if (break_label == NULL)
    break_label = new Unnamed_label(this->location());

  tree stmt_list = NULL_TREE;
  tree s = build3(SWITCH_EXPR, void_type_node, switch_val_tree,
		  this->clauses_->get_constant_tree(context, break_label),
		  NULL_TREE);
  SET_EXPR_LOCATION(s, this->location());
  append_to_statement_list(s, &stmt_list);

  append_to_statement_list(break_label->get_definition(), &stmt_list);

  return stmt_list;
}

// Class Switch_statement.

// Traversal.

int
Switch_statement::do_traverse(Traverse* traverse)
{
  if (this->val_ != NULL)
    {
      if (this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return this->clauses_->traverse(traverse);
}

// Lower a Switch_statement to a Constant_switch_statement or a series
// of if statements.

Statement*
Switch_statement::do_lower(Gogo*, Block* enclosing)
{
  source_location loc = this->location();

  if (this->val_ != NULL
      && (this->val_->is_error_expression()
	  || this->val_->type()->is_error_type()))
    return Statement::make_error_statement(loc);

  if (this->val_ != NULL
      && this->val_->type()->integer_type() != NULL
      && !this->clauses_->empty()
      && this->clauses_->is_constant())
    return new Constant_switch_statement(this->val_, this->clauses_,
					 this->break_label_, loc);

  Block* b = new Block(enclosing, loc);

  if (this->clauses_->empty())
    {
      Expression* val = this->val_;
      if (val == NULL)
	val = Expression::make_boolean(true, loc);
      return Statement::make_statement(val);
    }

  Temporary_statement* val_temp;
  if (this->val_ == NULL)
    val_temp = NULL;
  else
    {
      // var val_temp VAL_TYPE = VAL
      val_temp = Statement::make_temporary(NULL, this->val_, loc);
      b->add_statement(val_temp);
    }

  this->clauses_->lower(b, val_temp, this->break_label());

  Statement* s = Statement::make_unnamed_label_statement(this->break_label_);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Return the break label for this switch statement, creating it if
// necessary.

Unnamed_label*
Switch_statement::break_label()
{
  if (this->break_label_ == NULL)
    this->break_label_ = new Unnamed_label(this->location());
  return this->break_label_;
}

// Make a switch statement.

Switch_statement*
Statement::make_switch_statement(Expression* val, source_location location)
{
  return new Switch_statement(val, location);
}

// Class Type_case_clauses::Type_case_clause.

// Traversal.

int
Type_case_clauses::Type_case_clause::traverse(Traverse* traverse)
{
  if (!this->is_default_
      && ((traverse->traverse_mask()
	   & (Traverse::traverse_types | Traverse::traverse_expressions)) != 0)
      && Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->statements_ != NULL)
    return this->statements_->traverse(traverse);
  return TRAVERSE_CONTINUE;
}

// Lower one clause in a type switch.  Add statements to the block B.
// The type descriptor we are switching on is in DESCRIPTOR_TEMP.
// BREAK_LABEL is the label at the end of the type switch.
// *STMTS_LABEL, if not NULL, is a label to put at the start of the
// statements.

void
Type_case_clauses::Type_case_clause::lower(Block* b,
					   Temporary_statement* descriptor_temp,
					   Unnamed_label* break_label,
					   Unnamed_label** stmts_label) const
{
  source_location loc = this->location_;

  Unnamed_label* next_case_label = NULL;
  if (!this->is_default_)
    {
      Type* type = this->type_;

      Expression* cond;
      // The language permits case nil, which is of course a constant
      // rather than a type.  It will appear here as an invalid
      // forwarding type.
      if (type->is_nil_constant_as_type())
	{
	  Expression* ref =
	    Expression::make_temporary_reference(descriptor_temp, loc);
	  cond = Expression::make_binary(OPERATOR_EQEQ, ref,
					 Expression::make_nil(loc),
					 loc);
	}
      else
	{
	  Expression* func;
	  if (type->interface_type() == NULL)
	    {
	      // func ifacetypeeq(*descriptor, *descriptor) bool
	      static Named_object* ifacetypeeq;
	      if (ifacetypeeq == NULL)
		{
		  const source_location bloc = BUILTINS_LOCATION;
		  Typed_identifier_list* param_types =
		    new Typed_identifier_list();
		  Type* descriptor_type = Type::make_type_descriptor_ptr_type();
		  param_types->push_back(Typed_identifier("a", descriptor_type,
							  bloc));
		  param_types->push_back(Typed_identifier("b", descriptor_type,
							  bloc));
		  Typed_identifier_list* ret_types =
		    new Typed_identifier_list();
		  Type* bool_type = Type::lookup_bool_type();
		  ret_types->push_back(Typed_identifier("", bool_type, bloc));
		  Function_type* fntype = Type::make_function_type(NULL,
								   param_types,
								   ret_types,
								   bloc);
		  ifacetypeeq =
		    Named_object::make_function_declaration("ifacetypeeq", NULL,
							    fntype, bloc);
		  const char* n = "runtime.ifacetypeeq";
		  ifacetypeeq->func_declaration_value()->set_asm_name(n);
		}

	      // ifacetypeeq(descriptor_temp, DESCRIPTOR)
	      func = Expression::make_func_reference(ifacetypeeq, NULL, loc);
	    }
	  else
	    {
	      // func ifaceI2Tp(*descriptor, *descriptor) bool
	      static Named_object* ifaceI2Tp;
	      if (ifaceI2Tp == NULL)
		{
		  const source_location bloc = BUILTINS_LOCATION;
		  Typed_identifier_list* param_types =
		    new Typed_identifier_list();
		  Type* descriptor_type = Type::make_type_descriptor_ptr_type();
		  param_types->push_back(Typed_identifier("a", descriptor_type,
							  bloc));
		  param_types->push_back(Typed_identifier("b", descriptor_type,
							  bloc));
		  Typed_identifier_list* ret_types =
		    new Typed_identifier_list();
		  Type* bool_type = Type::lookup_bool_type();
		  ret_types->push_back(Typed_identifier("", bool_type, bloc));
		  Function_type* fntype = Type::make_function_type(NULL,
								   param_types,
								   ret_types,
								   bloc);
		  ifaceI2Tp =
		    Named_object::make_function_declaration("ifaceI2Tp", NULL,
							    fntype, bloc);
		  const char* n = "runtime.ifaceI2Tp";
		  ifaceI2Tp->func_declaration_value()->set_asm_name(n);
		}

	      // ifaceI2Tp(descriptor_temp, DESCRIPTOR)
	      func = Expression::make_func_reference(ifaceI2Tp, NULL, loc);
	    }
	  Expression_list* params = new Expression_list();
	  params->push_back(Expression::make_type_descriptor(type, loc));
	  Expression* ref =
	    Expression::make_temporary_reference(descriptor_temp, loc);
	  params->push_back(ref);
	  cond = Expression::make_call(func, params, false, loc);
	}

      Unnamed_label* dest;
      if (!this->is_fallthrough_)
	{
	  // if !COND { goto NEXT_CASE_LABEL }
	  next_case_label = new Unnamed_label(UNKNOWN_LOCATION);
	  dest = next_case_label;
	  cond = Expression::make_unary(OPERATOR_NOT, cond, loc);
	}
      else
	{
	  // if COND { goto STMTS_LABEL }
	  gcc_assert(stmts_label != NULL);
	  if (*stmts_label == NULL)
	    *stmts_label = new Unnamed_label(UNKNOWN_LOCATION);
	  dest = *stmts_label;
	}
      Block* then_block = new Block(b, loc);
      Statement* s = Statement::make_goto_unnamed_statement(dest, loc);
      then_block->add_statement(s);
      s = Statement::make_if_statement(cond, then_block, NULL, loc);
      b->add_statement(s);
    }

  if (this->statements_ != NULL
      || (!this->is_fallthrough_
	  && stmts_label != NULL
	  && *stmts_label != NULL))
    {
      gcc_assert(!this->is_fallthrough_);
      if (stmts_label != NULL && *stmts_label != NULL)
	{
	  gcc_assert(!this->is_default_);
	  if (this->statements_ != NULL)
	    (*stmts_label)->set_location(this->statements_->start_location());
	  Statement* s = Statement::make_unnamed_label_statement(*stmts_label);
	  b->add_statement(s);
	  *stmts_label = NULL;
	}
      if (this->statements_ != NULL)
	b->add_statement(Statement::make_block_statement(this->statements_,
							 loc));
    }

  if (this->is_fallthrough_)
    gcc_assert(next_case_label == NULL);
  else
    {
      source_location gloc = (this->statements_ == NULL
			      ? loc
			      : this->statements_->end_location());
      b->add_statement(Statement::make_goto_unnamed_statement(break_label,
							      gloc));
      if (next_case_label != NULL)
	{
	  Statement* s =
	    Statement::make_unnamed_label_statement(next_case_label);
	  b->add_statement(s);
	}
    }
}

// Class Type_case_clauses.

// Traversal.

int
Type_case_clauses::traverse(Traverse* traverse)
{
  for (Type_clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (p->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Check for duplicate types.

void
Type_case_clauses::check_duplicates() const
{
  typedef Unordered_set_hash(const Type*, Type_hash_identical,
			     Type_identical) Types_seen;
  Types_seen types_seen;
  for (Type_clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      Type* t = p->type();
      if (t == NULL)
	continue;
      if (t->is_nil_constant_as_type())
	t = Type::make_nil_type();
      std::pair<Types_seen::iterator, bool> ins = types_seen.insert(t);
      if (!ins.second)
	error_at(p->location(), "duplicate type in switch");
    }
}

// Lower the clauses in a type switch.  Add statements to the block B.
// The type descriptor we are switching on is in DESCRIPTOR_TEMP.
// BREAK_LABEL is the label at the end of the type switch.

void
Type_case_clauses::lower(Block* b, Temporary_statement* descriptor_temp,
			 Unnamed_label* break_label) const
{
  const Type_case_clause* default_case = NULL;

  Unnamed_label* stmts_label = NULL;
  for (Type_clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (!p->is_default())
	p->lower(b, descriptor_temp, break_label, &stmts_label);
      else
	{
	  // We are generating a series of tests, which means that we
	  // need to move the default case to the end.
	  default_case = &*p;
	}
    }
  gcc_assert(stmts_label == NULL);

  if (default_case != NULL)
    default_case->lower(b, descriptor_temp, break_label, NULL);
}

// Class Type_switch_statement.

// Traversal.

int
Type_switch_statement::do_traverse(Traverse* traverse)
{
  if (this->var_ == NULL)
    {
      if (this->traverse_expression(traverse, &this->expr_) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->clauses_ != NULL)
    return this->clauses_->traverse(traverse);
  return TRAVERSE_CONTINUE;
}

// Lower a type switch statement to a series of if statements.  The gc
// compiler is able to generate a table in some cases.  However, that
// does not work for us because we may have type descriptors in
// different shared libraries, so we can't compare them with simple
// equality testing.

Statement*
Type_switch_statement::do_lower(Gogo*, Block* enclosing)
{
  const source_location loc = this->location();

  if (this->clauses_ != NULL)
    this->clauses_->check_duplicates();

  Block* b = new Block(enclosing, loc);

  Type* val_type = (this->var_ != NULL
		    ? this->var_->var_value()->type()
		    : this->expr_->type());

  // var descriptor_temp DESCRIPTOR_TYPE
  Type* descriptor_type = Type::make_type_descriptor_ptr_type();
  Temporary_statement* descriptor_temp =
    Statement::make_temporary(descriptor_type, NULL, loc);
  b->add_statement(descriptor_temp);

  if (val_type->interface_type() == NULL)
    {
      // Doing a type switch on a non-interface type.  Should we issue
      // a warning for this case?
      Expression* lhs = Expression::make_temporary_reference(descriptor_temp,
							     loc);
      Expression* rhs;
      if (val_type->is_nil_type())
	rhs = Expression::make_nil(loc);
      else
	{
	  if (val_type->is_abstract())
	    val_type = val_type->make_non_abstract_type();
	  rhs = Expression::make_type_descriptor(val_type, loc);
	}
      Statement* s = Statement::make_assignment(lhs, rhs, loc);
      b->add_statement(s);
    }
  else
    {
      const source_location bloc = BUILTINS_LOCATION;

      // func {efacetype,ifacetype}(*interface) *descriptor
      // FIXME: This should be inlined.
      Typed_identifier_list* param_types = new Typed_identifier_list();
      param_types->push_back(Typed_identifier("i", val_type, bloc));
      Typed_identifier_list* ret_types = new Typed_identifier_list();
      ret_types->push_back(Typed_identifier("", descriptor_type, bloc));
      Function_type* fntype = Type::make_function_type(NULL, param_types,
						       ret_types, bloc);
      bool is_empty = val_type->interface_type()->is_empty();
      const char* fnname = is_empty ? "efacetype" : "ifacetype";
      Named_object* fn =
	Named_object::make_function_declaration(fnname, NULL, fntype, bloc);
      const char* asm_name = (is_empty
			      ? "runtime.efacetype"
			      : "runtime.ifacetype");
      fn->func_declaration_value()->set_asm_name(asm_name);

      // descriptor_temp = ifacetype(val_temp)
      Expression* func = Expression::make_func_reference(fn, NULL, loc);
      Expression_list* params = new Expression_list();
      Expression* ref;
      if (this->var_ == NULL)
	ref = this->expr_;
      else
	ref = Expression::make_var_reference(this->var_, loc);
      params->push_back(ref);
      Expression* call = Expression::make_call(func, params, false, loc);
      Expression* lhs = Expression::make_temporary_reference(descriptor_temp,
							     loc);
      Statement* s = Statement::make_assignment(lhs, call, loc);
      b->add_statement(s);
    }

  if (this->clauses_ != NULL)
    this->clauses_->lower(b, descriptor_temp, this->break_label());

  Statement* s = Statement::make_unnamed_label_statement(this->break_label_);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Return the break label for this type switch statement, creating it
// if necessary.

Unnamed_label*
Type_switch_statement::break_label()
{
  if (this->break_label_ == NULL)
    this->break_label_ = new Unnamed_label(this->location());
  return this->break_label_;
}

// Make a type switch statement.

Type_switch_statement*
Statement::make_type_switch_statement(Named_object* var, Expression* expr,
				      source_location location)
{
  return new Type_switch_statement(var, expr, location);
}

// Class Select_clauses::Select_clause.

// Traversal.

int
Select_clauses::Select_clause::traverse(Traverse* traverse)
{
  if (!this->is_lowered_
      && (traverse->traverse_mask()
	  & (Traverse::traverse_types | Traverse::traverse_expressions)) != 0)
    {
      if (this->channel_ != NULL)
	{
	  if (Expression::traverse(&this->channel_, traverse) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
      if (this->val_ != NULL)
	{
	  if (Expression::traverse(&this->val_, traverse) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
    }
  if (this->statements_ != NULL)
    {
      if (this->statements_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Lowering.  Here we pull out the channel and the send values, to
// enforce the order of evaluation.  We also add explicit send and
// receive statements to the clauses.

void
Select_clauses::Select_clause::lower(Block* b)
{
  if (this->is_default_)
    {
      gcc_assert(this->channel_ == NULL && this->val_ == NULL);
      this->is_lowered_ = true;
      return;
    }

  source_location loc = this->location_;

  // Evaluate the channel before the select statement.
  Temporary_statement* channel_temp = Statement::make_temporary(NULL,
								this->channel_,
								loc);
  b->add_statement(channel_temp);
  this->channel_ = Expression::make_temporary_reference(channel_temp, loc);

  // If this is a send clause, evaluate the value to send before the
  // select statement.
  Temporary_statement* val_temp = NULL;
  if (this->is_send_)
    {
      val_temp = Statement::make_temporary(NULL, this->val_, loc);
      b->add_statement(val_temp);
    }

  // Add the send or receive before the rest of the statements if any.
  Block *init = new Block(b, loc);
  Expression* ref = Expression::make_temporary_reference(channel_temp, loc);
  if (this->is_send_)
    {
      Expression* ref2 = Expression::make_temporary_reference(val_temp, loc);
      Send_expression* send = Expression::make_send(ref, ref2, loc);
      send->discarding_value();
      send->set_for_select();
      init->add_statement(Statement::make_statement(send));
    }
  else
    {
      Receive_expression* recv = Expression::make_receive(ref, loc);
      recv->set_for_select();
      if (this->val_ != NULL)
	{
	  gcc_assert(this->var_ == NULL);
	  init->add_statement(Statement::make_assignment(this->val_, recv,
							 loc));
	}
      else if (this->var_ != NULL)
	{
	  this->var_->var_value()->set_init(recv);
	  this->var_->var_value()->clear_type_from_chan_element();
	}
      else
	{
	  recv->discarding_value();
	  init->add_statement(Statement::make_statement(recv));
	}
    }

  if (this->statements_ != NULL)
    init->add_statement(Statement::make_block_statement(this->statements_,
							loc));

  this->statements_ = init;

  // Now all references should be handled through the statements, not
  // through here.
  this->is_lowered_ = true;
  this->val_ = NULL;
  this->var_ = NULL;
}

// Determine types.

void
Select_clauses::Select_clause::determine_types()
{
  gcc_assert(this->is_lowered_);
  if (this->statements_ != NULL)
    this->statements_->determine_types();
}

// Whether this clause may fall through to the statement which follows
// the overall select statement.

bool
Select_clauses::Select_clause::may_fall_through() const
{
  if (this->statements_ == NULL)
    return true;
  return this->statements_->may_fall_through();
}

// Return a tree for the statements to execute.

tree
Select_clauses::Select_clause::get_statements_tree(Translate_context* context)
{
  if (this->statements_ == NULL)
    return NULL_TREE;
  return this->statements_->get_tree(context);
}

// Class Select_clauses.

// Traversal.

int
Select_clauses::traverse(Traverse* traverse)
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (p->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Lowering.  Here we pull out the channel and the send values, to
// enforce the order of evaluation.  We also add explicit send and
// receive statements to the clauses.

void
Select_clauses::lower(Block* b)
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->lower(b);
}

// Determine types.

void
Select_clauses::determine_types()
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->determine_types();
}

// Return whether these select clauses fall through to the statement
// following the overall select statement.

bool
Select_clauses::may_fall_through() const
{
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    if (p->may_fall_through())
      return true;
  return false;
}

// Return a tree.  We build a call to
//   size_t __go_select(size_t count, _Bool has_default,
//                      channel* channels, _Bool* is_send)
//
// There are COUNT entries in the CHANNELS and IS_SEND arrays.  The
// value in the IS_SEND array is true for send, false for receive.
// __go_select returns an integer from 0 to COUNT, inclusive.  A
// return of 0 means that the default case should be run; this only
// happens if HAS_DEFAULT is non-zero.  Otherwise the number indicates
// the case to run.

// FIXME: This doesn't handle channels which send interface types
// where the receiver has a static type which matches that interface.

tree
Select_clauses::get_tree(Translate_context* context,
			 Unnamed_label *break_label,
			 source_location location)
{
  size_t count = this->clauses_.size();
  VEC(constructor_elt, gc)* chan_init = VEC_alloc(constructor_elt, gc, count);
  VEC(constructor_elt, gc)* is_send_init = VEC_alloc(constructor_elt, gc,
						     count);
  Select_clause* default_clause = NULL;
  tree final_stmt_list = NULL_TREE;
  tree channel_type_tree = NULL_TREE;

  size_t i = 0;
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (p->is_default())
	{
	  default_clause = &*p;
	  --count;
	  continue;
	}

      if (p->channel()->type()->channel_type() == NULL)
	{
	  // We should have given an error in the send or receive
	  // statement we created via lowering.
	  gcc_assert(saw_errors());
	  return error_mark_node;
	}

      tree channel_tree = p->channel()->get_tree(context);
      if (channel_tree == error_mark_node)
	return error_mark_node;
      channel_type_tree = TREE_TYPE(channel_tree);

      constructor_elt* elt = VEC_quick_push(constructor_elt, chan_init, NULL);
      elt->index = build_int_cstu(sizetype, i);
      elt->value = channel_tree;

      elt = VEC_quick_push(constructor_elt, is_send_init, NULL);
      elt->index = build_int_cstu(sizetype, i);
      elt->value = p->is_send() ? boolean_true_node : boolean_false_node;

      ++i;
    }
  gcc_assert(i == count);

  if (i == 0 && default_clause != NULL)
    {
      // There is only a default clause.
      gcc_assert(final_stmt_list == NULL_TREE);
      tree stmt_list = NULL_TREE;
      append_to_statement_list(default_clause->get_statements_tree(context),
			       &stmt_list);
      append_to_statement_list(break_label->get_definition(), &stmt_list);
      return stmt_list;
    }

  tree pointer_chan_type_tree = (channel_type_tree == NULL_TREE
				 ? ptr_type_node
				 : build_pointer_type(channel_type_tree));
  tree chans_arg;
  tree pointer_boolean_type_tree = build_pointer_type(boolean_type_node);
  tree is_sends_arg;

  if (i == 0)
    {
      chans_arg = fold_convert_loc(location, pointer_chan_type_tree,
				   null_pointer_node);
      is_sends_arg = fold_convert_loc(location, pointer_boolean_type_tree,
				      null_pointer_node);
    }
  else
    {
      tree index_type_tree = build_index_type(size_int(count - 1));
      tree chan_array_type_tree = build_array_type(channel_type_tree,
						   index_type_tree);
      tree chan_constructor = build_constructor(chan_array_type_tree,
						chan_init);
      tree chan_var = create_tmp_var(chan_array_type_tree, "CHAN");
      DECL_IGNORED_P(chan_var) = 0;
      DECL_INITIAL(chan_var) = chan_constructor;
      DECL_SOURCE_LOCATION(chan_var) = location;
      TREE_ADDRESSABLE(chan_var) = 1;
      tree decl_expr = build1(DECL_EXPR, void_type_node, chan_var);
      SET_EXPR_LOCATION(decl_expr, location);
      append_to_statement_list(decl_expr, &final_stmt_list);

      tree is_send_array_type_tree = build_array_type(boolean_type_node,
						      index_type_tree);
      tree is_send_constructor = build_constructor(is_send_array_type_tree,
						   is_send_init);
      tree is_send_var = create_tmp_var(is_send_array_type_tree, "ISSEND");
      DECL_IGNORED_P(is_send_var) = 0;
      DECL_INITIAL(is_send_var) = is_send_constructor;
      DECL_SOURCE_LOCATION(is_send_var) = location;
      TREE_ADDRESSABLE(is_send_var) = 1;
      decl_expr = build1(DECL_EXPR, void_type_node, is_send_var);
      SET_EXPR_LOCATION(decl_expr, location);
      append_to_statement_list(decl_expr, &final_stmt_list);

      chans_arg = fold_convert_loc(location, pointer_chan_type_tree,
				   build_fold_addr_expr_loc(location,
							    chan_var));
      is_sends_arg = fold_convert_loc(location, pointer_boolean_type_tree,
				      build_fold_addr_expr_loc(location,
							       is_send_var));
    }

  static tree select_fndecl;
  tree call = Gogo::call_builtin(&select_fndecl,
				 location,
				 "__go_select",
				 4,
				 sizetype,
				 sizetype,
				 size_int(count),
				 boolean_type_node,
				 (default_clause == NULL
				  ? boolean_false_node
				  : boolean_true_node),
				 pointer_chan_type_tree,
				 chans_arg,
				 pointer_boolean_type_tree,
				 is_sends_arg);
  if (call == error_mark_node)
    return error_mark_node;

  tree stmt_list = NULL_TREE;

  if (default_clause != NULL)
    this->add_clause_tree(context, 0, default_clause, break_label, &stmt_list);

  i = 1;
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (!p->is_default())
	{
	  this->add_clause_tree(context, i, &*p, break_label, &stmt_list);
	  ++i;
	}
    }

  append_to_statement_list(break_label->get_definition(), &stmt_list);

  tree switch_stmt = build3(SWITCH_EXPR, sizetype, call, stmt_list, NULL_TREE);
  SET_EXPR_LOCATION(switch_stmt, location);
  append_to_statement_list(switch_stmt, &final_stmt_list);

  return final_stmt_list;
}

// Add the tree for CLAUSE to STMT_LIST.

void
Select_clauses::add_clause_tree(Translate_context* context, int case_index,
				Select_clause* clause,
				Unnamed_label* bottom_label, tree* stmt_list)
{
  tree label = create_artificial_label(clause->location());
  append_to_statement_list(build3(CASE_LABEL_EXPR, void_type_node,
				  build_int_cst(sizetype, case_index),
				  NULL_TREE, label),
			   stmt_list);
  append_to_statement_list(clause->get_statements_tree(context), stmt_list);
  tree g = bottom_label->get_goto(clause->statements() == NULL
				  ? clause->location()
				  : clause->statements()->end_location());
  append_to_statement_list(g, stmt_list);
}

// Class Select_statement.

// Return the break label for this switch statement, creating it if
// necessary.

Unnamed_label*
Select_statement::break_label()
{
  if (this->break_label_ == NULL)
    this->break_label_ = new Unnamed_label(this->location());
  return this->break_label_;
}

// Lower a select statement.  This will still return a select
// statement, but it will be modified to implement the order of
// evaluation rules, and to include the send and receive statements as
// explicit statements in the clauses.

Statement*
Select_statement::do_lower(Gogo*, Block* enclosing)
{
  if (this->is_lowered_)
    return this;
  Block* b = new Block(enclosing, this->location());
  this->clauses_->lower(b);
  this->is_lowered_ = true;
  b->add_statement(this);
  return Statement::make_block_statement(b, this->location());
}

// Return the tree for a select statement.

tree
Select_statement::do_get_tree(Translate_context* context)
{
  return this->clauses_->get_tree(context, this->break_label(),
				  this->location());
}

// Make a select statement.

Select_statement*
Statement::make_select_statement(source_location location)
{
  return new Select_statement(location);
}

// Class For_statement.

// Traversal.

int
For_statement::do_traverse(Traverse* traverse)
{
  if (this->init_ != NULL)
    {
      if (this->init_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->cond_ != NULL)
    {
      if (this->traverse_expression(traverse, &this->cond_) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->post_ != NULL)
    {
      if (this->post_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return this->statements_->traverse(traverse);
}

// Lower a For_statement into if statements and gotos.  Getting rid of
// complex statements make it easier to handle garbage collection.

Statement*
For_statement::do_lower(Gogo*, Block* enclosing)
{
  Statement* s;
  source_location loc = this->location();

  Block* b = new Block(enclosing, this->location());
  if (this->init_ != NULL)
    {
      s = Statement::make_block_statement(this->init_,
					  this->init_->start_location());
      b->add_statement(s);
    }

  Unnamed_label* entry = NULL;
  if (this->cond_ != NULL)
    {
      entry = new Unnamed_label(this->location());
      b->add_statement(Statement::make_goto_unnamed_statement(entry, loc));
    }

  Unnamed_label* top = new Unnamed_label(this->location());
  b->add_statement(Statement::make_unnamed_label_statement(top));

  s = Statement::make_block_statement(this->statements_,
				      this->statements_->start_location());
  b->add_statement(s);

  source_location end_loc = this->statements_->end_location();

  Unnamed_label* cont = this->continue_label_;
  if (cont != NULL)
    b->add_statement(Statement::make_unnamed_label_statement(cont));

  if (this->post_ != NULL)
    {
      s = Statement::make_block_statement(this->post_,
					  this->post_->start_location());
      b->add_statement(s);
      end_loc = this->post_->end_location();
    }

  if (this->cond_ == NULL)
    b->add_statement(Statement::make_goto_unnamed_statement(top, end_loc));
  else
    {
      b->add_statement(Statement::make_unnamed_label_statement(entry));

      source_location cond_loc = this->cond_->location();
      Block* then_block = new Block(b, cond_loc);
      s = Statement::make_goto_unnamed_statement(top, cond_loc);
      then_block->add_statement(s);

      s = Statement::make_if_statement(this->cond_, then_block, NULL, cond_loc);
      b->add_statement(s);
    }

  Unnamed_label* brk = this->break_label_;
  if (brk != NULL)
    b->add_statement(Statement::make_unnamed_label_statement(brk));

  b->set_end_location(end_loc);

  return Statement::make_block_statement(b, loc);
}

// Return the break label, creating it if necessary.

Unnamed_label*
For_statement::break_label()
{
  if (this->break_label_ == NULL)
    this->break_label_ = new Unnamed_label(this->location());
  return this->break_label_;
}

// Return the continue LABEL_EXPR.

Unnamed_label*
For_statement::continue_label()
{
  if (this->continue_label_ == NULL)
    this->continue_label_ = new Unnamed_label(this->location());
  return this->continue_label_;
}

// Set the break and continue labels a for statement.  This is used
// when lowering a for range statement.

void
For_statement::set_break_continue_labels(Unnamed_label* break_label,
					 Unnamed_label* continue_label)
{
  gcc_assert(this->break_label_ == NULL && this->continue_label_ == NULL);
  this->break_label_ = break_label;
  this->continue_label_ = continue_label;
}

// Make a for statement.

For_statement*
Statement::make_for_statement(Block* init, Expression* cond, Block* post,
			      source_location location)
{
  return new For_statement(init, cond, post, location);
}

// Class For_range_statement.

// Traversal.

int
For_range_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->index_var_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->value_var_ != NULL)
    {
      if (this->traverse_expression(traverse, &this->value_var_)
	  == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->traverse_expression(traverse, &this->range_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->statements_->traverse(traverse);
}

// Lower a for range statement.  For simplicity we lower this into a
// for statement, which will then be lowered in turn to goto
// statements.

Statement*
For_range_statement::do_lower(Gogo* gogo, Block* enclosing)
{
  Type* range_type = this->range_->type();
  if (range_type->points_to() != NULL
      && range_type->points_to()->array_type() != NULL
      && !range_type->points_to()->is_open_array_type())
    range_type = range_type->points_to();

  Type* index_type;
  Type* value_type = NULL;
  if (range_type->array_type() != NULL)
    {
      index_type = Type::lookup_integer_type("int");
      value_type = range_type->array_type()->element_type();
    }
  else if (range_type->is_string_type())
    {
      index_type = Type::lookup_integer_type("int");
      value_type = index_type;
    }
  else if (range_type->map_type() != NULL)
    {
      index_type = range_type->map_type()->key_type();
      value_type = range_type->map_type()->val_type();
    }
  else if (range_type->channel_type() != NULL)
    {
      index_type = range_type->channel_type()->element_type();
      if (this->value_var_ != NULL)
	{
	  if (!this->value_var_->type()->is_error_type())
	    this->report_error(_("too many variables for range clause "
				 "with channel"));
	  return Statement::make_error_statement(this->location());
	}
    }
  else
    {
      this->report_error(_("range clause must have "
			   "array, slice, setring, map, or channel type"));
      return Statement::make_error_statement(this->location());
    }

  source_location loc = this->location();
  Block* temp_block = new Block(enclosing, loc);

  Named_object* range_object = NULL;
  Temporary_statement* range_temp = NULL;
  Var_expression* ve = this->range_->var_expression();
  if (ve != NULL)
    range_object = ve->named_object();
  else
    {
      range_temp = Statement::make_temporary(NULL, this->range_, loc);
      temp_block->add_statement(range_temp);
    }

  Temporary_statement* index_temp = Statement::make_temporary(index_type,
							      NULL, loc);
  temp_block->add_statement(index_temp);

  Temporary_statement* value_temp = NULL;
  if (this->value_var_ != NULL)
    {
      value_temp = Statement::make_temporary(value_type, NULL, loc);
      temp_block->add_statement(value_temp);
    }

  Block* body = new Block(temp_block, loc);

  Block* init;
  Expression* cond;
  Block* iter_init;
  Block* post;

  // Arrange to do a loop appropriate for the type.  We will produce
  //   for INIT ; COND ; POST {
  //           ITER_INIT
  //           INDEX = INDEX_TEMP
  //           VALUE = VALUE_TEMP // If there is a value
  //           original statements
  //   }

  if (range_type->array_type() != NULL)
    this->lower_range_array(gogo, temp_block, body, range_object, range_temp,
			    index_temp, value_temp, &init, &cond, &iter_init,
			    &post);
  else if (range_type->is_string_type())
    this->lower_range_string(gogo, temp_block, body, range_object, range_temp,
			     index_temp, value_temp, &init, &cond, &iter_init,
			     &post);
  else if (range_type->map_type() != NULL)
    this->lower_range_map(gogo, temp_block, body, range_object, range_temp,
			  index_temp, value_temp, &init, &cond, &iter_init,
			  &post);
  else if (range_type->channel_type() != NULL)
    this->lower_range_channel(gogo, temp_block, body, range_object, range_temp,
			      index_temp, value_temp, &init, &cond, &iter_init,
			      &post);
  else
    gcc_unreachable();

  if (iter_init != NULL)
    body->add_statement(Statement::make_block_statement(iter_init, loc));

  Statement* assign;
  Expression* index_ref = Expression::make_temporary_reference(index_temp, loc);
  if (this->value_var_ == NULL)
    {
      assign = Statement::make_assignment(this->index_var_, index_ref, loc);
    }
  else
    {
      Expression_list* lhs = new Expression_list();
      lhs->push_back(this->index_var_);
      lhs->push_back(this->value_var_);

      Expression_list* rhs = new Expression_list();
      rhs->push_back(index_ref);
      rhs->push_back(Expression::make_temporary_reference(value_temp, loc));

      assign = Statement::make_tuple_assignment(lhs, rhs, loc);
    }
  body->add_statement(assign);

  body->add_statement(Statement::make_block_statement(this->statements_, loc));

  body->set_end_location(this->statements_->end_location());

  For_statement* loop = Statement::make_for_statement(init, cond, post,
						      this->location());
  loop->add_statements(body);
  loop->set_break_continue_labels(this->break_label_, this->continue_label_);

  temp_block->add_statement(loop);

  return Statement::make_block_statement(temp_block, loc);
}

// Return a reference to the range, which may be in RANGE_OBJECT or in
// RANGE_TEMP.

Expression*
For_range_statement::make_range_ref(Named_object* range_object,
				    Temporary_statement* range_temp,
				    source_location loc)
{
  if (range_object != NULL)
    return Expression::make_var_reference(range_object, loc);
  else
    return Expression::make_temporary_reference(range_temp, loc);
}

// Return a call to the predeclared function FUNCNAME passing a
// reference to the temporary variable ARG.

Expression*
For_range_statement::call_builtin(Gogo* gogo, const char* funcname,
				  Expression* arg,
				  source_location loc)
{
  Named_object* no = gogo->lookup_global(funcname);
  gcc_assert(no != NULL && no->is_function_declaration());
  Expression* func = Expression::make_func_reference(no, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(arg);
  return Expression::make_call(func, params, false, loc);
}

// Lower a for range over an array or slice.

void
For_range_statement::lower_range_array(Gogo* gogo,
				       Block* enclosing,
				       Block* body_block,
				       Named_object* range_object,
				       Temporary_statement* range_temp,
				       Temporary_statement* index_temp,
				       Temporary_statement* value_temp,
				       Block** pinit,
				       Expression** pcond,
				       Block** piter_init,
				       Block** ppost)
{
  source_location loc = this->location();

  // The loop we generate:
  //   len_temp := len(range)
  //   for index_temp = 0; index_temp < len_temp; index_temp++ {
  //           value_temp = range[index_temp]
  //           index = index_temp
  //           value = value_temp
  //           original body
  //   }

  // Set *PINIT to
  //   var len_temp int
  //   len_temp = len(range)
  //   index_temp = 0

  Block* init = new Block(enclosing, loc);

  Expression* ref = this->make_range_ref(range_object, range_temp, loc);
  Expression* len_call = this->call_builtin(gogo, "len", ref, loc);
  Temporary_statement* len_temp = Statement::make_temporary(index_temp->type(),
							    len_call, loc);
  init->add_statement(len_temp);

  mpz_t zval;
  mpz_init_set_ui(zval, 0UL);
  Expression* zexpr = Expression::make_integer(&zval, NULL, loc);
  mpz_clear(zval);

  ref = Expression::make_temporary_reference(index_temp, loc);
  Statement* s = Statement::make_assignment(ref, zexpr, loc);
  init->add_statement(s);

  *pinit = init;

  // Set *PCOND to
  //   index_temp < len_temp

  ref = Expression::make_temporary_reference(index_temp, loc);
  Expression* ref2 = Expression::make_temporary_reference(len_temp, loc);
  Expression* lt = Expression::make_binary(OPERATOR_LT, ref, ref2, loc);

  *pcond = lt;

  // Set *PITER_INIT to
  //   value_temp = range[index_temp]

  Block* iter_init = NULL;
  if (value_temp != NULL)
    {
      iter_init = new Block(body_block, loc);

      ref = this->make_range_ref(range_object, range_temp, loc);
      Expression* ref2 = Expression::make_temporary_reference(index_temp, loc);
      Expression* index = Expression::make_index(ref, ref2, NULL, loc);

      ref = Expression::make_temporary_reference(value_temp, loc);
      s = Statement::make_assignment(ref, index, loc);

      iter_init->add_statement(s);
    }
  *piter_init = iter_init;

  // Set *PPOST to
  //   index_temp++

  Block* post = new Block(enclosing, loc);
  ref = Expression::make_temporary_reference(index_temp, loc);
  s = Statement::make_inc_statement(ref);
  post->add_statement(s);
  *ppost = post;
}

// Lower a for range over a string.

void
For_range_statement::lower_range_string(Gogo* gogo,
					Block* enclosing,
					Block* body_block,
					Named_object* range_object,
					Temporary_statement* range_temp,
					Temporary_statement* index_temp,
					Temporary_statement* value_temp,
					Block** pinit,
					Expression** pcond,
					Block** piter_init,
					Block** ppost)
{
  source_location loc = this->location();

  // The loop we generate:
  //   var next_index_temp int
  //   for index_temp = 0; ; index_temp = next_index_temp {
  //           next_index_temp, value_temp = stringiter2(range, index_temp)
  //           if next_index_temp == 0 {
  //                   break
  //           }
  //           index = index_temp
  //           value = value_temp
  //           original body
  //   }

  // Set *PINIT to
  //   var next_index_temp int
  //   index_temp = 0

  Block* init = new Block(enclosing, loc);

  Temporary_statement* next_index_temp =
    Statement::make_temporary(index_temp->type(), NULL, loc);
  init->add_statement(next_index_temp);

  mpz_t zval;
  mpz_init_set_ui(zval, 0UL);
  Expression* zexpr = Expression::make_integer(&zval, NULL, loc);

  Expression* ref = Expression::make_temporary_reference(index_temp, loc);
  Statement* s = Statement::make_assignment(ref, zexpr, loc);

  init->add_statement(s);
  *pinit = init;

  // The loop has no condition.

  *pcond = NULL;

  // Set *PITER_INIT to
  //   next_index_temp = runtime.stringiter(range, index_temp)
  // or
  //   next_index_temp, value_temp = runtime.stringiter2(range, index_temp)
  // followed by
  //   if next_index_temp == 0 {
  //           break
  //   }

  Block* iter_init = new Block(body_block, loc);

  Named_object* no;
  if (value_temp == NULL)
    {
      static Named_object* stringiter;
      if (stringiter == NULL)
	{
	  source_location bloc = BUILTINS_LOCATION;
	  Type* int_type = gogo->lookup_global("int")->type_value();

	  Typed_identifier_list* params = new Typed_identifier_list();
	  params->push_back(Typed_identifier("s", Type::make_string_type(),
					     bloc));
	  params->push_back(Typed_identifier("k", int_type, bloc));

	  Typed_identifier_list* results = new Typed_identifier_list();
	  results->push_back(Typed_identifier("", int_type, bloc));

	  Function_type* fntype = Type::make_function_type(NULL, params,
							   results, bloc);
	  stringiter = Named_object::make_function_declaration("stringiter",
							       NULL, fntype,
							       bloc);
	  const char* n = "runtime.stringiter";
	  stringiter->func_declaration_value()->set_asm_name(n);
	}
      no = stringiter;
    }
  else
    {
      static Named_object* stringiter2;
      if (stringiter2 == NULL)
	{
	  source_location bloc = BUILTINS_LOCATION;
	  Type* int_type = gogo->lookup_global("int")->type_value();

	  Typed_identifier_list* params = new Typed_identifier_list();
	  params->push_back(Typed_identifier("s", Type::make_string_type(),
					     bloc));
	  params->push_back(Typed_identifier("k", int_type, bloc));

	  Typed_identifier_list* results = new Typed_identifier_list();
	  results->push_back(Typed_identifier("", int_type, bloc));
	  results->push_back(Typed_identifier("", int_type, bloc));

	  Function_type* fntype = Type::make_function_type(NULL, params,
							   results, bloc);
	  stringiter2 = Named_object::make_function_declaration("stringiter",
								NULL, fntype,
								bloc);
	  const char* n = "runtime.stringiter2";
	  stringiter2->func_declaration_value()->set_asm_name(n);
	}
      no = stringiter2;
    }

  Expression* func = Expression::make_func_reference(no, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(this->make_range_ref(range_object, range_temp, loc));
  params->push_back(Expression::make_temporary_reference(index_temp, loc));
  Call_expression* call = Expression::make_call(func, params, false, loc);

  if (value_temp == NULL)
    {
      ref = Expression::make_temporary_reference(next_index_temp, loc);
      s = Statement::make_assignment(ref, call, loc);
    }
  else
    {
      Expression_list* lhs = new Expression_list();
      lhs->push_back(Expression::make_temporary_reference(next_index_temp,
							  loc));
      lhs->push_back(Expression::make_temporary_reference(value_temp, loc));

      Expression_list* rhs = new Expression_list();
      rhs->push_back(Expression::make_call_result(call, 0));
      rhs->push_back(Expression::make_call_result(call, 1));

      s = Statement::make_tuple_assignment(lhs, rhs, loc);
    }
  iter_init->add_statement(s);

  ref = Expression::make_temporary_reference(next_index_temp, loc);
  zexpr = Expression::make_integer(&zval, NULL, loc);
  mpz_clear(zval);
  Expression* equals = Expression::make_binary(OPERATOR_EQEQ, ref, zexpr, loc);

  Block* then_block = new Block(iter_init, loc);
  s = Statement::make_break_statement(this->break_label(), loc);
  then_block->add_statement(s);

  s = Statement::make_if_statement(equals, then_block, NULL, loc);
  iter_init->add_statement(s);

  *piter_init = iter_init;

  // Set *PPOST to
  //   index_temp = next_index_temp

  Block* post = new Block(enclosing, loc);

  Expression* lhs = Expression::make_temporary_reference(index_temp, loc);
  Expression* rhs = Expression::make_temporary_reference(next_index_temp, loc);
  s = Statement::make_assignment(lhs, rhs, loc);

  post->add_statement(s);
  *ppost = post;
}

// Lower a for range over a map.

void
For_range_statement::lower_range_map(Gogo* gogo,
				     Block* enclosing,
				     Block* body_block,
				     Named_object* range_object,
				     Temporary_statement* range_temp,
				     Temporary_statement* index_temp,
				     Temporary_statement* value_temp,
				     Block** pinit,
				     Expression** pcond,
				     Block** piter_init,
				     Block** ppost)
{
  source_location loc = this->location();

  // The runtime uses a struct to handle ranges over a map.  The
  // struct is four pointers long.  The first pointer is NULL when we
  // have completed the iteration.

  // The loop we generate:
  //   var hiter map_iteration_struct
  //   for mapiterinit(range, &hiter); hiter[0] != nil; mapiternext(&hiter) {
  //           mapiter2(hiter, &index_temp, &value_temp)
  //           index = index_temp
  //           value = value_temp
  //           original body
  //   }

  // Set *PINIT to
  //   var hiter map_iteration_struct
  //   runtime.mapiterinit(range, &hiter)

  Block* init = new Block(enclosing, loc);

  const unsigned long map_iteration_size = 4;

  mpz_t ival;
  mpz_init_set_ui(ival, map_iteration_size);
  Expression* iexpr = Expression::make_integer(&ival, NULL, loc);
  mpz_clear(ival);

  Type* byte_type = gogo->lookup_global("byte")->type_value();
  Type* ptr_type = Type::make_pointer_type(byte_type);

  Type* map_iteration_type = Type::make_array_type(ptr_type, iexpr);
  Type* map_iteration_ptr = Type::make_pointer_type(map_iteration_type);

  Temporary_statement* hiter = Statement::make_temporary(map_iteration_type,
							 NULL, loc);
  init->add_statement(hiter);

  source_location bloc = BUILTINS_LOCATION;
  Typed_identifier_list* param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("map", this->range_->type(), bloc));
  param_types->push_back(Typed_identifier("it", map_iteration_ptr, bloc));
  Function_type* fntype = Type::make_function_type(NULL, param_types, NULL,
						   bloc);

  Named_object* mapiterinit =
    Named_object::make_function_declaration("mapiterinit", NULL, fntype, bloc);
  const char* n = "runtime.mapiterinit";
  mapiterinit->func_declaration_value()->set_asm_name(n);

  Expression* func = Expression::make_func_reference(mapiterinit, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(this->make_range_ref(range_object, range_temp, loc));
  Expression* ref = Expression::make_temporary_reference(hiter, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  Expression* call = Expression::make_call(func, params, false, loc);
  init->add_statement(Statement::make_statement(call));

  *pinit = init;

  // Set *PCOND to
  //   hiter[0] != nil

  ref = Expression::make_temporary_reference(hiter, loc);

  mpz_t zval;
  mpz_init_set_ui(zval, 0UL);
  Expression* zexpr = Expression::make_integer(&zval, NULL, loc);
  mpz_clear(zval);

  Expression* index = Expression::make_index(ref, zexpr, NULL, loc);

  Expression* ne = Expression::make_binary(OPERATOR_NOTEQ, index,
					   Expression::make_nil(loc),
					   loc);

  *pcond = ne;

  // Set *PITER_INIT to
  //   mapiter1(hiter, &index_temp)
  // or
  //   mapiter2(hiter, &index_temp, &value_temp)

  Block* iter_init = new Block(body_block, loc);

  param_types = new Typed_identifier_list();
  param_types->push_back(Typed_identifier("hiter", map_iteration_ptr, bloc));
  Type* pkey_type = Type::make_pointer_type(index_temp->type());
  param_types->push_back(Typed_identifier("key", pkey_type, bloc));
  if (value_temp != NULL)
    {
      Type* pval_type = Type::make_pointer_type(value_temp->type());
      param_types->push_back(Typed_identifier("val", pval_type, bloc));
    }
  fntype = Type::make_function_type(NULL, param_types, NULL, bloc);
  n = value_temp == NULL ? "mapiter1" : "mapiter2";
  Named_object* mapiter = Named_object::make_function_declaration(n, NULL,
								  fntype, bloc);
  n = value_temp == NULL ? "runtime.mapiter1" : "runtime.mapiter2";
  mapiter->func_declaration_value()->set_asm_name(n);

  func = Expression::make_func_reference(mapiter, NULL, loc);
  params = new Expression_list();
  ref = Expression::make_temporary_reference(hiter, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  ref = Expression::make_temporary_reference(index_temp, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  if (value_temp != NULL)
    {
      ref = Expression::make_temporary_reference(value_temp, loc);
      params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
    }
  call = Expression::make_call(func, params, false, loc);
  iter_init->add_statement(Statement::make_statement(call));

  *piter_init = iter_init;

  // Set *PPOST to
  //   mapiternext(&hiter)

  Block* post = new Block(enclosing, loc);

  static Named_object* mapiternext;
  if (mapiternext == NULL)
    {
      param_types = new Typed_identifier_list();
      param_types->push_back(Typed_identifier("it", map_iteration_ptr, bloc));
      fntype = Type::make_function_type(NULL, param_types, NULL, bloc);
      mapiternext = Named_object::make_function_declaration("mapiternext",
							    NULL, fntype,
							    bloc);
      const char* n = "runtime.mapiternext";
      mapiternext->func_declaration_value()->set_asm_name(n);
    }

  func = Expression::make_func_reference(mapiternext, NULL, loc);
  params = new Expression_list();
  ref = Expression::make_temporary_reference(hiter, loc);
  params->push_back(Expression::make_unary(OPERATOR_AND, ref, loc));
  call = Expression::make_call(func, params, false, loc);
  post->add_statement(Statement::make_statement(call));

  *ppost = post;
}

// Lower a for range over a channel.

void
For_range_statement::lower_range_channel(Gogo* gogo,
					 Block*,
					 Block* body_block,
					 Named_object* range_object,
					 Temporary_statement* range_temp,
					 Temporary_statement* index_temp,
					 Temporary_statement* value_temp,
					 Block** pinit,
					 Expression** pcond,
					 Block** piter_init,
					 Block** ppost)
{
  gcc_assert(value_temp == NULL);

  source_location loc = this->location();

  // The loop we generate:
  //   for {
  //           index_temp = <-range
  //           if closed(range) {
  //                   break
  //           }
  //           index = index_temp
  //           value = value_temp
  //           original body
  //   }

  // We have no initialization code, no condition, and no post code.

  *pinit = NULL;
  *pcond = NULL;
  *ppost = NULL;

  // Set *PITER_INIT to
  //   index_temp = <-range
  //   if closed(range) {
  //           break
  //   }

  Block* iter_init = new Block(body_block, loc);

  Expression* ref = this->make_range_ref(range_object, range_temp, loc);
  Expression* cond = this->call_builtin(gogo, "closed", ref, loc);

  ref = this->make_range_ref(range_object, range_temp, loc);
  Expression* recv = Expression::make_receive(ref, loc);
  ref = Expression::make_temporary_reference(index_temp, loc);
  Statement* s = Statement::make_assignment(ref, recv, loc);
  iter_init->add_statement(s);

  Block* then_block = new Block(iter_init, loc);
  s = Statement::make_break_statement(this->break_label(), loc);
  then_block->add_statement(s);

  s = Statement::make_if_statement(cond, then_block, NULL, loc);
  iter_init->add_statement(s);

  *piter_init = iter_init;
}

// Return the break LABEL_EXPR.

Unnamed_label*
For_range_statement::break_label()
{
  if (this->break_label_ == NULL)
    this->break_label_ = new Unnamed_label(this->location());
  return this->break_label_;
}

// Return the continue LABEL_EXPR.

Unnamed_label*
For_range_statement::continue_label()
{
  if (this->continue_label_ == NULL)
    this->continue_label_ = new Unnamed_label(this->location());
  return this->continue_label_;
}

// Make a for statement with a range clause.

For_range_statement*
Statement::make_for_range_statement(Expression* index_var,
				    Expression* value_var,
				    Expression* range,
				    source_location location)
{
  return new For_range_statement(index_var, value_var, range, location);
}
