// statements.cc -- Go frontend statements.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "go-diagnostics.h"
#include "types.h"
#include "expressions.h"
#include "gogo.h"
#include "runtime.h"
#include "backend.h"
#include "statements.h"
#include "ast-dump.h"

// Class Statement.

Statement::Statement(Statement_classification classification,
		     Location location)
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

// Convert a Statement to the backend representation.  This is really
// done by the child class.

Bstatement*
Statement::get_backend(Translate_context* context)
{
  if (this->classification_ == STATEMENT_ERROR)
    return context->backend()->error_statement();
  return this->do_get_backend(context);
}

// Dump AST representation for a statement to a dump context.

void
Statement::dump_statement(Ast_dump_context* ast_dump_context) const
{
  this->do_dump_statement(ast_dump_context);
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
  go_error_at(this->location_, "%s", msg);
  this->set_is_error();
}

// An error statement, used to avoid crashing after we report an
// error.

class Error_statement : public Statement
{
 public:
  Error_statement(Location location)
    : Statement(STATEMENT_ERROR, location)
  { }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;
};

//
// Helper to tack on available source position information
// at the end of a statement.
//
static std::string
dsuffix(Location location)
{
  std::string lstr = Linemap::location_to_string(location);
  if (lstr == "")
    return lstr;
  std::string rval(" // ");
  rval += lstr;
  return rval;
}

// Dump the AST representation for an error statement.

void
Error_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "Error statement" << std::endl;
}

// Make an error statement.

Statement*
Statement::make_error_statement(Location location)
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

// Lower the variable's initialization expression.

Statement*
Variable_declaration_statement::do_lower(Gogo* gogo, Named_object* function,
					 Block*, Statement_inserter* inserter)
{
  this->var_->var_value()->lower_init_expression(gogo, function, inserter);
  return this;
}

// Flatten the variable's initialization expression.

Statement*
Variable_declaration_statement::do_flatten(Gogo* gogo, Named_object* function,
                                           Block*, Statement_inserter* inserter)
{
  Variable* var = this->var_->var_value();
  if (var->type()->is_error_type()
      || (var->init() != NULL
          && var->init()->is_error_expression()))
    {
      go_assert(saw_errors());
      return Statement::make_error_statement(this->location());
    }
  this->var_->var_value()->flatten_init_expression(gogo, function, inserter);
  return this;
}

// Convert a variable declaration to the backend representation.

Bstatement*
Variable_declaration_statement::do_get_backend(Translate_context* context)
{
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  Variable* var = this->var_->var_value();
  Bvariable* bvar = this->var_->get_backend_variable(context->gogo(),
						     context->function());
  Bexpression* binit = var->get_init(context->gogo(), context->function());

  if (!var->is_in_heap())
    {
      go_assert(binit != NULL);
      return context->backend()->init_statement(bfunction, bvar, binit);
    }

  // Something takes the address of this variable, so the value is
  // stored in the heap.  Initialize it to newly allocated memory
  // space, and assign the initial value to the new space.
  Location loc = this->location();
  Named_object* newfn = context->gogo()->lookup_global("new");
  go_assert(newfn != NULL && newfn->is_function_declaration());
  Expression* func = Expression::make_func_reference(newfn, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(Expression::make_type(var->type(), loc));
  Expression* call = Expression::make_call(func, params, false, loc);
  context->gogo()->lower_expression(context->function(), NULL, &call);
  Temporary_statement* temp = Statement::make_temporary(NULL, call, loc);
  Bstatement* btemp = temp->get_backend(context);

  Bstatement* set = NULL;
  if (binit != NULL)
    {
      Expression* e = Expression::make_temporary_reference(temp, loc);
      e = Expression::make_unary(OPERATOR_MULT, e, loc);
      Bexpression* be = e->get_backend(context);
      set = context->backend()->assignment_statement(bfunction, be, binit, loc);
    }

  Expression* ref = Expression::make_temporary_reference(temp, loc);
  Bexpression* bref = ref->get_backend(context);
  Bstatement* sinit = context->backend()->init_statement(bfunction, bvar, bref);

  std::vector<Bstatement*> stats;
  stats.reserve(3);
  stats.push_back(btemp);
  if (set != NULL)
    stats.push_back(set);
  stats.push_back(sinit);
  return context->backend()->statement_list(stats);
}

// Dump the AST representation for a variable declaration.

void
Variable_declaration_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();

  go_assert(var_->is_variable());
  ast_dump_context->ostream() << "var " << this->var_->name() <<  " ";
  Variable* var = this->var_->var_value();
  if (var->has_type())
    {
      ast_dump_context->dump_type(var->type());
      ast_dump_context->ostream() << " ";
    }
  if (var->init() != NULL)
    {
      ast_dump_context->ostream() <<  "= ";
      ast_dump_context->dump_expression(var->init());
    }
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
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
  Type* type = this->type_ != NULL ? this->type_ : this->init_->type();

  // Temporary variables cannot have a void type.
  if (type->is_void_type())
    {
      go_assert(saw_errors());
      return Type::make_error_type();
    }
  return type;
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
      go_assert(!this->type_->is_abstract());
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
	    go_error_at(this->location(), "incompatible types in assignment");
	  else
	    go_error_at(this->location(), "incompatible types in assignment (%s)",
		     reason.c_str());
	  this->set_is_error();
	}
    }
}

// Flatten a temporary statement: add another temporary when it might
// be needed for interface conversion.

Statement*
Temporary_statement::do_flatten(Gogo*, Named_object*, Block*,
				Statement_inserter* inserter)
{
  if (this->type()->is_error_type()
      || (this->init_ != NULL
          && this->init_->is_error_expression()))
    {
      go_assert(saw_errors());
      return Statement::make_error_statement(this->location());
    }

  if (this->type_ != NULL
      && this->init_ != NULL
      && !Type::are_identical(this->type_, this->init_->type(), false, NULL)
      && this->init_->type()->interface_type() != NULL
      && !this->init_->is_variable())
    {
      Temporary_statement *temp =
	Statement::make_temporary(NULL, this->init_, this->location());
      inserter->insert(temp);
      this->init_ = Expression::make_temporary_reference(temp,
							 this->location());
    }
  return this;
}

// Convert to backend representation.

Bstatement*
Temporary_statement::do_get_backend(Translate_context* context)
{
  go_assert(this->bvariable_ == NULL);

  Named_object* function = context->function();
  go_assert(function != NULL);
  Bfunction* bfunction = function->func_value()->get_decl();
  Btype* btype = this->type()->get_backend(context->gogo());

  Bexpression* binit;
  if (this->init_ == NULL)
    binit = NULL;
  else if (this->type_ == NULL)
    binit = this->init_->get_backend(context);
  else
    {
      Expression* init = Expression::convert_for_assignment(context->gogo(),
							    this->type_,
							    this->init_,
							    this->location());
      binit = init->get_backend(context);
    }

  if (binit != NULL)
    binit = context->backend()->convert_expression(btype, binit,
                                                   this->location());

  Bstatement* statement;
  this->bvariable_ =
    context->backend()->temporary_variable(bfunction, context->bblock(),
					   btype, binit,
					   this->is_address_taken_,
					   this->location(), &statement);
  return statement;
}

// Return the backend variable.

Bvariable*
Temporary_statement::get_backend_variable(Translate_context* context) const
{
  if (this->bvariable_ == NULL)
    {
      go_assert(saw_errors());
      return context->backend()->error_variable();
    }
  return this->bvariable_;
}

// Dump the AST represemtation for a temporary statement

void
Temporary_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_temp_variable_name(this);
  if (this->type_ != NULL)
    {
      ast_dump_context->ostream() << " ";
      ast_dump_context->dump_type(this->type_);
    }
  if (this->init_ != NULL)
    {
      ast_dump_context->ostream() << " = ";
      ast_dump_context->dump_expression(this->init_);
    }
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make and initialize a temporary variable in BLOCK.

Temporary_statement*
Statement::make_temporary(Type* type, Expression* init,
			  Location location)
{
  return new Temporary_statement(type, init, location);
}

// The Move_subexpressions class is used to move all top-level
// subexpressions of an expression.  This is used for things like
// index expressions in which we must evaluate the index value before
// it can be changed by a multiple assignment.

class Move_subexpressions : public Traverse
{
 public:
  Move_subexpressions(int skip, Block* block)
    : Traverse(traverse_expressions),
      skip_(skip), block_(block)
  { }

 protected:
  int
  expression(Expression**);

 private:
  // The number of subexpressions to skip moving.  This is used to
  // avoid moving the array itself, as we only need to move the index.
  int skip_;
  // The block where new temporary variables should be added.
  Block* block_;
};

int
Move_subexpressions::expression(Expression** pexpr)
{
  if (this->skip_ > 0)
    --this->skip_;
  else if ((*pexpr)->temporary_reference_expression() == NULL
	   && !(*pexpr)->is_nil_expression()
           && !(*pexpr)->is_constant())
    {
      Location loc = (*pexpr)->location();
      Temporary_statement* temp = Statement::make_temporary(NULL, *pexpr, loc);
      this->block_->add_statement(temp);
      *pexpr = Expression::make_temporary_reference(temp, loc);
    }
  // We only need to move top-level subexpressions.
  return TRAVERSE_SKIP_COMPONENTS;
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

  int i;
  if ((*pexpr)->must_eval_subexpressions_in_order(&i))
    {
      Move_subexpressions ms(i, this->block_);
      if ((*pexpr)->traverse_subexpressions(&ms) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }

  if ((*pexpr)->must_eval_in_order())
    {
      Call_expression* call = (*pexpr)->call_expression();
      if (call != NULL && call->is_multi_value_arg())
	{
	  // A call expression which returns multiple results as an argument
	  // to another call must be handled specially.  We can't create a
	  // temporary because there is no type to give it.  Instead, group
	  // the caller and this multi-valued call argument and use a temporary
	  // variable to hold them.
	  return TRAVERSE_SKIP_COMPONENTS;
	}

      Location loc = (*pexpr)->location();
      Temporary_statement* temp = Statement::make_temporary(NULL, *pexpr, loc);
      this->block_->add_statement(temp);
      *pexpr = Expression::make_temporary_reference(temp, loc);
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// Class Assignment_statement.

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

// Lower an assignment to a map index expression to a runtime function
// call.

Statement*
Assignment_statement::do_lower(Gogo*, Named_object*, Block* enclosing,
			       Statement_inserter*)
{
  Map_index_expression* mie = this->lhs_->map_index_expression();
  if (mie != NULL)
    {
      Location loc = this->location();

      Expression* map = mie->map();
      Map_type* mt = map->type()->map_type();
      if (mt == NULL)
	{
	  go_assert(saw_errors());
	  return Statement::make_error_statement(loc);
	}

      Block* b = new Block(enclosing, loc);

      // Move out any subexpressions on the left hand side to make
      // sure that functions are called in the required order.
      Move_ordered_evals moe(b);
      mie->traverse_subexpressions(&moe);

      // Copy the key into a temporary so that we can take its address
      // without pushing the value onto the heap.

      // var key_temp KEY_TYPE = MAP_INDEX
      Temporary_statement* key_temp = Statement::make_temporary(mt->key_type(),
								mie->index(),
								loc);
      b->add_statement(key_temp);

      // Copy the value into a temporary to ensure that it is
      // evaluated before we add the key to the map.  This may matter
      // if the value is itself a reference to the map.

      // var val_temp VAL_TYPE = RHS
      Temporary_statement* val_temp = Statement::make_temporary(mt->val_type(),
								this->rhs_,
								loc);
      b->add_statement(val_temp);

      // *mapassign(TYPE, MAP, &key_temp) = RHS
      Expression* a1 = Expression::make_type_descriptor(mt, loc);
      Expression* a2 = mie->map();
      Temporary_reference_expression* ref =
	Expression::make_temporary_reference(key_temp, loc);
      Expression* a3 = Expression::make_unary(OPERATOR_AND, ref, loc);
      Expression* call = Runtime::make_call(Runtime::MAPASSIGN, loc, 3,
					    a1, a2, a3);
      Type* ptrval_type = Type::make_pointer_type(mt->val_type());
      call = Expression::make_cast(ptrval_type, call, loc);
      Expression* indir = Expression::make_unary(OPERATOR_MULT, call, loc);
      ref = Expression::make_temporary_reference(val_temp, loc);
      b->add_statement(Statement::make_assignment(indir, ref, loc));

      return Statement::make_block_statement(b, loc);
    }

  return this;
}

// Set types for the assignment.

void
Assignment_statement::do_determine_types()
{
  this->lhs_->determine_type_no_context();
  Type* rhs_context_type = this->lhs_->type();
  if (rhs_context_type->is_sink_type())
    rhs_context_type = NULL;
  Type_context context(rhs_context_type, false);
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
      if (!this->lhs_->type()->is_error())
	this->report_error(_("invalid left hand side of assignment"));
      return;
    }

  Type* lhs_type = this->lhs_->type();
  Type* rhs_type = this->rhs_->type();

  // Invalid assignment of nil to the blank identifier.
  if (lhs_type->is_sink_type()
      && rhs_type->is_nil_type())
    {
      this->report_error(_("use of untyped nil"));
      return;
    }

  std::string reason;
  if (!Type::are_assignable(lhs_type, rhs_type, &reason))
    {
      if (reason.empty())
	go_error_at(this->location(), "incompatible types in assignment");
      else
	go_error_at(this->location(), "incompatible types in assignment (%s)",
		    reason.c_str());
      this->set_is_error();
    }

  if (lhs_type->is_error() || rhs_type->is_error())
    this->set_is_error();
}

// Flatten an assignment statement.  We may need a temporary for
// interface conversion.

Statement*
Assignment_statement::do_flatten(Gogo*, Named_object*, Block*,
				 Statement_inserter* inserter)
{
  if (this->lhs_->is_error_expression()
      || this->lhs_->type()->is_error_type()
      || this->rhs_->is_error_expression()
      || this->rhs_->type()->is_error_type())
    {
      go_assert(saw_errors());
      return Statement::make_error_statement(this->location());
    }

  if (!this->lhs_->is_sink_expression()
      && !Type::are_identical(this->lhs_->type(), this->rhs_->type(),
			      false, NULL)
      && this->rhs_->type()->interface_type() != NULL
      && !this->rhs_->is_variable())
    {
      Temporary_statement* temp =
	Statement::make_temporary(NULL, this->rhs_, this->location());
      inserter->insert(temp);
      this->rhs_ = Expression::make_temporary_reference(temp,
							this->location());
    }
  return this;
}


// Helper class to locate a root Var_expression within an expression
// tree and mark it as being in an "lvalue" or assignment
// context. Examples:
//
//    x, y = 40, foo(w)
//    x[2] = bar(v)
//    x.z.w[blah(v + u)], y.another = 2, 3
//
// In the code above, vars "x" and "y" appear in lvalue / assignment
// context, whereas the other vars "v", "u", etc are in rvalue context.
//
// Note: at the moment the Var_expression version of "do_copy()"
// defaults to returning the original object, not a new object,
// meaning that a given Var_expression can be referenced from more
// than one place in the tree. This means that when we want to mark a
// Var_expression as having lvalue semantics, we need to make a copy
// of it. Example:
//
//    mystruct.myfield += 42
//
// When this is lowered to eliminate the += operator, we get a tree
//
//    mystruct.myfield = mystruct.field + 42
//
// in which the "mystruct" same Var_expression is referenced on both
// LHS and RHS subtrees. This in turn means that if we try to mark the
// LHS Var_expression the RHS Var_expression will also be marked.  To
// address this issue, the code below clones any var_expression before
// applying an lvalue marking.
//

class Mark_lvalue_varexprs : public Traverse
{
 public:
  Mark_lvalue_varexprs()
    : Traverse(traverse_expressions)
  { }

 protected:
  int
  expression(Expression**);

 private:
};

int Mark_lvalue_varexprs::expression(Expression** ppexpr)
{
  Expression* e = *ppexpr;

  Var_expression* ve = e->var_expression();
  if (ve)
    {
      ve = new Var_expression(ve->named_object(), ve->location());
      ve->set_in_lvalue_pos();
      *ppexpr = ve;
      return TRAVERSE_EXIT;
    }

  Field_reference_expression* fre = e->field_reference_expression();
  if (fre != NULL)
    return TRAVERSE_CONTINUE;

  Array_index_expression* aie = e->array_index_expression();
  if (aie != NULL)
    {
      Mark_lvalue_varexprs mlve;
      aie->set_is_lvalue();
      aie->array()->traverse_subexpressions(&mlve);
      return TRAVERSE_EXIT;
    }

  Unary_expression* ue = e->unary_expression();
  if (ue && ue->op() == OPERATOR_MULT)
    return TRAVERSE_CONTINUE;

  Type_conversion_expression* ce = e->conversion_expression();
  if (ce)
    return TRAVERSE_CONTINUE;

  Temporary_reference_expression* tre =
      e->temporary_reference_expression();
  if (tre)
    {
      tre = new Temporary_reference_expression(tre->statement(),
                                               tre->location());
      *ppexpr = tre;
      tre->set_is_lvalue();
      return TRAVERSE_EXIT;
    }

  return TRAVERSE_EXIT;
}

// Convert an assignment statement to the backend representation.

Bstatement*
Assignment_statement::do_get_backend(Translate_context* context)
{
  if (this->lhs_->is_sink_expression())
    {
      Bexpression* rhs = this->rhs_->get_backend(context);
      Bfunction* bfunction = context->function()->func_value()->get_decl();
      return context->backend()->expression_statement(bfunction, rhs);
    }

  Mark_lvalue_varexprs mlve;
  Expression::traverse(&this->lhs_, &mlve);

  Bexpression* lhs = this->lhs_->get_backend(context);
  Expression* conv =
      Expression::convert_for_assignment(context->gogo(), this->lhs_->type(),
                                         this->rhs_, this->location());
  Bexpression* rhs = conv->get_backend(context);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  return context->backend()->assignment_statement(bfunction, lhs, rhs,
                                                  this->location());
}

// Dump the AST representation for an assignment statement.

void
Assignment_statement::do_dump_statement(Ast_dump_context* ast_dump_context)
    const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(this->lhs_);
  ast_dump_context->ostream() << " = " ;
  ast_dump_context->dump_expression(this->rhs_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make an assignment statement.

Statement*
Statement::make_assignment(Expression* lhs, Expression* rhs,
			   Location location)
{
  return new Assignment_statement(lhs, rhs, location);
}

// An assignment operation statement.

class Assignment_operation_statement : public Statement
{
 public:
  Assignment_operation_statement(Operator op, Expression* lhs, Expression* rhs,
				 Location location)
    : Statement(STATEMENT_ASSIGNMENT_OPERATION, location),
      op_(op), lhs_(lhs), rhs_(rhs)
  { }

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { go_unreachable(); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

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
Assignment_operation_statement::do_lower(Gogo*, Named_object*,
					 Block* enclosing, Statement_inserter*)
{
  Location loc = this->location();

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
      go_unreachable();
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

// Dump the AST representation for an assignment operation statement

void
Assignment_operation_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(this->lhs_);
  ast_dump_context->dump_operator(this->op_);
  ast_dump_context->dump_expression(this->rhs_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make an assignment operation statement.

Statement*
Statement::make_assignment_operation(Operator op, Expression* lhs,
				     Expression* rhs, Location location)
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
			     Location location)
    : Statement(STATEMENT_TUPLE_ASSIGNMENT, location),
      lhs_(lhs), rhs_(rhs)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { go_unreachable(); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

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
Tuple_assignment_statement::do_lower(Gogo*, Named_object*, Block* enclosing,
				     Statement_inserter*)
{
  Location loc = this->location();

  Block* b = new Block(enclosing, loc);

  // First move out any subexpressions on the left hand side.  The
  // right hand side will be evaluated in the required order anyhow.
  Move_ordered_evals moe(b);
  for (Expression_list::iterator plhs = this->lhs_->begin();
       plhs != this->lhs_->end();
       ++plhs)
    Expression::traverse(&*plhs, &moe);

  std::vector<Temporary_statement*> temps;
  temps.reserve(this->lhs_->size());

  Expression_list::const_iterator prhs = this->rhs_->begin();
  for (Expression_list::const_iterator plhs = this->lhs_->begin();
       plhs != this->lhs_->end();
       ++plhs, ++prhs)
    {
      go_assert(prhs != this->rhs_->end());

      if ((*plhs)->is_error_expression()
	  || (*plhs)->type()->is_error()
	  || (*prhs)->is_error_expression()
	  || (*prhs)->type()->is_error())
	continue;

      if ((*plhs)->is_sink_expression())
	{
          if ((*prhs)->type()->is_nil_type())
            this->report_error(_("use of untyped nil"));
          else
            b->add_statement(Statement::make_statement(*prhs, true));
	  continue;
	}

      Temporary_statement* temp = Statement::make_temporary((*plhs)->type(),
							    *prhs, loc);
      b->add_statement(temp);
      temps.push_back(temp);

    }
  go_assert(prhs == this->rhs_->end());

  prhs = this->rhs_->begin();
  std::vector<Temporary_statement*>::const_iterator ptemp = temps.begin();
  for (Expression_list::const_iterator plhs = this->lhs_->begin();
       plhs != this->lhs_->end();
       ++plhs, ++prhs)
    {
      if ((*plhs)->is_error_expression()
	  || (*plhs)->type()->is_error()
	  || (*prhs)->is_error_expression()
	  || (*prhs)->type()->is_error())
	continue;

      if ((*plhs)->is_sink_expression())
	continue;

      Expression* ref = Expression::make_temporary_reference(*ptemp, loc);
      b->add_statement(Statement::make_assignment(*plhs, ref, loc));
      ++ptemp;
    }
  go_assert(ptemp == temps.end() || saw_errors());

  return Statement::make_block_statement(b, loc);
}

// Dump the AST representation for a tuple assignment statement.

void
Tuple_assignment_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression_list(this->lhs_);
  ast_dump_context->ostream() << " = ";
  ast_dump_context->dump_expression_list(this->rhs_);
  ast_dump_context->ostream()  << dsuffix(location()) << std::endl;
}

// Make a tuple assignment statement.

Statement*
Statement::make_tuple_assignment(Expression_list* lhs, Expression_list* rhs,
				 Location location)
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
				 Location location)
    : Statement(STATEMENT_TUPLE_MAP_ASSIGNMENT, location),
      val_(val), present_(present), map_index_(map_index)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { go_unreachable(); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

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
Tuple_map_assignment_statement::do_lower(Gogo* gogo, Named_object*,
					 Block* enclosing, Statement_inserter*)
{
  Location loc = this->location();

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

  // var val_ptr_temp *VAL_TYPE
  Type* val_ptr_type = Type::make_pointer_type(map_type->val_type());
  Temporary_statement* val_ptr_temp = Statement::make_temporary(val_ptr_type,
								NULL, loc);
  b->add_statement(val_ptr_temp);

  // var present_temp bool
  Temporary_statement* present_temp =
    Statement::make_temporary((this->present_->type()->is_sink_type())
			      ? Type::make_boolean_type()
			      : this->present_->type(),
			      NULL, loc);
  b->add_statement(present_temp);

  // val_ptr_temp, present_temp = mapaccess2(DESCRIPTOR, MAP, &key_temp)
  Expression* a1 = Expression::make_type_descriptor(map_type, loc);
  Expression* a2 = map_index->map();
  Temporary_reference_expression* ref =
    Expression::make_temporary_reference(key_temp, loc);
  Expression* a3 = Expression::make_unary(OPERATOR_AND, ref, loc);
  Expression* a4 = map_type->fat_zero_value(gogo);
  Call_expression* call;
  if (a4 == NULL)
    call = Runtime::make_call(Runtime::MAPACCESS2, loc, 3, a1, a2, a3);
  else
    call = Runtime::make_call(Runtime::MAPACCESS2_FAT, loc, 4, a1, a2, a3, a4);
  ref = Expression::make_temporary_reference(val_ptr_temp, loc);
  ref->set_is_lvalue();
  Expression* res = Expression::make_call_result(call, 0);
  res = Expression::make_unsafe_cast(val_ptr_type, res, loc);
  Statement* s = Statement::make_assignment(ref, res, loc);
  b->add_statement(s);
  ref = Expression::make_temporary_reference(present_temp, loc);
  ref->set_is_lvalue();
  res = Expression::make_call_result(call, 1);
  s = Statement::make_assignment(ref, res, loc);
  b->add_statement(s);

  // val = *val__ptr_temp
  ref = Expression::make_temporary_reference(val_ptr_temp, loc);
  Expression* ind = Expression::make_unary(OPERATOR_MULT, ref, loc);
  s = Statement::make_assignment(this->val_, ind, loc);
  b->add_statement(s);

  // present = present_temp
  ref = Expression::make_temporary_reference(present_temp, loc);
  s = Statement::make_assignment(this->present_, ref, loc);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Dump the AST representation for a tuple map assignment statement.

void
Tuple_map_assignment_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(this->val_);
  ast_dump_context->ostream() << ", ";
  ast_dump_context->dump_expression(this->present_);
  ast_dump_context->ostream() << " = ";
  ast_dump_context->dump_expression(this->map_index_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a map assignment statement which returns a pair of values.

Statement*
Statement::make_tuple_map_assignment(Expression* val, Expression* present,
				     Expression* map_index,
				     Location location)
{
  return new Tuple_map_assignment_statement(val, present, map_index, location);
}

// A tuple assignment from a receive statement.

class Tuple_receive_assignment_statement : public Statement
{
 public:
  Tuple_receive_assignment_statement(Expression* val, Expression* closed,
				     Expression* channel, Location location)
    : Statement(STATEMENT_TUPLE_RECEIVE_ASSIGNMENT, location),
      val_(val), closed_(closed), channel_(channel)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { go_unreachable(); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // Lvalue which receives the value from the channel.
  Expression* val_;
  // Lvalue which receives whether the channel is closed.
  Expression* closed_;
  // The channel on which we receive the value.
  Expression* channel_;
};

// Traversal.

int
Tuple_receive_assignment_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->val_) == TRAVERSE_EXIT
      || this->traverse_expression(traverse, &this->closed_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->channel_);
}

// Lower to a function call.

Statement*
Tuple_receive_assignment_statement::do_lower(Gogo*, Named_object*,
					     Block* enclosing,
					     Statement_inserter*)
{
  Location loc = this->location();

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
  this->closed_->traverse_subexpressions(&moe);

  // var val_temp ELEMENT_TYPE
  Temporary_statement* val_temp =
    Statement::make_temporary(channel_type->element_type(), NULL, loc);
  b->add_statement(val_temp);

  // var closed_temp bool
  Temporary_statement* closed_temp =
    Statement::make_temporary((this->closed_->type()->is_sink_type())
			      ? Type::make_boolean_type()
			      : this->closed_->type(),
			      NULL, loc);
  b->add_statement(closed_temp);

  // closed_temp = chanrecv2(channel, &val_temp)
  Temporary_reference_expression* ref =
    Expression::make_temporary_reference(val_temp, loc);
  Expression* p2 = Expression::make_unary(OPERATOR_AND, ref, loc);
  Expression* call = Runtime::make_call(Runtime::CHANRECV2,
					loc, 2, this->channel_, p2);
  ref = Expression::make_temporary_reference(closed_temp, loc);
  ref->set_is_lvalue();
  Statement* s = Statement::make_assignment(ref, call, loc);
  b->add_statement(s);

  // val = val_temp
  ref = Expression::make_temporary_reference(val_temp, loc);
  s = Statement::make_assignment(this->val_, ref, loc);
  b->add_statement(s);

  // closed = closed_temp
  ref = Expression::make_temporary_reference(closed_temp, loc);
  s = Statement::make_assignment(this->closed_, ref, loc);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Dump the AST representation for a tuple receive statement.

void
Tuple_receive_assignment_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(this->val_);
  ast_dump_context->ostream() << ", ";
  ast_dump_context->dump_expression(this->closed_);
  ast_dump_context->ostream() << " <- ";
  ast_dump_context->dump_expression(this->channel_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a nonblocking receive statement.

Statement*
Statement::make_tuple_receive_assignment(Expression* val, Expression* closed,
					 Expression* channel,
					 Location location)
{
  return new Tuple_receive_assignment_statement(val, closed, channel,
						location);
}

// An assignment to a pair of values from a type guard.  This is a
// conditional type guard.  v, ok = i.(type).

class Tuple_type_guard_assignment_statement : public Statement
{
 public:
  Tuple_type_guard_assignment_statement(Expression* val, Expression* ok,
					Expression* expr, Type* type,
					Location location)
    : Statement(STATEMENT_TUPLE_TYPE_GUARD_ASSIGNMENT, location),
      val_(val), ok_(ok), expr_(expr), type_(type)
  { }

 protected:
  int
  do_traverse(Traverse*);

  bool
  do_traverse_assignments(Traverse_assignments*)
  { go_unreachable(); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  Call_expression*
  lower_to_type(Runtime::Function);

  void
  lower_to_object_type(Block*, Runtime::Function);

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
Tuple_type_guard_assignment_statement::do_lower(Gogo*, Named_object*,
						Block* enclosing,
						Statement_inserter*)
{
  Location loc = this->location();

  Type* expr_type = this->expr_->type();
  if (expr_type->interface_type() == NULL)
    {
      if (!expr_type->is_error() && !this->type_->is_error())
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
	call = Runtime::make_call((expr_is_empty
				   ? Runtime::IFACEE2E2
				   : Runtime::IFACEI2E2),
				  loc, 1, this->expr_);
      else
	call = this->lower_to_type(expr_is_empty
				   ? Runtime::IFACEE2I2
				   : Runtime::IFACEI2I2);
    }
  else if (this->type_->points_to() != NULL)
    call = this->lower_to_type(expr_is_empty
			       ? Runtime::IFACEE2T2P
			       : Runtime::IFACEI2T2P);
  else
    {
      this->lower_to_object_type(b,
				 (expr_is_empty
				  ? Runtime::IFACEE2T2
				  : Runtime::IFACEI2T2));
      call = NULL;
    }

  if (call != NULL)
    {
      Expression* res = Expression::make_call_result(call, 0);
      res = Expression::make_unsafe_cast(this->type_, res, loc);
      Statement* s = Statement::make_assignment(this->val_, res, loc);
      b->add_statement(s);

      res = Expression::make_call_result(call, 1);
      s = Statement::make_assignment(this->ok_, res, loc);
      b->add_statement(s);
    }

  return Statement::make_block_statement(b, loc);
}

// Lower a conversion to a non-empty interface type or a pointer type.

Call_expression*
Tuple_type_guard_assignment_statement::lower_to_type(Runtime::Function code)
{
  Location loc = this->location();
  return Runtime::make_call(code, loc, 2,
			    Expression::make_type_descriptor(this->type_, loc),
			    this->expr_);
}

// Lower a conversion to a non-interface non-pointer type.

void
Tuple_type_guard_assignment_statement::lower_to_object_type(
    Block* b,
    Runtime::Function code)
{
  Location loc = this->location();

  // var val_temp TYPE
  Temporary_statement* val_temp = Statement::make_temporary(this->type_,
							    NULL, loc);
  b->add_statement(val_temp);

  // ok = CODE(type_descriptor, expr, &val_temp)
  Expression* p1 = Expression::make_type_descriptor(this->type_, loc);
  Expression* ref = Expression::make_temporary_reference(val_temp, loc);
  Expression* p3 = Expression::make_unary(OPERATOR_AND, ref, loc);
  Expression* call = Runtime::make_call(code, loc, 3, p1, this->expr_, p3);
  Statement* s = Statement::make_assignment(this->ok_, call, loc);
  b->add_statement(s);

  // val = val_temp
  ref = Expression::make_temporary_reference(val_temp, loc);
  s = Statement::make_assignment(this->val_, ref, loc);
  b->add_statement(s);
}

// Dump the AST representation for a tuple type guard statement.

void
Tuple_type_guard_assignment_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(this->val_);
  ast_dump_context->ostream() << ", ";
  ast_dump_context->dump_expression(this->ok_);
  ast_dump_context->ostream() << " = ";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << " . ";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream()  << dsuffix(location()) << std::endl;
}

// Make an assignment from a type guard to a pair of variables.

Statement*
Statement::make_tuple_type_guard_assignment(Expression* val, Expression* ok,
					    Expression* expr, Type* type,
					    Location location)
{
  return new Tuple_type_guard_assignment_statement(val, ok, expr, type,
						   location);
}

// Class Expression_statement.

// Constructor.

Expression_statement::Expression_statement(Expression* expr, bool is_ignored)
  : Statement(STATEMENT_EXPRESSION, expr->location()),
    expr_(expr), is_ignored_(is_ignored)
{
}

// Determine types.

void
Expression_statement::do_determine_types()
{
  this->expr_->determine_type_no_context();
}

// Check the types of an expression statement.  The only check we do
// is to possibly give an error about discarding the value of the
// expression.

void
Expression_statement::do_check_types(Gogo*)
{
  if (!this->is_ignored_)
    this->expr_->discarding_value();
}

// An expression statement is only a terminating statement if it is
// a call to panic.

bool
Expression_statement::do_may_fall_through() const
{
  const Call_expression* call = this->expr_->call_expression();
  if (call != NULL)
    {
      const Expression* fn = call->fn();
      // panic is still an unknown named object.
      const Unknown_expression* ue = fn->unknown_expression();
      if (ue != NULL)
	{
	  Named_object* no = ue->named_object();

          if (no->is_unknown())
            no = no->unknown_value()->real_named_object();
          if (no != NULL)
            {
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
    }
  return true;
}

// Convert to backend representation.

Bstatement*
Expression_statement::do_get_backend(Translate_context* context)
{
  Bexpression* bexpr = this->expr_->get_backend(context);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  return context->backend()->expression_statement(bfunction, bexpr);
}

// Dump the AST representation for an expression statement

void
Expression_statement::do_dump_statement(Ast_dump_context* ast_dump_context)
    const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(expr_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make an expression statement from an Expression.

Statement*
Statement::make_statement(Expression* expr, bool is_ignored)
{
  return new Expression_statement(expr, is_ignored);
}

// Convert a block to the backend representation of a statement.

Bstatement*
Block_statement::do_get_backend(Translate_context* context)
{
  Bblock* bblock = this->block_->get_backend(context);
  return context->backend()->block_statement(bblock);
}

// Dump the AST for a block statement

void
Block_statement::do_dump_statement(Ast_dump_context*) const
{
  // block statement braces are dumped when traversing.
}

// Make a block statement.

Statement*
Statement::make_block_statement(Block* block, Location location)
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
  { go_unreachable(); }

  Statement*
  do_lower(Gogo*, Named_object*, Block*, Statement_inserter*);

  Bstatement*
  do_get_backend(Translate_context*)
  { go_unreachable(); }

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // The l-value to increment or decrement.
  Expression* expr_;
  // Whether to increment or decrement.
  bool is_inc_;
};

// Lower to += or -=.

Statement*
Inc_dec_statement::do_lower(Gogo*, Named_object*, Block*, Statement_inserter*)
{
  Location loc = this->location();
  Expression* oexpr = Expression::make_integer_ul(1, this->expr_->type(), loc);
  Operator op = this->is_inc_ ? OPERATOR_PLUSEQ : OPERATOR_MINUSEQ;
  return Statement::make_assignment_operation(op, this->expr_, oexpr, loc);
}

// Dump the AST representation for a inc/dec statement.

void
Inc_dec_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(expr_);
  ast_dump_context->ostream() << (is_inc_? "++": "--") << dsuffix(location()) << std::endl;
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

// Constructor.

Thunk_statement::Thunk_statement(Statement_classification classification,
				 Call_expression* call,
				 Location location)
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

  // If this calls something that is not a simple function, then we
  // need a thunk.
  Expression* fn = this->call_->call_expression()->fn();
  if (fn->func_expression() == NULL)
    return false;

  // If the function uses a closure, then we need a thunk.  FIXME: We
  // could accept a zero argument function with a closure.
  if (fn->func_expression()->closure() != NULL)
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
  if (!this->call_->discarding_value())
    return;
  Call_expression* ce = this->call_->call_expression();
  if (ce == NULL)
    {
      if (!this->call_->is_error_expression())
	this->report_error("expected call expression");
      return;
    }
}

// The Traverse class used to find and simplify thunk statements.

class Simplify_thunk_traverse : public Traverse
{
 public:
  Simplify_thunk_traverse(Gogo* gogo)
    : Traverse(traverse_functions | traverse_blocks),
      gogo_(gogo), function_(NULL)
  { }

  int
  function(Named_object*);

  int
  block(Block*);

 private:
  // General IR.
  Gogo* gogo_;
  // The function we are traversing.
  Named_object* function_;
};

// Keep track of the current function while looking for thunks.

int
Simplify_thunk_traverse::function(Named_object* no)
{
  go_assert(this->function_ == NULL);
  this->function_ = no;
  int t = no->func_value()->traverse(this);
  this->function_ = NULL;
  if (t == TRAVERSE_EXIT)
    return t;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Look for thunks in a block.

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
  if (stat->simplify_statement(this->gogo_, this->function_, b))
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

// Return true if the thunk function is a constant, which means that
// it does not need to be passed to the thunk routine.

bool
Thunk_statement::is_constant_function() const
{
  Call_expression* ce = this->call_->call_expression();
  Function_type* fntype = ce->get_function_type();
  if (fntype == NULL)
    {
      go_assert(saw_errors());
      return false;
    }
  if (fntype->is_builtin())
    return true;
  Expression* fn = ce->fn();
  if (fn->func_expression() != NULL)
    return fn->func_expression()->closure() == NULL;
  if (fn->interface_field_reference_expression() != NULL)
    return true;
  return false;
}

// Simplify complex thunk statements into simple ones.  A complicated
// thunk statement is one which takes anything other than zero
// parameters or a single pointer parameter.  We rewrite it into code
// which allocates a struct, stores the parameter values into the
// struct, and does a simple go or defer statement which passes the
// struct to a thunk.  The thunk does the real call.

bool
Thunk_statement::simplify_statement(Gogo* gogo, Named_object* function,
				    Block* block)
{
  if (this->classification() == STATEMENT_ERROR)
    return false;
  if (this->call_->is_error_expression())
    return false;

  if (this->classification() == STATEMENT_DEFER)
    {
      // Make sure that the defer stack exists for the function.  We
      // will use when converting this statement to the backend
      // representation, but we want it to exist when we start
      // converting the function.
      function->func_value()->defer_stack(this->location());
    }

  Call_expression* ce = this->call_->call_expression();
  Function_type* fntype = ce->get_function_type();
  if (fntype == NULL)
    {
      go_assert(saw_errors());
      this->set_is_error();
      return false;
    }
  if (this->is_simple(fntype))
    return false;

  Expression* fn = ce->fn();
  Interface_field_reference_expression* interface_method =
    fn->interface_field_reference_expression();

  Location location = this->location();

  bool is_constant_function = this->is_constant_function();
  Temporary_statement* fn_temp = NULL;
  if (!is_constant_function)
    {
      fn_temp = Statement::make_temporary(NULL, fn, location);
      block->insert_statement_before(block->statements()->size() - 1, fn_temp);
      fn = Expression::make_temporary_reference(fn_temp, location);
    }

  std::string thunk_name = Gogo::thunk_name();

  // Build the thunk.
  this->build_thunk(gogo, thunk_name);

  // Generate code to call the thunk.

  // Get the values to store into the struct which is the single
  // argument to the thunk.

  Expression_list* vals = new Expression_list();
  if (!is_constant_function)
    vals->push_back(fn);

  if (interface_method != NULL)
    vals->push_back(interface_method->expr());

  if (ce->args() != NULL)
    {
      for (Expression_list::const_iterator p = ce->args()->begin();
	   p != ce->args()->end();
	   ++p)
	{
	  if ((*p)->is_constant())
	    continue;
	  vals->push_back(*p);
	}
    }

  // Build the struct.
  Expression* constructor =
    Expression::make_struct_composite_literal(this->struct_type_, vals,
					      location);

  // Allocate the initialized struct on the heap.
  constructor = Expression::make_heap_expression(constructor, location);

  // Throw an error if the function is nil.  This is so that for `go
  // nil` we get a backtrace from the go statement, rather than a
  // useless backtrace from the brand new goroutine.
  Expression* param = constructor;
  if (!is_constant_function)
    {
      fn = Expression::make_temporary_reference(fn_temp, location);
      Expression* nil = Expression::make_nil(location);
      Expression* isnil = Expression::make_binary(OPERATOR_EQEQ, fn, nil,
						  location);
      Expression* crash = gogo->runtime_error(RUNTIME_ERROR_GO_NIL, location);
      crash = Expression::make_conditional(isnil, crash,
					   Expression::make_nil(location),
					   location);
      param = Expression::make_compound(crash, constructor, location);
    }

  // Look up the thunk.
  Named_object* named_thunk = gogo->lookup(thunk_name, NULL);
  go_assert(named_thunk != NULL && named_thunk->is_function());

  // Build the call.
  Expression* func = Expression::make_func_reference(named_thunk, NULL,
						     location);
  Expression_list* params = new Expression_list();
  params->push_back(param);
  Call_expression* call = Expression::make_call(func, params, false, location);

  // Build the simple go or defer statement.
  Statement* s;
  if (this->classification() == STATEMENT_GO)
    s = Statement::make_go_statement(call, location);
  else if (this->classification() == STATEMENT_DEFER)
    s = Statement::make_defer_statement(call, location);
  else
    go_unreachable();

  // The current block should end with the go statement.
  go_assert(block->statements()->size() >= 1);
  go_assert(block->statements()->back() == this);
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
  Location location = this->location();

  Struct_field_list* fields = new Struct_field_list();

  Call_expression* ce = this->call_->call_expression();
  Expression* fn = ce->fn();

  if (!this->is_constant_function())
    {
      // The function to call.
      fields->push_back(Struct_field(Typed_identifier("fn", fntype,
						      location)));
    }

  // If this thunk statement calls a method on an interface, we pass
  // the interface object to the thunk.
  Interface_field_reference_expression* interface_method =
    fn->interface_field_reference_expression();
  if (interface_method != NULL)
    {
      Typed_identifier tid("object", interface_method->expr()->type(),
			   location);
      fields->push_back(Struct_field(tid));
    }

  // The predeclared recover function has no argument.  However, we
  // add an argument when building recover thunks.  Handle that here.
  if (ce->is_recover_call())
    {
      fields->push_back(Struct_field(Typed_identifier("can_recover",
						      Type::lookup_bool_type(),
						      location)));
    }

  const Expression_list* args = ce->args();
  if (args != NULL)
    {
      int i = 0;
      for (Expression_list::const_iterator p = args->begin();
	   p != args->end();
	   ++p, ++i)
	{
	  if ((*p)->is_constant())
	    continue;

	  char buf[50];
	  this->thunk_field_param(i, buf, sizeof buf);
	  fields->push_back(Struct_field(Typed_identifier(buf, (*p)->type(),
							  location)));
	}
    }

  Struct_type *st = Type::make_struct_type(fields, location);
  st->set_is_struct_incomparable();
  return st;
}

// Build the thunk we are going to call.  This is a brand new, albeit
// artificial, function.

void
Thunk_statement::build_thunk(Gogo* gogo, const std::string& thunk_name)
{
  Location location = this->location();

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

  gogo->start_block(location);

  // For a defer statement, start with a call to
  // __go_set_defer_retaddr.  */
  Label* retaddr_label = NULL;
  if (may_call_recover)
    {
      retaddr_label = gogo->add_label_reference("retaddr", location, false);
      Expression* arg = Expression::make_label_addr(retaddr_label, location);
      Expression* call = Runtime::make_call(Runtime::SETDEFERRETADDR,
					    location, 1, arg);

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

      function->func_value()->set_calls_defer_retaddr();
    }

  // Get a reference to the parameter.
  Named_object* named_parameter = gogo->lookup(parameter_name, NULL);
  go_assert(named_parameter != NULL && named_parameter->is_variable());

  // Build the call.  Note that the field names are the same as the
  // ones used in build_struct.
  Expression* thunk_parameter = Expression::make_var_reference(named_parameter,
							       location);
  thunk_parameter = Expression::make_unary(OPERATOR_MULT, thunk_parameter,
					   location);

  Interface_field_reference_expression* interface_method =
    ce->fn()->interface_field_reference_expression();

  Expression* func_to_call;
  unsigned int next_index;
  if (this->is_constant_function())
    {
      func_to_call = ce->fn();
      next_index = 0;
    }
  else
    {
      func_to_call = Expression::make_field_reference(thunk_parameter,
						      0, location);
      next_index = 1;
    }

  if (interface_method != NULL)
    {
      // The main program passes the interface object.
      go_assert(next_index == 0);
      Expression* r = Expression::make_field_reference(thunk_parameter, 0,
						       location);
      const std::string& name(interface_method->name());
      func_to_call = Expression::make_interface_field_reference(r, name,
								location);
      next_index = 1;
    }

  Expression_list* call_params = new Expression_list();
  const Struct_field_list* fields = this->struct_type_->fields();
  Struct_field_list::const_iterator p = fields->begin();
  for (unsigned int i = 0; i < next_index; ++i)
    ++p;
  bool is_recover_call = ce->is_recover_call();
  Expression* recover_arg = NULL;

  const Expression_list* args = ce->args();
  if (args != NULL)
    {
      for (Expression_list::const_iterator arg = args->begin();
	   arg != args->end();
	   ++arg)
	{
	  Expression* param;
	  if ((*arg)->is_constant())
	    param = *arg;
	  else
	    {
	      Expression* thunk_param =
		Expression::make_var_reference(named_parameter, location);
	      thunk_param =
		Expression::make_unary(OPERATOR_MULT, thunk_param, location);
	      param = Expression::make_field_reference(thunk_param,
						       next_index,
						       location);
	      ++next_index;
	    }

	  if (!is_recover_call)
	    call_params->push_back(param);
	  else
	    {
	      go_assert(call_params->empty());
	      recover_arg = param;
	    }
	}
    }

  if (call_params->empty())
    {
      delete call_params;
      call_params = NULL;
    }

  Call_expression* call = Expression::make_call(func_to_call, call_params,
						false, location);

  // This call expression was already lowered before entering the
  // thunk statement.  Don't try to lower varargs again, as that will
  // cause confusion for, e.g., method calls which already have a
  // receiver parameter.
  call->set_varargs_are_lowered();

  Statement* call_statement = Statement::make_statement(call, true);

  gogo->add_statement(call_statement);

  // If this is a defer statement, the label comes immediately after
  // the call.
  if (may_call_recover)
    {
      gogo->add_label_definition("retaddr", location);

      Expression_list* vals = new Expression_list();
      vals->push_back(Expression::make_boolean(false, location));
      gogo->add_statement(Statement::make_return_statement(vals, location));
    }

  Block* b = gogo->finish_block(location);

  gogo->add_block(b, location);

  gogo->lower_block(function, b);

  // We already ran the determine_types pass, so we need to run it
  // just for the call statement now.  The other types are known.
  call_statement->determine_types();

  gogo->flatten_block(function, b);

  if (may_call_recover
      || recover_arg != NULL
      || this->classification() == STATEMENT_GO)
    {
      // Dig up the call expression, which may have been changed
      // during lowering.
      go_assert(call_statement->classification() == STATEMENT_EXPRESSION);
      Expression_statement* es =
	static_cast<Expression_statement*>(call_statement);
      Call_expression* ce = es->expr()->call_expression();
      if (ce == NULL)
	go_assert(saw_errors());
      else
	{
	  if (may_call_recover)
	    ce->set_is_deferred();
	  if (this->classification() == STATEMENT_GO)
	    ce->set_is_concurrent();
	  if (recover_arg != NULL)
	    ce->set_recover_arg(recover_arg);
	}
    }

  // That is all the thunk has to do.
  gogo->finish_function(location);
}

// Get the function and argument expressions.

bool
Thunk_statement::get_fn_and_arg(Expression** pfn, Expression** parg)
{
  if (this->call_->is_error_expression())
    return false;

  Call_expression* ce = this->call_->call_expression();

  Expression* fn = ce->fn();
  Func_expression* fe = fn->func_expression();
  go_assert(fe != NULL);
  *pfn = Expression::make_func_code_reference(fe->named_object(),
					      fe->location());

  const Expression_list* args = ce->args();
  if (args == NULL || args->empty())
    *parg = Expression::make_nil(this->location());
  else
    {
      go_assert(args->size() == 1);
      *parg = args->front();
    }

  return true;
}

// Class Go_statement.

Bstatement*
Go_statement::do_get_backend(Translate_context* context)
{
  Expression* fn;
  Expression* arg;
  if (!this->get_fn_and_arg(&fn, &arg))
    return context->backend()->error_statement();

  Expression* call = Runtime::make_call(Runtime::GO, this->location(), 2,
					fn, arg);
  Bexpression* bcall = call->get_backend(context);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  return context->backend()->expression_statement(bfunction, bcall);
}

// Dump the AST representation for go statement.

void
Go_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "go ";
  ast_dump_context->dump_expression(this->call());
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a go statement.

Statement*
Statement::make_go_statement(Call_expression* call, Location location)
{
  return new Go_statement(call, location);
}

// Class Defer_statement.

Bstatement*
Defer_statement::do_get_backend(Translate_context* context)
{
  Expression* fn;
  Expression* arg;
  if (!this->get_fn_and_arg(&fn, &arg))
    return context->backend()->error_statement();

  Location loc = this->location();
  Expression* ds = context->function()->func_value()->defer_stack(loc);

  Expression* call = Runtime::make_call(Runtime::DEFERPROC, loc, 3,
					ds, fn, arg);
  Bexpression* bcall = call->get_backend(context);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  return context->backend()->expression_statement(bfunction, bcall);
}

// Dump the AST representation for defer statement.

void
Defer_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "defer ";
  ast_dump_context->dump_expression(this->call());
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a defer statement.

Statement*
Statement::make_defer_statement(Call_expression* call,
				Location location)
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
// split up the call's results.  If the return statement lists
// explicit values, implement this statement by assigning the values
// to the result variables and change this statement to a naked
// return.  This lets panic/recover work correctly.

Statement*
Return_statement::do_lower(Gogo*, Named_object* function, Block* enclosing,
			   Statement_inserter*)
{
  if (this->is_lowered_)
    return this;

  Expression_list* vals = this->vals_;
  this->vals_ = NULL;
  this->is_lowered_ = true;

  Location loc = this->location();

  size_t vals_count = vals == NULL ? 0 : vals->size();
  Function::Results* results = function->func_value()->result_variables();
  size_t results_count = results == NULL ? 0 : results->size();

  if (vals_count == 0)
    {
      if (results_count > 0 && !function->func_value()->results_are_named())
	{
	  this->report_error(_("not enough arguments to return"));
	  return this;
	}
      return this;
    }

  if (results_count == 0)
    {
      this->report_error(_("return with value in function "
			   "with no return type"));
      return this;
    }

  // If the current function has multiple return values, and we are
  // returning a single call expression, split up the call expression.
  if (results_count > 1
      && vals->size() == 1
      && vals->front()->call_expression() != NULL)
    {
      Call_expression* call = vals->front()->call_expression();
      call->set_expected_result_count(results_count);
      delete vals;
      vals = new Expression_list;
      for (size_t i = 0; i < results_count; ++i)
	vals->push_back(Expression::make_call_result(call, i));
      vals_count = results_count;
    }

  if (vals_count < results_count)
    {
      this->report_error(_("not enough arguments to return"));
      return this;
    }

  if (vals_count > results_count)
    {
      this->report_error(_("too many values in return statement"));
      return this;
    }

  Block* b = new Block(enclosing, loc);

  Expression_list* lhs = new Expression_list();
  Expression_list* rhs = new Expression_list();

  Expression_list::const_iterator pe = vals->begin();
  int i = 1;
  for (Function::Results::const_iterator pr = results->begin();
       pr != results->end();
       ++pr, ++pe, ++i)
    {
      Named_object* rv = *pr;
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
	    go_error_at(e->location(),
			"incompatible type for return value %d", i);
	  else
	    go_error_at(e->location(),
			"incompatible type for return value %d (%s)",
			i, reason.c_str());
	}
    }
  go_assert(lhs->size() == rhs->size());

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

  b->add_statement(this);

  delete vals;

  return Statement::make_block_statement(b, loc);
}

// Convert a return statement to the backend representation.

Bstatement*
Return_statement::do_get_backend(Translate_context* context)
{
  Location loc = this->location();

  Function* function = context->function()->func_value();
  Function::Results* results = function->result_variables();
  std::vector<Bexpression*> retvals;
  if (results != NULL && !results->empty())
    {
      retvals.reserve(results->size());
      for (Function::Results::const_iterator p = results->begin();
	   p != results->end();
	   p++)
	{
	  Expression* vr = Expression::make_var_reference(*p, loc);
	  retvals.push_back(vr->get_backend(context));
	}
    }

  return context->backend()->return_statement(function->get_decl(),
					      retvals, loc);
}

// Dump the AST representation for a return statement.

void
Return_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "return " ;
  ast_dump_context->dump_expression_list(this->vals_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a return statement.

Return_statement*
Statement::make_return_statement(Expression_list* vals,
				 Location location)
{
  return new Return_statement(vals, location);
}

// Make a statement that returns the result of a call expression.

Statement*
Statement::make_return_from_call(Call_expression* call, Location location)
{
  size_t rc = call->result_count();
  if (rc == 0)
    return Statement::make_statement(call, true);
  else
    {
      Expression_list* vals = new Expression_list();
      if (rc == 1)
	vals->push_back(call);
      else
	{
	  for (size_t i = 0; i < rc; ++i)
	    vals->push_back(Expression::make_call_result(call, i));
	}
      return Statement::make_return_statement(vals, location);
    }
}

// A break or continue statement.

class Bc_statement : public Statement
{
 public:
  Bc_statement(bool is_break, Unnamed_label* label, Location location)
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

  Bstatement*
  do_get_backend(Translate_context* context)
  { return this->label_->get_goto(context, this->location()); }

  void
  do_dump_statement(Ast_dump_context*) const;

 private:
  // The label that this branches to.
  Unnamed_label* label_;
  // True if this is "break", false if it is "continue".
  bool is_break_;
};

// Dump the AST representation for a break/continue statement

void
Bc_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << (this->is_break_ ? "break" : "continue");
  if (this->label_ != NULL)
    {
      ast_dump_context->ostream() << " ";
      ast_dump_context->dump_label_name(this->label_);
    }
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a break statement.

Statement*
Statement::make_break_statement(Unnamed_label* label, Location location)
{
  return new Bc_statement(true, label, location);
}

// Make a continue statement.

Statement*
Statement::make_continue_statement(Unnamed_label* label,
				   Location location)
{
  return new Bc_statement(false, label, location);
}

// Class Goto_statement.

int
Goto_statement::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Check types for a label.  There aren't any types per se, but we use
// this to give an error if the label was never defined.

void
Goto_statement::do_check_types(Gogo*)
{
  if (!this->label_->is_defined())
    {
      go_error_at(this->location(), "reference to undefined label %qs",
		  Gogo::message_name(this->label_->name()).c_str());
      this->set_is_error();
    }
}

// Convert the goto statement to the backend representation.

Bstatement*
Goto_statement::do_get_backend(Translate_context* context)
{
  Blabel* blabel = this->label_->get_backend_label(context);
  return context->backend()->goto_statement(blabel, this->location());
}

// Dump the AST representation for a goto statement.

void
Goto_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "goto " << this->label_->name() << dsuffix(location()) << std::endl;
}

// Make a goto statement.

Statement*
Statement::make_goto_statement(Label* label, Location location)
{
  return new Goto_statement(label, location);
}

// Class Goto_unnamed_statement.

int
Goto_unnamed_statement::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Convert the goto unnamed statement to the backend representation.

Bstatement*
Goto_unnamed_statement::do_get_backend(Translate_context* context)
{
  return this->label_->get_goto(context, this->location());
}

// Dump the AST representation for an unnamed goto statement

void
Goto_unnamed_statement::do_dump_statement(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "goto ";
  ast_dump_context->dump_label_name(this->label_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a goto statement to an unnamed label.

Statement*
Statement::make_goto_unnamed_statement(Unnamed_label* label,
				       Location location)
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

// Return the backend representation of the statement defining this
// label.

Bstatement*
Label_statement::do_get_backend(Translate_context* context)
{
  if (this->label_->is_dummy_label())
    {
      Bexpression* bce = context->backend()->boolean_constant_expression(false);
      Bfunction* bfunction = context->function()->func_value()->get_decl();
      return context->backend()->expression_statement(bfunction, bce);
    }
  Blabel* blabel = this->label_->get_backend_label(context);
  return context->backend()->label_definition_statement(blabel);
}

// Dump the AST for a label definition statement.

void
Label_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << this->label_->name() << ":" << dsuffix(location()) << std::endl;
}

// Make a label statement.

Statement*
Statement::make_label_statement(Label* label, Location location)
{
  return new Label_statement(label, location);
}

// Class Unnamed_label_statement.

Unnamed_label_statement::Unnamed_label_statement(Unnamed_label* label)
  : Statement(STATEMENT_UNNAMED_LABEL, label->location()),
    label_(label)
{ }

int
Unnamed_label_statement::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Get the backend definition for this unnamed label statement.

Bstatement*
Unnamed_label_statement::do_get_backend(Translate_context* context)
{
  return this->label_->get_definition(context);
}

// Dump the AST representation for an unnamed label definition statement.

void
Unnamed_label_statement::do_dump_statement(Ast_dump_context* ast_dump_context)
    const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_label_name(this->label_);
  ast_dump_context->ostream() << ":" << dsuffix(location()) << std::endl;
}

// Make an unnamed label statement.

Statement*
Statement::make_unnamed_label_statement(Unnamed_label* label)
{
  return new Unnamed_label_statement(label);
}

// Class If_statement.

// Traversal.

int
If_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->cond_) == TRAVERSE_EXIT
      || this->then_block_->traverse(traverse) == TRAVERSE_EXIT)
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
  Type_context context(Type::lookup_bool_type(), false);
  this->cond_->determine_type(&context);
  this->then_block_->determine_types();
  if (this->else_block_ != NULL)
    this->else_block_->determine_types();
}

// Check types.

void
If_statement::do_check_types(Gogo*)
{
  Type* type = this->cond_->type();
  if (type->is_error())
    this->set_is_error();
  else if (!type->is_boolean_type())
    this->report_error(_("expected boolean expression"));
}

// Whether the overall statement may fall through.

bool
If_statement::do_may_fall_through() const
{
  return (this->else_block_ == NULL
	  || this->then_block_->may_fall_through()
	  || this->else_block_->may_fall_through());
}

// Get the backend representation.

Bstatement*
If_statement::do_get_backend(Translate_context* context)
{
  go_assert(this->cond_->type()->is_boolean_type()
	     || this->cond_->type()->is_error());
  Bexpression* cond = this->cond_->get_backend(context);
  Bblock* then_block = this->then_block_->get_backend(context);
  Bblock* else_block = (this->else_block_ == NULL
			? NULL
			: this->else_block_->get_backend(context));
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  return context->backend()->if_statement(bfunction,
                                          cond, then_block, else_block,
					  this->location());
}

// Dump the AST representation for an if statement

void
If_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "if ";
  ast_dump_context->dump_expression(this->cond_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->dump_block(this->then_block_);
      if (this->else_block_ != NULL)
	{
	  ast_dump_context->print_indent();
	  ast_dump_context->ostream() << "else" << std::endl;
	  ast_dump_context->dump_block(this->else_block_);
	}
    }
}

// Make an if statement.

Statement*
Statement::make_if_statement(Expression* cond, Block* then_block,
			     Block* else_block, Location location)
{
  return new If_statement(cond, then_block, else_block, location);
}

// Class Case_clauses::Hash_integer_value.

class Case_clauses::Hash_integer_value
{
 public:
  size_t
  operator()(Expression*) const;
};

size_t
Case_clauses::Hash_integer_value::operator()(Expression* pe) const
{
  Numeric_constant nc;
  mpz_t ival;
  if (!pe->numeric_constant_value(&nc) || !nc.to_int(&ival))
    go_unreachable();
  size_t ret = mpz_get_ui(ival);
  mpz_clear(ival);
  return ret;
}

// Class Case_clauses::Eq_integer_value.

class Case_clauses::Eq_integer_value
{
 public:
  bool
  operator()(Expression*, Expression*) const;
};

bool
Case_clauses::Eq_integer_value::operator()(Expression* a, Expression* b) const
{
  Numeric_constant anc;
  mpz_t aval;
  Numeric_constant bnc;
  mpz_t bval;
  if (!a->numeric_constant_value(&anc)
      || !anc.to_int(&aval)
      || !b->numeric_constant_value(&bnc)
      || !bnc.to_int(&bval))
    go_unreachable();
  bool ret = mpz_cmp(aval, bval) == 0;
  mpz_clear(aval);
  mpz_clear(bval);
  return ret;
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
  Location loc = this->location_;
  Unnamed_label* next_case_label;
  if (this->cases_ == NULL || this->cases_->empty())
    {
      go_assert(this->is_default_);
      next_case_label = NULL;
    }
  else
    {
      Expression* cond = NULL;

      for (Expression_list::const_iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	{
	  Expression* ref = Expression::make_temporary_reference(val_temp,
								 loc);
	  Expression* this_cond = Expression::make_binary(OPERATOR_EQEQ, ref,
							  *p, loc);
	  if (cond == NULL)
	    cond = this_cond;
	  else
	    cond = Expression::make_binary(OPERATOR_OROR, cond, this_cond, loc);
	}

      Block* then_block = new Block(b, loc);
      next_case_label = new Unnamed_label(Linemap::unknown_location());
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
	      go_error_at((*p)->location(),
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

// Convert the case values and statements to the backend
// representation.  BREAK_LABEL is the label which break statements
// should branch to.  CASE_CONSTANTS is used to detect duplicate
// constants.  *CASES should be passed as an empty vector; the values
// for this case will be added to it.  If this is the default case,
// *CASES will remain empty.  This returns the statement to execute if
// one of these cases is selected.

Bstatement*
Case_clauses::Case_clause::get_backend(Translate_context* context,
				       Unnamed_label* break_label,
				       Case_constants* case_constants,
				       std::vector<Bexpression*>* cases) const
{
  if (this->cases_ != NULL)
    {
      go_assert(!this->is_default_);
      for (Expression_list::const_iterator p = this->cases_->begin();
	   p != this->cases_->end();
	   ++p)
	{
	  Expression* e = *p;
	  if (e->classification() != Expression::EXPRESSION_INTEGER)
	    {
	      Numeric_constant nc;
	      mpz_t ival;
	      if (!(*p)->numeric_constant_value(&nc) || !nc.to_int(&ival))
		{
		  // Something went wrong.  This can happen with a
		  // negative constant and an unsigned switch value.
		  go_assert(saw_errors());
		  continue;
		}
	      go_assert(nc.type() != NULL);
	      e = Expression::make_integer_z(&ival, nc.type(), e->location());
	      mpz_clear(ival);
	    }

	  std::pair<Case_constants::iterator, bool> ins =
	    case_constants->insert(e);
	  if (!ins.second)
	    {
	      // Value was already present.
	      go_error_at(this->location_, "duplicate case in switch");
	      e = Expression::make_error(this->location_);
	    }
	  cases->push_back(e->get_backend(context));
	}
    }

  Bstatement* statements;
  if (this->statements_ == NULL)
    statements = NULL;
  else
    {
      Bblock* bblock = this->statements_->get_backend(context);
      statements = context->backend()->block_statement(bblock);
    }

  Bstatement* break_stat;
  if (this->is_fallthrough_)
    break_stat = NULL;
  else
    break_stat = break_label->get_goto(context, this->location_);

  if (statements == NULL)
    return break_stat;
  else if (break_stat == NULL)
    return statements;
  else
    return context->backend()->compound_statement(statements, break_stat);
}

// Dump the AST representation for a case clause

void
Case_clauses::Case_clause::dump_clause(Ast_dump_context* ast_dump_context)
    const
{
  ast_dump_context->print_indent();
  if (this->is_default_)
    {
      ast_dump_context->ostream() << "default:";
    }
  else
    {
      ast_dump_context->ostream() << "case ";
      ast_dump_context->dump_expression_list(this->cases_);
      ast_dump_context->ostream() << ":" ;
    }
  ast_dump_context->dump_block(this->statements_);
  if (this->is_fallthrough_)
    {
      ast_dump_context->print_indent();
      ast_dump_context->ostream() <<  " (fallthrough)" << dsuffix(location()) << std::endl;
    }
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

// Convert the cases to the backend representation.  This sets
// *ALL_CASES and *ALL_STATEMENTS.

void
Case_clauses::get_backend(Translate_context* context,
			  Unnamed_label* break_label,
			  std::vector<std::vector<Bexpression*> >* all_cases,
			  std::vector<Bstatement*>* all_statements) const
{
  Case_constants case_constants;

  size_t c = this->clauses_.size();
  all_cases->resize(c);
  all_statements->resize(c);

  size_t i = 0;
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p, ++i)
    {
      std::vector<Bexpression*> cases;
      Bstatement* stat = p->get_backend(context, break_label, &case_constants,
					&cases);
      // The final clause can't fall through.
      if (i == c - 1 && p->is_fallthrough())
        {
          go_assert(saw_errors());
          stat = context->backend()->error_statement();
        }
      (*all_cases)[i].swap(cases);
      (*all_statements)[i] = stat;
    }
}

// Dump the AST representation for case clauses (from a switch statement)

void
Case_clauses::dump_clauses(Ast_dump_context* ast_dump_context) const
{
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->dump_clause(ast_dump_context);
}

// A constant switch statement.  A Switch_statement is lowered to this
// when all the cases are constants.

class Constant_switch_statement : public Statement
{
 public:
  Constant_switch_statement(Expression* val, Case_clauses* clauses,
			    Unnamed_label* break_label,
			    Location location)
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

  Bstatement*
  do_get_backend(Translate_context*);

  void
  do_dump_statement(Ast_dump_context*) const;

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

// Convert to GENERIC.

Bstatement*
Constant_switch_statement::do_get_backend(Translate_context* context)
{
  Bexpression* switch_val_expr = this->val_->get_backend(context);

  Unnamed_label* break_label = this->break_label_;
  if (break_label == NULL)
    break_label = new Unnamed_label(this->location());

  std::vector<std::vector<Bexpression*> > all_cases;
  std::vector<Bstatement*> all_statements;
  this->clauses_->get_backend(context, break_label, &all_cases,
			      &all_statements);

  Bfunction* bfunction = context->function()->func_value()->get_decl();
  Bstatement* switch_statement;
  switch_statement = context->backend()->switch_statement(bfunction,
							  switch_val_expr,
							  all_cases,
							  all_statements,
							  this->location());
  Bstatement* ldef = break_label->get_definition(context);
  return context->backend()->compound_statement(switch_statement, ldef);
}

// Dump the AST representation for a constant switch statement.

void
Constant_switch_statement::do_dump_statement(Ast_dump_context* ast_dump_context)
    const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "switch ";
  ast_dump_context->dump_expression(this->val_);

  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->ostream() << " {" << std::endl;
      this->clauses_->dump_clauses(ast_dump_context);
      ast_dump_context->ostream() << "}";
    }

   ast_dump_context->ostream() << std::endl;
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
Switch_statement::do_lower(Gogo*, Named_object*, Block* enclosing,
			   Statement_inserter*)
{
  Location loc = this->location();

  if (this->val_ != NULL
      && (this->val_->is_error_expression()
	  || this->val_->type()->is_error()))
    {
      go_assert(saw_errors());
      return Statement::make_error_statement(loc);
    }

  if (this->val_ != NULL
      && this->val_->type()->integer_type() != NULL
      && !this->clauses_->empty()
      && this->clauses_->is_constant())
    return new Constant_switch_statement(this->val_, this->clauses_,
					 this->break_label_, loc);

  if (this->val_ != NULL
      && !this->val_->type()->is_comparable()
      && !Type::are_compatible_for_comparison(true, this->val_->type(),
					      Type::make_nil_type(), NULL))
    {
      go_error_at(this->val_->location(),
		  "cannot switch on value whose type that may not be compared");
      return Statement::make_error_statement(loc);
    }

  Block* b = new Block(enclosing, loc);

  if (this->clauses_->empty())
    {
      Expression* val = this->val_;
      if (val == NULL)
	val = Expression::make_boolean(true, loc);
      return Statement::make_statement(val, true);
    }

  // var val_temp VAL_TYPE = VAL
  Expression* val = this->val_;
  if (val == NULL)
    val = Expression::make_boolean(true, loc);

  Type* type = val->type();
  if (type->is_abstract())
    type = type->make_non_abstract_type();
  Temporary_statement* val_temp = Statement::make_temporary(type, val, loc);
  b->add_statement(val_temp);

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

// Dump the AST representation for a switch statement.

void
Switch_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "switch ";
  if (this->val_ != NULL)
    {
      ast_dump_context->dump_expression(this->val_);
    }
  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->ostream() << " {" << dsuffix(location()) << std::endl;
      this->clauses_->dump_clauses(ast_dump_context);
      ast_dump_context->print_indent();
      ast_dump_context->ostream() << "}";
    }
  ast_dump_context->ostream() << std::endl;
}

// Return whether this switch may fall through.

bool
Switch_statement::do_may_fall_through() const
{
  if (this->clauses_ == NULL)
    return true;

  // If we have a break label, then some case needed it.  That implies
  // that the switch statement as a whole can fall through.
  if (this->break_label_ != NULL)
    return true;

  return this->clauses_->may_fall_through();
}

// Make a switch statement.

Switch_statement*
Statement::make_switch_statement(Expression* val, Location location)
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
Type_case_clauses::Type_case_clause::lower(Type* switch_val_type,
					   Block* b,
					   Temporary_statement* descriptor_temp,
					   Unnamed_label* break_label,
					   Unnamed_label** stmts_label) const
{
  Location loc = this->location_;

  Unnamed_label* next_case_label = NULL;
  if (!this->is_default_)
    {
      Type* type = this->type_;

      std::string reason;
      if (switch_val_type->interface_type() != NULL
	  && !type->is_nil_constant_as_type()
	  && type->interface_type() == NULL
	  && !switch_val_type->interface_type()->implements_interface(type,
								      &reason))
	{
	  if (reason.empty())
	    go_error_at(this->location_, "impossible type switch case");
	  else
	    go_error_at(this->location_, "impossible type switch case (%s)",
			reason.c_str());
	}

      Expression* ref = Expression::make_temporary_reference(descriptor_temp,
							     loc);

      Expression* cond;
      // The language permits case nil, which is of course a constant
      // rather than a type.  It will appear here as an invalid
      // forwarding type.
      if (type->is_nil_constant_as_type())
	cond = Expression::make_binary(OPERATOR_EQEQ, ref,
				       Expression::make_nil(loc),
				       loc);
      else
	cond = Runtime::make_call((type->interface_type() == NULL
				   ? Runtime::IFACETYPEEQ
				   : Runtime::IFACET2IP),
				  loc, 2,
				  Expression::make_type_descriptor(type, loc),
				  ref);

      Unnamed_label* dest;
      if (!this->is_fallthrough_)
	{
	  // if !COND { goto NEXT_CASE_LABEL }
	  next_case_label = new Unnamed_label(Linemap::unknown_location());
	  dest = next_case_label;
	  cond = Expression::make_unary(OPERATOR_NOT, cond, loc);
	}
      else
	{
	  // if COND { goto STMTS_LABEL }
	  go_assert(stmts_label != NULL);
	  if (*stmts_label == NULL)
	    *stmts_label = new Unnamed_label(Linemap::unknown_location());
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
      go_assert(!this->is_fallthrough_);
      if (stmts_label != NULL && *stmts_label != NULL)
	{
	  go_assert(!this->is_default_);
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
    go_assert(next_case_label == NULL);
  else
    {
      Location gloc = (this->statements_ == NULL
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

// Return true if this type clause may fall through to the statements
// following the switch.

bool
Type_case_clauses::Type_case_clause::may_fall_through() const
{
  if (this->is_fallthrough_)
    {
      // This case means that we automatically fall through to the
      // next case (it's used for T1 in case T1, T2:).  It does not
      // mean that we fall through to the end of the type switch as a
      // whole.  There is sure to be a next case and that next case
      // will determine whether we fall through to the statements
      // after the type switch.
      return false;
    }
  if (this->statements_ == NULL)
    return true;
  return this->statements_->may_fall_through();
}

// Dump the AST representation for a type case clause

void
Type_case_clauses::Type_case_clause::dump_clause(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  if (this->is_default_)
    {
      ast_dump_context->ostream() << "default:";
    }
  else
    {
      ast_dump_context->ostream() << "case ";
      ast_dump_context->dump_type(this->type_);
      ast_dump_context->ostream() << ":" ;
    }
  ast_dump_context->dump_block(this->statements_);
  if (this->is_fallthrough_)
    {
      ast_dump_context->print_indent();
      ast_dump_context->ostream() <<  " (fallthrough)" << std::endl;
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
	go_error_at(p->location(), "duplicate type in switch");
    }
}

// Lower the clauses in a type switch.  Add statements to the block B.
// The type descriptor we are switching on is in DESCRIPTOR_TEMP.
// BREAK_LABEL is the label at the end of the type switch.

void
Type_case_clauses::lower(Type* switch_val_type, Block* b,
			 Temporary_statement* descriptor_temp,
			 Unnamed_label* break_label) const
{
  const Type_case_clause* default_case = NULL;

  Unnamed_label* stmts_label = NULL;
  for (Type_clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (!p->is_default())
	p->lower(switch_val_type, b, descriptor_temp, break_label,
		 &stmts_label);
      else
	{
	  // We are generating a series of tests, which means that we
	  // need to move the default case to the end.
	  default_case = &*p;
	}
    }
  go_assert(stmts_label == NULL);

  if (default_case != NULL)
    default_case->lower(switch_val_type, b, descriptor_temp, break_label,
			NULL);
}

// Return true if these clauses may fall through to the statements
// following the switch statement.

bool
Type_case_clauses::may_fall_through() const
{
  bool found_default = false;
  for (Type_clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    {
      if (p->may_fall_through())
	return true;
      if (p->is_default())
	found_default = true;
    }
  return !found_default;
}

// Dump the AST representation for case clauses (from a switch statement)

void
Type_case_clauses::dump_clauses(Ast_dump_context* ast_dump_context) const
{
  for (Type_clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->dump_clause(ast_dump_context);
}

// Class Type_switch_statement.

// Traversal.

int
Type_switch_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->expr_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
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
Type_switch_statement::do_lower(Gogo*, Named_object*, Block* enclosing,
				Statement_inserter*)
{
  const Location loc = this->location();

  if (this->clauses_ != NULL)
    this->clauses_->check_duplicates();

  Block* b = new Block(enclosing, loc);

  Type* val_type = this->expr_->type();
  if (val_type->interface_type() == NULL)
    {
      if (!val_type->is_error())
	this->report_error(_("cannot type switch on non-interface value"));
      return Statement::make_error_statement(loc);
    }

  // var descriptor_temp DESCRIPTOR_TYPE
  Type* descriptor_type = Type::make_type_descriptor_ptr_type();
  Temporary_statement* descriptor_temp =
    Statement::make_temporary(descriptor_type, NULL, loc);
  b->add_statement(descriptor_temp);

  // descriptor_temp = ifacetype(val_temp) FIXME: This should be
  // inlined.
  bool is_empty = val_type->interface_type()->is_empty();
  Expression* call = Runtime::make_call((is_empty
					 ? Runtime::EFACETYPE
					 : Runtime::IFACETYPE),
					loc, 1, this->expr_);
  Temporary_reference_expression* lhs =
    Expression::make_temporary_reference(descriptor_temp, loc);
  lhs->set_is_lvalue();
  Statement* s = Statement::make_assignment(lhs, call, loc);
  b->add_statement(s);

  if (this->clauses_ != NULL)
    this->clauses_->lower(val_type, b, descriptor_temp, this->break_label());

  s = Statement::make_unnamed_label_statement(this->break_label_);
  b->add_statement(s);

  return Statement::make_block_statement(b, loc);
}

// Return whether this switch may fall through.

bool
Type_switch_statement::do_may_fall_through() const
{
  if (this->clauses_ == NULL)
    return true;

  // If we have a break label, then some case needed it.  That implies
  // that the switch statement as a whole can fall through.
  if (this->break_label_ != NULL)
    return true;

  return this->clauses_->may_fall_through();
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

// Dump the AST representation for a type switch statement

void
Type_switch_statement::do_dump_statement(Ast_dump_context* ast_dump_context)
    const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "switch ";
  if (!this->name_.empty())
    ast_dump_context->ostream() << this->name_ << " = ";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << " .(type)";
  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->ostream() << " {" << dsuffix(location()) << std::endl;
      this->clauses_->dump_clauses(ast_dump_context);
      ast_dump_context->ostream() << "}";
    }
  ast_dump_context->ostream() << std::endl;
}

// Make a type switch statement.

Type_switch_statement*
Statement::make_type_switch_statement(const std::string& name, Expression* expr,
				      Location location)
{
  return new Type_switch_statement(name, expr, location);
}

// Class Send_statement.

// Traversal.

int
Send_statement::do_traverse(Traverse* traverse)
{
  if (this->traverse_expression(traverse, &this->channel_) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return this->traverse_expression(traverse, &this->val_);
}

// Determine types.

void
Send_statement::do_determine_types()
{
  this->channel_->determine_type_no_context();
  Type* type = this->channel_->type();
  Type_context context;
  if (type->channel_type() != NULL)
    context.type = type->channel_type()->element_type();
  this->val_->determine_type(&context);
}

// Check types.

void
Send_statement::do_check_types(Gogo*)
{
  Type* type = this->channel_->type();
  if (type->is_error())
    {
      this->set_is_error();
      return;
    }
  Channel_type* channel_type = type->channel_type();
  if (channel_type == NULL)
    {
      go_error_at(this->location(), "left operand of %<<-%> must be channel");
      this->set_is_error();
      return;
    }
  Type* element_type = channel_type->element_type();
  if (!Type::are_assignable(element_type, this->val_->type(), NULL))
    {
      this->report_error(_("incompatible types in send"));
      return;
    }
  if (!channel_type->may_send())
    {
      this->report_error(_("invalid send on receive-only channel"));
      return;
    }
}

// Flatten a send statement.  We may need a temporary for interface
// conversion.

Statement*
Send_statement::do_flatten(Gogo*, Named_object*, Block*,
			   Statement_inserter* inserter)
{
  if (this->channel_->is_error_expression()
      || this->channel_->type()->is_error_type())
    {
      go_assert(saw_errors());
      return Statement::make_error_statement(this->location());
    }

  Type* element_type = this->channel_->type()->channel_type()->element_type();
  if (!Type::are_identical(element_type, this->val_->type(), false, NULL)
      && this->val_->type()->interface_type() != NULL
      && !this->val_->is_variable())
    {
      Temporary_statement* temp =
	Statement::make_temporary(NULL, this->val_, this->location());
      inserter->insert(temp);
      this->val_ = Expression::make_temporary_reference(temp,
							this->location());
    }
  return this;
}

// Convert a send statement to the backend representation.

Bstatement*
Send_statement::do_get_backend(Translate_context* context)
{
  Location loc = this->location();

  Channel_type* channel_type = this->channel_->type()->channel_type();
  Type* element_type = channel_type->element_type();
  Expression* val = Expression::convert_for_assignment(context->gogo(),
						       element_type,
						       this->val_, loc);

  bool can_take_address;
  switch (element_type->base()->classification())
    {
    case Type::TYPE_BOOLEAN:
    case Type::TYPE_INTEGER:
    case Type::TYPE_FUNCTION:
    case Type::TYPE_POINTER:
    case Type::TYPE_MAP:
    case Type::TYPE_CHANNEL:
    case Type::TYPE_FLOAT:
    case Type::TYPE_COMPLEX:
    case Type::TYPE_STRING:
    case Type::TYPE_INTERFACE:
      can_take_address = false;
      break;

    case Type::TYPE_STRUCT:
      can_take_address = true;
      break;

    case Type::TYPE_ARRAY:
      can_take_address = !element_type->is_slice_type();
      break;

    default:
    case Type::TYPE_ERROR:
    case Type::TYPE_VOID:
    case Type::TYPE_SINK:
    case Type::TYPE_NIL:
    case Type::TYPE_NAMED:
    case Type::TYPE_FORWARD:
      go_assert(saw_errors());
      return context->backend()->error_statement();
    }

  // Only try to take the address of a variable.  We have already
  // moved variables to the heap, so this should not cause that to
  // happen unnecessarily.
  if (can_take_address
      && val->var_expression() == NULL
      && val->temporary_reference_expression() == NULL)
    can_take_address = false;

  Bstatement* btemp = NULL;
  if (can_take_address)
    {
      // The function doesn't change the value, so just take its
      // address directly.
      val = Expression::make_unary(OPERATOR_AND, val, loc);
    }
  else
    {
      // The value is not in a variable, or is small enough that it
      // might be in a register, and taking the address would push it
      // on the stack.  Copy it into a temporary variable to take the
      // address.
      Temporary_statement* temp = Statement::make_temporary(element_type,
							    val, loc);
      Expression* ref = Expression::make_temporary_reference(temp, loc);
      val = Expression::make_unary(OPERATOR_AND, ref, loc);
      btemp = temp->get_backend(context);
    }

  Expression* call = Runtime::make_call(Runtime::CHANSEND, loc, 2,
					this->channel_, val);

  context->gogo()->lower_expression(context->function(), NULL, &call);
  Bexpression* bcall = call->get_backend(context);
  Bfunction* bfunction = context->function()->func_value()->get_decl();
  Bstatement* s = context->backend()->expression_statement(bfunction, bcall);

  if (btemp == NULL)
    return s;
  else
    return context->backend()->compound_statement(btemp, s);
}

// Dump the AST representation for a send statement

void
Send_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->dump_expression(this->channel_);
  ast_dump_context->ostream() << " <- ";
  ast_dump_context->dump_expression(this->val_);
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a send statement.

Send_statement*
Statement::make_send_statement(Expression* channel, Expression* val,
			       Location location)
{
  return new Send_statement(channel, val, location);
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
      if (this->closed_ != NULL)
	{
	  if (Expression::traverse(&this->closed_, traverse) == TRAVERSE_EXIT)
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

// Lowering.  We call a function to register this clause, and arrange
// to set any variables in any receive clause.

void
Select_clauses::Select_clause::lower(Gogo* gogo, Named_object* function,
				     Block* b, Temporary_statement* sel)
{
  Location loc = this->location_;

  Expression* selref = Expression::make_temporary_reference(sel, loc);
  selref = Expression::make_unary(OPERATOR_AND, selref, loc);

  if (this->is_default_)
    {
      go_assert(this->channel_ == NULL && this->val_ == NULL);
      this->lower_default(b, selref);
      this->is_lowered_ = true;
      return;
    }

  // Evaluate the channel before the select statement.
  Temporary_statement* channel_temp = Statement::make_temporary(NULL,
								this->channel_,
								loc);
  b->add_statement(channel_temp);
  Expression* chanref = Expression::make_temporary_reference(channel_temp,
							     loc);

  if (this->is_send_)
    this->lower_send(b, selref, chanref);
  else
    this->lower_recv(gogo, function, b, selref, chanref);

  // Now all references should be handled through the statements, not
  // through here.
  this->is_lowered_ = true;
  this->val_ = NULL;
}

// Lower a default clause in a select statement.

void
Select_clauses::Select_clause::lower_default(Block* b, Expression* selref)
{
  Location loc = this->location_;
  Expression* call = Runtime::make_call(Runtime::SELECTDEFAULT, loc, 1,
					selref);
  b->add_statement(Statement::make_statement(call, true));
}

// Lower a send clause in a select statement.

void
Select_clauses::Select_clause::lower_send(Block* b, Expression* selref,
					  Expression* chanref)
{
  Location loc = this->location_;

  Channel_type* ct = this->channel_->type()->channel_type();
  if (ct == NULL)
    return;

  Type* valtype = ct->element_type();

  // Note that copying the value to a temporary here means that we
  // evaluate the send values in the required order.
  Temporary_statement* val = Statement::make_temporary(valtype, this->val_,
						       loc);
  b->add_statement(val);

  Expression* valref = Expression::make_temporary_reference(val, loc);
  Expression* valaddr = Expression::make_unary(OPERATOR_AND, valref, loc);

  Expression* call = Runtime::make_call(Runtime::SELECTSEND, loc, 3, selref,
					chanref, valaddr);
  b->add_statement(Statement::make_statement(call, true));
}

// Lower a receive clause in a select statement.

void
Select_clauses::Select_clause::lower_recv(Gogo* gogo, Named_object* function,
					  Block* b, Expression* selref,
					  Expression* chanref)
{
  Location loc = this->location_;

  Channel_type* ct = this->channel_->type()->channel_type();
  if (ct == NULL)
    return;

  Type* valtype = ct->element_type();
  Temporary_statement* val = Statement::make_temporary(valtype, NULL, loc);
  b->add_statement(val);

  Expression* valref = Expression::make_temporary_reference(val, loc);
  Expression* valaddr = Expression::make_unary(OPERATOR_AND, valref, loc);

  Temporary_statement* closed_temp = NULL;

  Expression* caddr;
  if (this->closed_ == NULL && this->closedvar_ == NULL)
    caddr = Expression::make_nil(loc);
  else
    {
      closed_temp = Statement::make_temporary(Type::lookup_bool_type(), NULL,
					      loc);
      b->add_statement(closed_temp);
      Expression* cref = Expression::make_temporary_reference(closed_temp,
							      loc);
      caddr = Expression::make_unary(OPERATOR_AND, cref, loc);
    }

  Expression* call = Runtime::make_call(Runtime::SELECTRECV, loc, 4, selref,
					chanref, valaddr, caddr);

  b->add_statement(Statement::make_statement(call, true));

  // If the block of statements is executed, arrange for the received
  // value to move from VAL to the place where the statements expect
  // it.

  Block* init = NULL;

  if (this->var_ != NULL)
    {
      go_assert(this->val_ == NULL);
      valref = Expression::make_temporary_reference(val, loc);
      this->var_->var_value()->set_init(valref);
      this->var_->var_value()->clear_type_from_chan_element();
    }
  else if (this->val_ != NULL && !this->val_->is_sink_expression())
    {
      init = new Block(b, loc);
      valref = Expression::make_temporary_reference(val, loc);
      init->add_statement(Statement::make_assignment(this->val_, valref, loc));
    }

  if (this->closedvar_ != NULL)
    {
      go_assert(this->closed_ == NULL);
      Expression* cref = Expression::make_temporary_reference(closed_temp,
							      loc);
      this->closedvar_->var_value()->set_init(cref);
    }
  else if (this->closed_ != NULL && !this->closed_->is_sink_expression())
    {
      if (init == NULL)
	init = new Block(b, loc);
      Expression* cref = Expression::make_temporary_reference(closed_temp,
							      loc);
      init->add_statement(Statement::make_assignment(this->closed_, cref,
						     loc));
    }

  if (init != NULL)
    {
      gogo->lower_block(function, init);

      if (this->statements_ != NULL)
	init->add_statement(Statement::make_block_statement(this->statements_,
							    loc));
      this->statements_ = init;
    }
}

// Determine types.

void
Select_clauses::Select_clause::determine_types()
{
  go_assert(this->is_lowered_);
  if (this->statements_ != NULL)
    this->statements_->determine_types();
}

// Check types.

void
Select_clauses::Select_clause::check_types()
{
  if (this->is_default_)
    return;

  Channel_type* ct = this->channel_->type()->channel_type();
  if (ct == NULL)
    {
      go_error_at(this->channel_->location(), "expected channel");
      return;
    }

  if (this->is_send_ && !ct->may_send())
    go_error_at(this->location(), "invalid send on receive-only channel");
  else if (!this->is_send_ && !ct->may_receive())
    go_error_at(this->location(), "invalid receive on send-only channel");
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

// Return the backend representation for the statements to execute.

Bstatement*
Select_clauses::Select_clause::get_statements_backend(
    Translate_context* context)
{
  if (this->statements_ == NULL)
    return NULL;
  Bblock* bblock = this->statements_->get_backend(context);
  return context->backend()->block_statement(bblock);
}

// Dump the AST representation for a select case clause

void
Select_clauses::Select_clause::dump_clause(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  if (this->is_default_)
    {
      ast_dump_context->ostream() << "default:";
    }
  else
    {
      ast_dump_context->ostream() << "case "  ;
      if (this->is_send_)
        {
          ast_dump_context->dump_expression(this->channel_);
          ast_dump_context->ostream() << " <- " ;
	  if (this->val_ != NULL)
	    ast_dump_context->dump_expression(this->val_);
        }
      else
        {
	  if (this->val_ != NULL)
	    ast_dump_context->dump_expression(this->val_);
          if (this->closed_ != NULL)
            {
	      // FIXME: can val_ == NULL and closed_ ! = NULL?
              ast_dump_context->ostream() << " , " ;
              ast_dump_context->dump_expression(this->closed_);
            }
          if (this->closedvar_ != NULL || this->var_ != NULL)
            ast_dump_context->ostream() << " := " ;

          ast_dump_context->ostream() << " <- " ;
          ast_dump_context->dump_expression(this->channel_);
        }
      ast_dump_context->ostream() << ":" ;
    }
  ast_dump_context->dump_block(this->statements_);
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
Select_clauses::lower(Gogo* gogo, Named_object* function, Block* b,
		      Temporary_statement* sel)
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->lower(gogo, function, b, sel);
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

// Check types.

void
Select_clauses::check_types()
{
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->check_types();
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

// Convert to the backend representation.  We have already accumulated
// all the select information.  Now we call selectgo, which will
// return the index of the clause to execute.

Bstatement*
Select_clauses::get_backend(Translate_context* context,
			    Temporary_statement* sel,
			    Unnamed_label *break_label,
			    Location location)
{
  size_t count = this->clauses_.size();
  std::vector<std::vector<Bexpression*> > cases(count);
  std::vector<Bstatement*> clauses(count);

  Type* int_type = Type::lookup_integer_type("int");

  int i = 0;
  for (Clauses::iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p, ++i)
    {
      Expression* index_expr = Expression::make_integer_ul(i, int_type,
							   location);
      cases[i].push_back(index_expr->get_backend(context));

      Bstatement* s = p->get_statements_backend(context);
      Location gloc = (p->statements() == NULL
		       ? p->location()
		       : p->statements()->end_location());
      Bstatement* g = break_label->get_goto(context, gloc);

      if (s == NULL)
	clauses[i] = g;
      else
        clauses[i] = context->backend()->compound_statement(s, g);
    }

  Expression* selref = Expression::make_temporary_reference(sel, location);
  selref = Expression::make_unary(OPERATOR_AND, selref, location);
  Expression* call = Runtime::make_call(Runtime::SELECTGO, location, 1,
					selref);
  context->gogo()->lower_expression(context->function(), NULL, &call);
  Bexpression* bcall = call->get_backend(context);

  if (count == 0)
    {
      Bfunction* bfunction = context->function()->func_value()->get_decl();
      return context->backend()->expression_statement(bfunction, bcall);
    }

  std::vector<Bstatement*> statements;
  statements.reserve(2);

  Bfunction* bfunction = context->function()->func_value()->get_decl();
  Bstatement* switch_stmt = context->backend()->switch_statement(bfunction,
								 bcall,
								 cases,
								 clauses,
								 location);
  statements.push_back(switch_stmt);

  Bstatement* ldef = break_label->get_definition(context);
  statements.push_back(ldef);

  return context->backend()->statement_list(statements);
}
// Dump the AST representation for select clauses.

void
Select_clauses::dump_clauses(Ast_dump_context* ast_dump_context) const
{
  for (Clauses::const_iterator p = this->clauses_.begin();
       p != this->clauses_.end();
       ++p)
    p->dump_clause(ast_dump_context);
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
Select_statement::do_lower(Gogo* gogo, Named_object* function,
			   Block* enclosing, Statement_inserter*)
{
  if (this->is_lowered_)
    return this;

  Location loc = this->location();

  Block* b = new Block(enclosing, loc);

  go_assert(this->sel_ == NULL);

  int ncases = this->clauses_->size();
  Type* selstruct_type = Channel_type::select_type(ncases);
  this->sel_ = Statement::make_temporary(selstruct_type, NULL, loc);
  b->add_statement(this->sel_);

  int64_t selstruct_size;
  if (!selstruct_type->backend_type_size(gogo, &selstruct_size))
    {
      go_assert(saw_errors());
      return Statement::make_error_statement(loc);
    }

  Expression* ref = Expression::make_temporary_reference(this->sel_, loc);
  ref = Expression::make_unary(OPERATOR_AND, ref, loc);
  Expression* selstruct_size_expr =
    Expression::make_integer_int64(selstruct_size, NULL, loc);
  Expression* size_expr = Expression::make_integer_ul(ncases, NULL, loc);
  Expression* call = Runtime::make_call(Runtime::NEWSELECT, loc, 3,
					ref, selstruct_size_expr, size_expr);
  b->add_statement(Statement::make_statement(call, true));

  this->clauses_->lower(gogo, function, b, this->sel_);
  this->is_lowered_ = true;
  b->add_statement(this);

  return Statement::make_block_statement(b, loc);
}

// Whether the select statement itself may fall through to the following
// statement.

bool
Select_statement::do_may_fall_through() const
{
  // A select statement is terminating if no break statement
  // refers to it and all of its clauses are terminating.
  if (this->break_label_ != NULL)
    return true;
  return this->clauses_->may_fall_through();
}

// Return the backend representation for a select statement.

Bstatement*
Select_statement::do_get_backend(Translate_context* context)
{
  return this->clauses_->get_backend(context, this->sel_, this->break_label(),
				     this->location());
}

// Dump the AST representation for a select statement.

void
Select_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "select";
  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->ostream() << " {" << dsuffix(location()) << std::endl;
      this->clauses_->dump_clauses(ast_dump_context);
      ast_dump_context->ostream() << "}";
    }
  ast_dump_context->ostream() << std::endl;
}

// Make a select statement.

Select_statement*
Statement::make_select_statement(Location location)
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
For_statement::do_lower(Gogo*, Named_object*, Block* enclosing,
			Statement_inserter*)
{
  Statement* s;
  Location loc = this->location();

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
  top->set_derived_from(this);
  b->add_statement(Statement::make_unnamed_label_statement(top));

  s = Statement::make_block_statement(this->statements_,
				      this->statements_->start_location());
  b->add_statement(s);

  Location end_loc = this->statements_->end_location();

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

      Location cond_loc = this->cond_->location();
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

  Statement* bs = Statement::make_block_statement(b, loc);
  bs->block_statement()->set_is_lowered_for_statement();
  return bs;
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
  go_assert(this->break_label_ == NULL && this->continue_label_ == NULL);
  this->break_label_ = break_label;
  this->continue_label_ = continue_label;
}

// Whether the overall statement may fall through.

bool
For_statement::do_may_fall_through() const
{
  // A for loop is terminating if it has no condition and
  // no break statement.
  if(this->cond_ != NULL)
    return true;
  if(this->break_label_ != NULL)
    return true;
  return false;
}

// Dump the AST representation for a for statement.

void
For_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{
  if (this->init_ != NULL && ast_dump_context->dump_subblocks())
    {
      ast_dump_context->print_indent();
      ast_dump_context->indent();
      ast_dump_context->ostream() << "// INIT  " << std::endl;
      ast_dump_context->dump_block(this->init_);
      ast_dump_context->unindent();
    }
  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "for ";
  if (this->cond_ != NULL)
    ast_dump_context->dump_expression(this->cond_);

  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->ostream() << " {" << std::endl;
      ast_dump_context->dump_block(this->statements_);
      if (this->init_ != NULL)
	{
	  ast_dump_context->print_indent();
	  ast_dump_context->ostream() << "// POST " << std::endl;
	  ast_dump_context->dump_block(this->post_);
	}
      ast_dump_context->unindent();

      ast_dump_context->print_indent();
      ast_dump_context->ostream() << "}";
    }

  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a for statement.

For_statement*
Statement::make_for_statement(Block* init, Expression* cond, Block* post,
			      Location location)
{
  return new For_statement(init, cond, post, location);
}

// Class For_range_statement.

// Traversal.

int
For_range_statement::do_traverse(Traverse* traverse)
{
  if (this->index_var_ != NULL)
    {
      if (this->traverse_expression(traverse, &this->index_var_)
	  == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
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
For_range_statement::do_lower(Gogo* gogo, Named_object*, Block* enclosing,
			      Statement_inserter*)
{
  Type* range_type = this->range_->type();
  if (range_type->points_to() != NULL
      && range_type->points_to()->array_type() != NULL
      && !range_type->points_to()->is_slice_type())
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
      value_type = gogo->lookup_global("rune")->type_value();
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
	  if (!this->value_var_->type()->is_error())
	    this->report_error(_("too many variables for range clause "
				 "with channel"));
	  return Statement::make_error_statement(this->location());
	}
    }
  else
    {
      this->report_error(_("range clause must have "
			   "array, slice, string, map, or channel type"));
      return Statement::make_error_statement(this->location());
    }

  Location loc = this->location();
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
      this->range_ = NULL;
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

  if (range_type->is_slice_type())
    this->lower_range_slice(gogo, temp_block, body, range_object, range_temp,
			    index_temp, value_temp, &init, &cond, &iter_init,
			    &post);
  else if (range_type->array_type() != NULL)
    this->lower_range_array(gogo, temp_block, body, range_object, range_temp,
			    index_temp, value_temp, &init, &cond, &iter_init,
			    &post);
  else if (range_type->is_string_type())
    this->lower_range_string(gogo, temp_block, body, range_object, range_temp,
			     index_temp, value_temp, &init, &cond, &iter_init,
			     &post);
  else if (range_type->map_type() != NULL)
    this->lower_range_map(gogo, range_type->map_type(), temp_block, body,
			  range_object, range_temp, index_temp, value_temp,
			  &init, &cond, &iter_init, &post);
  else if (range_type->channel_type() != NULL)
    this->lower_range_channel(gogo, temp_block, body, range_object, range_temp,
			      index_temp, value_temp, &init, &cond, &iter_init,
			      &post);
  else
    go_unreachable();

  if (iter_init != NULL)
    body->add_statement(Statement::make_block_statement(iter_init, loc));

  if (this->index_var_ != NULL)
    {
      Statement* assign;
      Expression* index_ref =
	Expression::make_temporary_reference(index_temp, loc);
      if (this->value_var_ == NULL)
	assign = Statement::make_assignment(this->index_var_, index_ref, loc);
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
    }

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
				    Location loc)
{
  if (range_object != NULL)
    return Expression::make_var_reference(range_object, loc);
  else
    return Expression::make_temporary_reference(range_temp, loc);
}

// Return a call to the predeclared function FUNCNAME passing a
// reference to the temporary variable ARG.

Call_expression*
For_range_statement::call_builtin(Gogo* gogo, const char* funcname,
				  Expression* arg,
				  Location loc)
{
  Named_object* no = gogo->lookup_global(funcname);
  go_assert(no != NULL && no->is_function_declaration());
  Expression* func = Expression::make_func_reference(no, NULL, loc);
  Expression_list* params = new Expression_list();
  params->push_back(arg);
  return Expression::make_call(func, params, false, loc);
}

// Lower a for range over an array.

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
  Location loc = this->location();

  // The loop we generate:
  //   len_temp := len(range)
  //   range_temp := range
  //   for index_temp = 0; index_temp < len_temp; index_temp++ {
  //           value_temp = range_temp[index_temp]
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
  range_temp = Statement::make_temporary(NULL, ref, loc);
  Expression* len_call = this->call_builtin(gogo, "len", ref, loc);
  Temporary_statement* len_temp = Statement::make_temporary(index_temp->type(),
							    len_call, loc);
  init->add_statement(range_temp);
  init->add_statement(len_temp);

  Expression* zexpr = Expression::make_integer_ul(0, NULL, loc);

  Temporary_reference_expression* tref =
    Expression::make_temporary_reference(index_temp, loc);
  tref->set_is_lvalue();
  Statement* s = Statement::make_assignment(tref, zexpr, loc);
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

      ref = Expression::make_temporary_reference(range_temp, loc);
      Expression* ref2 = Expression::make_temporary_reference(index_temp, loc);
      Expression* index = Expression::make_index(ref, ref2, NULL, NULL, loc);

      tref = Expression::make_temporary_reference(value_temp, loc);
      tref->set_is_lvalue();
      s = Statement::make_assignment(tref, index, loc);

      iter_init->add_statement(s);
    }
  *piter_init = iter_init;

  // Set *PPOST to
  //   index_temp++

  Block* post = new Block(enclosing, loc);
  tref = Expression::make_temporary_reference(index_temp, loc);
  tref->set_is_lvalue();
  s = Statement::make_inc_statement(tref);
  post->add_statement(s);
  *ppost = post;
}

// Lower a for range over a slice.

void
For_range_statement::lower_range_slice(Gogo* gogo,
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
  Location loc = this->location();

  // The loop we generate:
  //   for_temp := range
  //   len_temp := len(for_temp)
  //   for index_temp = 0; index_temp < len_temp; index_temp++ {
  //           value_temp = for_temp[index_temp]
  //           index = index_temp
  //           value = value_temp
  //           original body
  //   }
  //
  // Using for_temp means that we don't need to check bounds when
  // fetching range_temp[index_temp].

  // Set *PINIT to
  //   range_temp := range
  //   var len_temp int
  //   len_temp = len(range_temp)
  //   index_temp = 0

  Block* init = new Block(enclosing, loc);

  Expression* ref = this->make_range_ref(range_object, range_temp, loc);
  Temporary_statement* for_temp = Statement::make_temporary(NULL, ref, loc);
  init->add_statement(for_temp);

  ref = Expression::make_temporary_reference(for_temp, loc);
  Expression* len_call = this->call_builtin(gogo, "len", ref, loc);
  Temporary_statement* len_temp = Statement::make_temporary(index_temp->type(),
							    len_call, loc);
  init->add_statement(len_temp);

  Expression* zexpr = Expression::make_integer_ul(0, NULL, loc);

  Temporary_reference_expression* tref =
    Expression::make_temporary_reference(index_temp, loc);
  tref->set_is_lvalue();
  Statement* s = Statement::make_assignment(tref, zexpr, loc);
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

      ref = Expression::make_temporary_reference(for_temp, loc);
      Expression* ref2 = Expression::make_temporary_reference(index_temp, loc);
      Expression* index = Expression::make_index(ref, ref2, NULL, NULL, loc);

      tref = Expression::make_temporary_reference(value_temp, loc);
      tref->set_is_lvalue();
      s = Statement::make_assignment(tref, index, loc);

      iter_init->add_statement(s);
    }
  *piter_init = iter_init;

  // Set *PPOST to
  //   index_temp++

  Block* post = new Block(enclosing, loc);
  tref = Expression::make_temporary_reference(index_temp, loc);
  tref->set_is_lvalue();
  s = Statement::make_inc_statement(tref);
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
  Location loc = this->location();

  // The loop we generate:
  //   len_temp := len(range)
  //   var next_index_temp int
  //   for index_temp = 0; index_temp < len_temp; index_temp = next_index_temp {
  //           value_temp = rune(range[index_temp])
  //           if value_temp < utf8.RuneSelf {
  //                   next_index_temp = index_temp + 1
  //           } else {
  //                   value_temp, next_index_temp = decoderune(range, index_temp)
  //           }
  //           index = index_temp
  //           value = value_temp
  //           // original body
  //   }

  // Set *PINIT to
  //   len_temp := len(range)
  //   var next_index_temp int
  //   index_temp = 0
  //   var value_temp rune // if value_temp not passed in

  Block* init = new Block(enclosing, loc);

  Expression* ref = this->make_range_ref(range_object, range_temp, loc);
  Call_expression* call = this->call_builtin(gogo, "len", ref, loc);
  Temporary_statement* len_temp =
    Statement::make_temporary(index_temp->type(), call, loc);
  init->add_statement(len_temp);

  Temporary_statement* next_index_temp =
    Statement::make_temporary(index_temp->type(), NULL, loc);
  init->add_statement(next_index_temp);

  Temporary_reference_expression* index_ref =
    Expression::make_temporary_reference(index_temp, loc);
  index_ref->set_is_lvalue();
  Expression* zexpr = Expression::make_integer_ul(0, index_temp->type(), loc);
  Statement* s = Statement::make_assignment(index_ref, zexpr, loc);
  init->add_statement(s);

  Type* rune_type;
  if (value_temp != NULL)
    rune_type = value_temp->type();
  else
    {
      rune_type = gogo->lookup_global("rune")->type_value();
      value_temp = Statement::make_temporary(rune_type, NULL, loc);
      init->add_statement(value_temp);
    }

  *pinit = init;

  // Set *PCOND to
  //   index_temp < len_temp

  index_ref = Expression::make_temporary_reference(index_temp, loc);
  Expression* len_ref =
    Expression::make_temporary_reference(len_temp, loc);
  *pcond = Expression::make_binary(OPERATOR_LT, index_ref, len_ref, loc);

  // Set *PITER_INIT to
  //   value_temp = rune(range[index_temp])
  //   if value_temp < utf8.RuneSelf {
  //           next_index_temp = index_temp + 1
  //   } else {
  //           value_temp, next_index_temp = decoderune(range, index_temp)
  //   }

  Block* iter_init = new Block(body_block, loc);

  ref = this->make_range_ref(range_object, range_temp, loc);
  index_ref = Expression::make_temporary_reference(index_temp, loc);
  ref = Expression::make_string_index(ref, index_ref, NULL, loc);
  ref = Expression::make_cast(rune_type, ref, loc);
  Temporary_reference_expression* value_ref =
    Expression::make_temporary_reference(value_temp, loc);
  value_ref->set_is_lvalue();
  s = Statement::make_assignment(value_ref, ref, loc);
  iter_init->add_statement(s);

  value_ref = Expression::make_temporary_reference(value_temp, loc);
  Expression* rune_self = Expression::make_integer_ul(0x80, rune_type, loc);
  Expression* cond = Expression::make_binary(OPERATOR_LT, value_ref, rune_self,
					     loc);

  Block* then_block = new Block(iter_init, loc);

  Temporary_reference_expression* lhs =
    Expression::make_temporary_reference(next_index_temp, loc);
  lhs->set_is_lvalue();
  index_ref = Expression::make_temporary_reference(index_temp, loc);
  Expression* one = Expression::make_integer_ul(1, index_temp->type(), loc);
  Expression* sum = Expression::make_binary(OPERATOR_PLUS, index_ref, one,
					    loc);
  s = Statement::make_assignment(lhs, sum, loc);
  then_block->add_statement(s);

  Block* else_block = new Block(iter_init, loc);

  ref = this->make_range_ref(range_object, range_temp, loc);
  index_ref = Expression::make_temporary_reference(index_temp, loc);
  call = Runtime::make_call(Runtime::DECODERUNE, loc, 2, ref, index_ref);

  value_ref = Expression::make_temporary_reference(value_temp, loc);
  value_ref->set_is_lvalue();
  Expression* res = Expression::make_call_result(call, 0);
  s = Statement::make_assignment(value_ref, res, loc);
  else_block->add_statement(s);

  lhs = Expression::make_temporary_reference(next_index_temp, loc);
  lhs->set_is_lvalue();
  res = Expression::make_call_result(call, 1);
  s = Statement::make_assignment(lhs, res, loc);
  else_block->add_statement(s);

  s = Statement::make_if_statement(cond, then_block, else_block, loc);
  iter_init->add_statement(s);

  *piter_init = iter_init;

  // Set *PPOST to
  //   index_temp = next_index_temp

  Block* post = new Block(enclosing, loc);

  index_ref = Expression::make_temporary_reference(index_temp, loc);
  index_ref->set_is_lvalue();
  ref = Expression::make_temporary_reference(next_index_temp, loc);
  s = Statement::make_assignment(index_ref, ref, loc);

  post->add_statement(s);
  *ppost = post;
}

// Lower a for range over a map.

void
For_range_statement::lower_range_map(Gogo* gogo,
				     Map_type* map_type,
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
  Location loc = this->location();

  // The runtime uses a struct to handle ranges over a map.  The
  // struct is built by Map_type::hiter_type for a specific map type.

  // The loop we generate:
  //   var hiter map_iteration_struct
  //   for mapiterinit(type, range, &hiter); hiter.key != nil; mapiternext(&hiter) {
  //           index_temp = *hiter.key
  //           value_temp = *hiter.val
  //           index = index_temp
  //           value = value_temp
  //           original body
  //   }

  // Set *PINIT to
  //   var hiter map_iteration_struct
  //   runtime.mapiterinit(type, range, &hiter)

  Block* init = new Block(enclosing, loc);

  Type* map_iteration_type = map_type->hiter_type(gogo);
  Temporary_statement* hiter = Statement::make_temporary(map_iteration_type,
							 NULL, loc);
  init->add_statement(hiter);

  Expression* p1 = Expression::make_type_descriptor(map_type, loc);
  Expression* p2 = this->make_range_ref(range_object, range_temp, loc);
  Expression* ref = Expression::make_temporary_reference(hiter, loc);
  Expression* p3 = Expression::make_unary(OPERATOR_AND, ref, loc);
  Expression* call = Runtime::make_call(Runtime::MAPITERINIT, loc, 3,
					p1, p2, p3);
  init->add_statement(Statement::make_statement(call, true));

  *pinit = init;

  // Set *PCOND to
  //   hiter.key != nil

  ref = Expression::make_temporary_reference(hiter, loc);
  ref = Expression::make_field_reference(ref, 0, loc);
  Expression* ne = Expression::make_binary(OPERATOR_NOTEQ, ref,
					   Expression::make_nil(loc),
					   loc);
  *pcond = ne;

  // Set *PITER_INIT to
  //   index_temp = *hiter.key
  //   value_temp = *hiter.val

  Block* iter_init = new Block(body_block, loc);

  Expression* lhs = Expression::make_temporary_reference(index_temp, loc);
  Expression* rhs = Expression::make_temporary_reference(hiter, loc);
  rhs = Expression::make_field_reference(ref, 0, loc);
  rhs = Expression::make_unary(OPERATOR_MULT, ref, loc);
  Statement* set = Statement::make_assignment(lhs, rhs, loc);
  iter_init->add_statement(set);

  if (value_temp != NULL)
    {
      lhs = Expression::make_temporary_reference(value_temp, loc);
      rhs = Expression::make_temporary_reference(hiter, loc);
      rhs = Expression::make_field_reference(rhs, 1, loc);
      rhs = Expression::make_unary(OPERATOR_MULT, rhs, loc);
      set = Statement::make_assignment(lhs, rhs, loc);
      iter_init->add_statement(set);
    }

  *piter_init = iter_init;

  // Set *PPOST to
  //   mapiternext(&hiter)

  Block* post = new Block(enclosing, loc);

  ref = Expression::make_temporary_reference(hiter, loc);
  p1 = Expression::make_unary(OPERATOR_AND, ref, loc);
  call = Runtime::make_call(Runtime::MAPITERNEXT, loc, 1, p1);
  post->add_statement(Statement::make_statement(call, true));

  *ppost = post;
}

// Lower a for range over a channel.

void
For_range_statement::lower_range_channel(Gogo*,
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
  go_assert(value_temp == NULL);

  Location loc = this->location();

  // The loop we generate:
  //   for {
  //           index_temp, ok_temp = <-range
  //           if !ok_temp {
  //                   break
  //           }
  //           index = index_temp
  //           original body
  //   }

  // We have no initialization code, no condition, and no post code.

  *pinit = NULL;
  *pcond = NULL;
  *ppost = NULL;

  // Set *PITER_INIT to
  //   index_temp, ok_temp = <-range
  //   if !ok_temp {
  //           break
  //   }

  Block* iter_init = new Block(body_block, loc);

  Temporary_statement* ok_temp =
    Statement::make_temporary(Type::lookup_bool_type(), NULL, loc);
  iter_init->add_statement(ok_temp);

  Expression* cref = this->make_range_ref(range_object, range_temp, loc);
  Temporary_reference_expression* iref =
    Expression::make_temporary_reference(index_temp, loc);
  iref->set_is_lvalue();
  Temporary_reference_expression* oref =
    Expression::make_temporary_reference(ok_temp, loc);
  oref->set_is_lvalue();
  Statement* s = Statement::make_tuple_receive_assignment(iref, oref, cref,
							  loc);
  iter_init->add_statement(s);

  Block* then_block = new Block(iter_init, loc);
  s = Statement::make_break_statement(this->break_label(), loc);
  then_block->add_statement(s);

  oref = Expression::make_temporary_reference(ok_temp, loc);
  Expression* cond = Expression::make_unary(OPERATOR_NOT, oref, loc);
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

// Dump the AST representation for a for range statement.

void
For_range_statement::do_dump_statement(Ast_dump_context* ast_dump_context) const
{

  ast_dump_context->print_indent();
  ast_dump_context->ostream() << "for ";
  ast_dump_context->dump_expression(this->index_var_);
  if (this->value_var_ != NULL)
    {
      ast_dump_context->ostream() << ", ";
      ast_dump_context->dump_expression(this->value_var_);
    }

  ast_dump_context->ostream() << " = range ";
  ast_dump_context->dump_expression(this->range_);
  if (ast_dump_context->dump_subblocks())
    {
      ast_dump_context->ostream() << " {" << std::endl;

      ast_dump_context->indent();

      ast_dump_context->dump_block(this->statements_);

      ast_dump_context->unindent();
      ast_dump_context->print_indent();
      ast_dump_context->ostream() << "}";
    }
  ast_dump_context->ostream() << dsuffix(location()) << std::endl;
}

// Make a for statement with a range clause.

For_range_statement*
Statement::make_for_range_statement(Expression* index_var,
				    Expression* value_var,
				    Expression* range,
				    Location location)
{
  return new For_range_statement(index_var, value_var, range, location);
}
