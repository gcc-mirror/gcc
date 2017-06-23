// wb.cc -- Add write barriers as needed.

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "go-diagnostics.h"
#include "operator.h"
#include "lex.h"
#include "types.h"
#include "expressions.h"
#include "statements.h"
#include "runtime.h"
#include "gogo.h"

// Mark variables whose addresses are taken.  This has to be done
// before the write barrier pass and after the escape analysis pass.
// It would be nice to do this elsewhere but there isn't an obvious
// place.

class Mark_address_taken : public Traverse
{
 public:
  Mark_address_taken(Gogo* gogo)
    : Traverse(traverse_expressions),
      gogo_(gogo)
  { }

  int
  expression(Expression**);

 private:
  Gogo* gogo_;
};

// Mark variable addresses taken.

int
Mark_address_taken::expression(Expression** pexpr)
{
  Expression* expr = *pexpr;
  Unary_expression* ue = expr->unary_expression();
  if (ue != NULL)
    ue->check_operand_address_taken(this->gogo_);
  return TRAVERSE_CONTINUE;
}

// Add write barriers to the IR.  This are required by the concurrent
// garbage collector.  A write barrier is needed for any write of a
// pointer into memory controlled by the garbage collector.  Write
// barriers are not required for writes to local variables that live
// on the stack.  Write barriers are only required when the runtime
// enables them, which can be checked using a run time check on
// runtime.writeBarrier.enabled.
//
// Essentially, for each assignment A = B, where A is or contains a
// pointer, and where A is not, or at any rate may not be, a stack
// variable, we rewrite it into
//     if runtime.writeBarrier.enabled {
//         typedmemmove(typeof(A), &A, &B)
//     } else {
//         A = B
//     }
//
// The test of runtime.writeBarrier.Enabled is implemented by treating
// the variable as a *uint32, and testing *runtime.writeBarrier != 0.
// This is compatible with the definition in the runtime package.
//
// For types that are pointer shared (pointers, maps, chans, funcs),
// we replaced the call to typedmemmove with writebarrierptr(&A, B).
// As far as the GC is concerned, all pointers are the same, so it
// doesn't need the type descriptor.
//
// There are possible optimizations that are not implemented.
//
// runtime.writeBarrier can only change when the goroutine is
// preempted, which in practice means when a call is made into the
// runtime package, so we could optimize by only testing it once
// between function calls.
//
// A slice could be handled with a call to writebarrierptr plus two
// integer moves.

// Traverse the IR adding write barriers.

class Write_barriers : public Traverse
{
 public:
  Write_barriers(Gogo* gogo)
    : Traverse(traverse_functions | traverse_variables | traverse_statements),
      gogo_(gogo), function_(NULL)
  { }

  int
  function(Named_object*);

  int
  variable(Named_object*);

  int
  statement(Block*, size_t* pindex, Statement*);

 private:
  // General IR.
  Gogo* gogo_;
  // Current function.
  Function* function_;
};

// Traverse a function.  Just record it for later.

int
Write_barriers::function(Named_object* no)
{
  go_assert(this->function_ == NULL);
  this->function_ = no->func_value();
  int t = this->function_->traverse(this);
  this->function_ = NULL;

  if (t == TRAVERSE_EXIT)
    return t;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Insert write barriers for a global variable: ensure that variable
// initialization is handled correctly.  This is rarely needed, since
// we currently don't enable background GC until after all global
// variables are initialized.  But we do need this if an init function
// calls runtime.GC.

int
Write_barriers::variable(Named_object* no)
{
  // We handle local variables in the variable declaration statement.
  // We only have to handle global variables here.
  if (!no->is_variable())
    return TRAVERSE_CONTINUE;
  Variable* var = no->var_value();
  if (!var->is_global())
    return TRAVERSE_CONTINUE;

  // Nothing to do if there is no initializer.
  Expression* init = var->init();
  if (init == NULL)
    return TRAVERSE_CONTINUE;

  // Nothing to do for variables that do not contain any pointers.
  if (!var->type()->has_pointer())
    return TRAVERSE_CONTINUE;

  // Nothing to do if the initializer is static.
  init = Expression::make_cast(var->type(), init, var->location());
  if (!var->has_pre_init() && init->is_static_initializer())
    return TRAVERSE_CONTINUE;

  // Nothing to do for a type that can not be in the heap, or a
  // pointer to a type that can not be in the heap.
  if (!var->type()->in_heap())
    return TRAVERSE_CONTINUE;
  if (var->type()->points_to() != NULL && !var->type()->points_to()->in_heap())
    return TRAVERSE_CONTINUE;

  // Otherwise change the initializer into a pre_init assignment
  // statement with a write barrier.

  // We can't check for a dependency of the variable on itself after
  // we make this change, because the preinit statement will always
  // depend on the variable (since it assigns to it).  So check for a
  // self-dependency now.
  this->gogo_->check_self_dep(no);

  // Replace the initializer.
  Location loc = init->location();
  Expression* ref = Expression::make_var_reference(no, loc);
  ref->var_expression()->set_in_lvalue_pos();

  Statement_inserter inserter(this->gogo_, var);
  Statement* s = this->gogo_->assign_with_write_barrier(NULL, NULL, &inserter,
							ref, init, loc);

  var->add_preinit_statement(this->gogo_, s);
  var->clear_init();

  return TRAVERSE_CONTINUE;
}

// Insert write barriers for statements.

int
Write_barriers::statement(Block* block, size_t* pindex, Statement* s)
{
  switch (s->classification())
    {
    default:
      break;

    case Statement::STATEMENT_VARIABLE_DECLARATION:
      {
	Variable_declaration_statement* vds =
	  s->variable_declaration_statement();
	Named_object* no = vds->var();
	Variable* var = no->var_value();

	// We may need to emit a write barrier for the initialization
	// of the variable.

	// Nothing to do for a variable with no initializer.
	Expression* init = var->init();
	if (init == NULL)
	  break;

	// Nothing to do if the variable is not in the heap.  Only
	// local variables get declaration statements, and local
	// variables on the stack do not require write barriers.
	if (!var->is_in_heap())
	  break;

	// Nothing to do if the variable does not contain any pointers.
	if (!var->type()->has_pointer())
	  break;

	// Nothing to do for a type that can not be in the heap, or a
	// pointer to a type that can not be in the heap.
	if (!var->type()->in_heap())
	  break;
	if (var->type()->points_to() != NULL
	    && !var->type()->points_to()->in_heap())
	  break;

	// Otherwise initialize the variable with a write barrier.

	Function* function = this->function_;
	Location loc = init->location();
	Statement_inserter inserter(block, pindex);

	// Insert the variable declaration statement with no
	// initializer, so that the variable exists.
	var->clear_init();
	inserter.insert(s);

	// Create a statement that initializes the variable with a
	// write barrier.
	Expression* ref = Expression::make_var_reference(no, loc);
	Statement* assign = this->gogo_->assign_with_write_barrier(function,
								   block,
								   &inserter,
								   ref, init,
								   loc);

	// Replace the old variable declaration statement with the new
	// initialization.
	block->replace_statement(*pindex, assign);
      }
      break;

    case Statement::STATEMENT_ASSIGNMENT:
      {
	Assignment_statement* as = s->assignment_statement();
	Expression* lhs = as->lhs();
	Expression* rhs = as->rhs();

	// We may need to emit a write barrier for the assignment.

	if (!this->gogo_->assign_needs_write_barrier(lhs))
	  break;

	// Change the assignment to use a write barrier.
	Function* function = this->function_;
	Location loc = as->location();
	Statement_inserter inserter = Statement_inserter(block, pindex);
	Statement* assign = this->gogo_->assign_with_write_barrier(function,
								   block,
								   &inserter,
								   lhs, rhs,
								   loc);
	block->replace_statement(*pindex, assign);
      }
      break;
    }

  return TRAVERSE_CONTINUE;
}

// The write barrier pass.

void
Gogo::add_write_barriers()
{
  Mark_address_taken mat(this);
  this->traverse(&mat);

  Write_barriers wb(this);
  this->traverse(&wb);
}

// Return the runtime.writeBarrier variable.

Named_object*
Gogo::write_barrier_variable()
{
  static Named_object* write_barrier_var;
  if (write_barrier_var == NULL)
    {
      Location bloc = Linemap::predeclared_location();

      // We pretend that writeBarrier is a uint32, so that we do a
      // 32-bit load.  That is what the gc toolchain does.
      Type* uint32_type = Type::lookup_integer_type("uint32");
      Variable* var = new Variable(uint32_type, NULL, true, false, false,
				   bloc);

      bool add_to_globals;
      Package* package = this->add_imported_package("runtime", "_", false,
						    "runtime", "runtime",
						    bloc, &add_to_globals);
      write_barrier_var = Named_object::make_variable("writeBarrier",
						      package, var);
    }

  return write_barrier_var;
}

// Return whether an assignment that sets LHS needs a write barrier.

bool
Gogo::assign_needs_write_barrier(Expression* lhs)
{
  // Nothing to do if the variable does not contain any pointers.
  if (!lhs->type()->has_pointer())
    return false;

  // Nothing to do for an assignment to a temporary.
  if (lhs->temporary_reference_expression() != NULL)
    return false;

  // Nothing to do for an assignment to a sink.
  if (lhs->is_sink_expression())
    return false;

  // Nothing to do for an assignment to a local variable that is not
  // on the heap.
  Var_expression* ve = lhs->var_expression();
  if (ve != NULL)
    {
      Named_object* no = ve->named_object();
      if (no->is_variable())
	{
	  Variable* var = no->var_value();
	  if (!var->is_global() && !var->is_in_heap())
	    return false;
	}
      else if (no->is_result_variable())
	{
	  Result_variable* rvar = no->result_var_value();
	  if (!rvar->is_in_heap())
	    return false;
	}
    }

  // Nothing to do for a type that can not be in the heap, or a
  // pointer to a type that can not be in the heap.
  if (!lhs->type()->in_heap())
    return false;
  if (lhs->type()->points_to() != NULL && !lhs->type()->points_to()->in_heap())
    return false;

  // Write barrier needed in other cases.
  return true;
}

// Return a statement that sets LHS to RHS using a write barrier.
// ENCLOSING is the enclosing block.

Statement*
Gogo::assign_with_write_barrier(Function* function, Block* enclosing,
				Statement_inserter* inserter, Expression* lhs,
				Expression* rhs, Location loc)
{
  if (function != NULL
      && ((function->pragmas() & GOPRAGMA_NOWRITEBARRIER) != 0
	  || (function->pragmas() & GOPRAGMA_NOWRITEBARRIERREC) != 0))
    go_error_at(loc, "write barrier prohibited");

  Type* type = lhs->type();
  go_assert(type->has_pointer());

  Expression* addr;
  if (lhs->unary_expression() != NULL
      && lhs->unary_expression()->op() == OPERATOR_MULT)
    addr = lhs->unary_expression()->operand();
  else
    {
      addr = Expression::make_unary(OPERATOR_AND, lhs, loc);
      addr->unary_expression()->set_does_not_escape();
    }
  Temporary_statement* lhs_temp = Statement::make_temporary(NULL, addr, loc);
  inserter->insert(lhs_temp);
  lhs = Expression::make_temporary_reference(lhs_temp, loc);

  if (!Type::are_identical(type, rhs->type(), false, NULL)
      && rhs->type()->interface_type() != NULL
      && !rhs->is_variable())
    {
      // May need a temporary for interface conversion.
      Temporary_statement* temp = Statement::make_temporary(NULL, rhs, loc);
      inserter->insert(temp);
      rhs = Expression::make_temporary_reference(temp, loc);
    }
  rhs = Expression::convert_for_assignment(this, type, rhs, loc);
  Temporary_statement* rhs_temp = NULL;
  if (!rhs->is_variable() && !rhs->is_constant())
    {
      rhs_temp = Statement::make_temporary(NULL, rhs, loc);
      inserter->insert(rhs_temp);
      rhs = Expression::make_temporary_reference(rhs_temp, loc);
    }

  Expression* indir = Expression::make_unary(OPERATOR_MULT, lhs, loc);
  Statement* assign = Statement::make_assignment(indir, rhs, loc);

  lhs = Expression::make_temporary_reference(lhs_temp, loc);
  if (rhs_temp != NULL)
    rhs = Expression::make_temporary_reference(rhs_temp, loc);

  Type* unsafe_ptr_type = Type::make_pointer_type(Type::make_void_type());
  lhs = Expression::make_unsafe_cast(unsafe_ptr_type, lhs, loc);

  Expression* call;
  switch (type->base()->classification())
    {
    default:
      go_unreachable();

    case Type::TYPE_ERROR:
      return assign;

    case Type::TYPE_POINTER:
    case Type::TYPE_FUNCTION:
    case Type::TYPE_MAP:
    case Type::TYPE_CHANNEL:
      // These types are all represented by a single pointer.
      call = Runtime::make_call(Runtime::WRITEBARRIERPTR, loc, 2, lhs, rhs);
      break;

    case Type::TYPE_STRING:
    case Type::TYPE_STRUCT:
    case Type::TYPE_ARRAY:
    case Type::TYPE_INTERFACE:
      {
	rhs = Expression::make_unary(OPERATOR_AND, rhs, loc);
	rhs->unary_expression()->set_does_not_escape();
	call = Runtime::make_call(Runtime::TYPEDMEMMOVE, loc, 3,
				  Expression::make_type_descriptor(type, loc),
				  lhs, rhs);
      }
      break;
    }

  return this->check_write_barrier(enclosing, assign,
				   Statement::make_statement(call, false));
}

// Return a statement that tests whether write barriers are enabled
// and executes either the efficient code or the write barrier
// function call, depending.

Statement*
Gogo::check_write_barrier(Block* enclosing, Statement* without,
			  Statement* with)
{
  Location loc = without->location();
  Named_object* wb = this->write_barrier_variable();
  Expression* ref = Expression::make_var_reference(wb, loc);
  Expression* zero = Expression::make_integer_ul(0, ref->type(), loc);
  Expression* cond = Expression::make_binary(OPERATOR_EQEQ, ref, zero, loc);

  Block* then_block = new Block(enclosing, loc);
  then_block->add_statement(without);

  Block* else_block = new Block(enclosing, loc);
  else_block->add_statement(with);

  return Statement::make_if_statement(cond, then_block, else_block, loc);
}
