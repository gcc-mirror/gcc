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

// Mark variables whose addresses are taken and do some other
// cleanups.  This has to be done before the write barrier pass and
// after the escape analysis pass.  It would be nice to do this
// elsewhere but there isn't an obvious place.

class Mark_address_taken : public Traverse
{
 public:
  Mark_address_taken(Gogo* gogo)
    : Traverse(traverse_functions
	       | traverse_statements
	       | traverse_expressions),
      gogo_(gogo), function_(NULL)
  { }

  int
  function(Named_object*);

  int
  statement(Block*, size_t*, Statement*);

  int
  expression(Expression**);

 private:
  Call_expression*
  find_makeslice_call(Expression*);

  // General IR.
  Gogo* gogo_;
  // The function we are traversing.
  Named_object* function_;
};

// Record a function.

int
Mark_address_taken::function(Named_object* no)
{
  go_assert(this->function_ == NULL);
  this->function_ = no;
  int t = no->func_value()->traverse(this);
  this->function_ = NULL;

  if (t == TRAVERSE_EXIT)
    return t;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Traverse a statement.

int
Mark_address_taken::statement(Block* block, size_t* pindex, Statement* s)
{
  // If this is an assignment of the form s = append(s, ...), expand
  // it now, so that we can assign it to the left hand side in the
  // middle of the expansion and possibly skip a write barrier.
  Assignment_statement* as = s->assignment_statement();
  if (as != NULL && !as->lhs()->is_sink_expression())
    {
      Call_expression* rce = as->rhs()->call_expression();
      if (rce != NULL
	  && rce->builtin_call_expression() != NULL
	  && (rce->builtin_call_expression()->code()
	      == Builtin_call_expression::BUILTIN_APPEND)
          && Expression::is_same_variable(as->lhs(), rce->args()->front()))
	{
	  Statement_inserter inserter = Statement_inserter(block, pindex);
	  Expression* a =
	    rce->builtin_call_expression()->flatten_append(this->gogo_,
							   this->function_,
							   &inserter,
							   as->lhs(),
							   block);
	  go_assert(a == NULL);
	  // That does the assignment, so remove this statement.
	  Expression* e = Expression::make_boolean(true, s->location());
	  Statement* dummy = Statement::make_statement(e, true);
	  block->replace_statement(*pindex, dummy);
	}
    }
  return TRAVERSE_CONTINUE;
}

// Look through the expression of a Slice_value_expression's valmem to
// find an call to makeslice.

Call_expression*
Mark_address_taken::find_makeslice_call(Expression* expr)
{
  Unsafe_type_conversion_expression* utce =
    expr->unsafe_conversion_expression();
  if (utce != NULL)
    expr = utce->expr();

  Call_expression* call = expr->call_expression();
  if (call == NULL)
    return NULL;

  Func_expression* fe = call->fn()->func_expression();
  if (fe != NULL && fe->runtime_code() == Runtime::MAKESLICE)
    return call;

  // We don't worry about MAKESLICE64 bcause we don't want to use a
  // stack allocation for a large slice anyhow.

  return NULL;
}

// Mark variable addresses taken.

int
Mark_address_taken::expression(Expression** pexpr)
{
  Expression* expr = *pexpr;
  Unary_expression* ue = expr->unary_expression();
  if (ue != NULL)
    ue->check_operand_address_taken(this->gogo_);

  Array_index_expression* aie = expr->array_index_expression();
  if (aie != NULL
      && aie->end() != NULL
      && !aie->array()->type()->is_slice_type())
    {
      // Slice of an array. The escape analysis models this with
      // a child Node representing the address of the array.
      bool escapes = false;
      Node* n = Node::make_node(expr);
      if (n->child() == NULL
          || (n->child()->encoding() & ESCAPE_MASK) != Node::ESCAPE_NONE)
        escapes = true;
      aie->array()->address_taken(escapes);
    }

  if (expr->allocation_expression() != NULL)
    {
      Node* n = Node::make_node(expr);
      if ((n->encoding() & ESCAPE_MASK) == Node::ESCAPE_NONE)
        expr->allocation_expression()->set_allocate_on_stack();
    }
  if (expr->heap_expression() != NULL)
    {
      Node* n = Node::make_node(expr);
      if ((n->encoding() & ESCAPE_MASK) == Node::ESCAPE_NONE)
        expr->heap_expression()->set_allocate_on_stack();
    }
  if (expr->slice_literal() != NULL)
    {
      Node* n = Node::make_node(expr);
      if ((n->encoding() & ESCAPE_MASK) == Node::ESCAPE_NONE)
        expr->slice_literal()->set_storage_does_not_escape();
    }

  // Rewrite non-escaping makeslice with constant size to stack allocation.
  Slice_value_expression* sve = expr->slice_value_expression();
  if (sve != NULL)
    {
      Call_expression* call = this->find_makeslice_call(sve->valmem());
      if (call != NULL
	  && Node::make_node(call)->encoding() == Node::ESCAPE_NONE)
        {
          Expression* len_arg = call->args()->at(1);
          Expression* cap_arg = call->args()->at(2);
          Numeric_constant nclen;
          Numeric_constant nccap;
          unsigned long vlen;
          unsigned long vcap;
          if (len_arg->numeric_constant_value(&nclen)
              && cap_arg->numeric_constant_value(&nccap)
              && nclen.to_unsigned_long(&vlen) == Numeric_constant::NC_UL_VALID
              && nccap.to_unsigned_long(&vcap) == Numeric_constant::NC_UL_VALID)
            {
	      // Stack allocate an array and make a slice value from it.
              Location loc = expr->location();
              Type* elmt_type = expr->type()->array_type()->element_type();
              Expression* len_expr =
                Expression::make_integer_ul(vcap, cap_arg->type(), loc);
              Type* array_type = Type::make_array_type(elmt_type, len_expr);
              Expression* alloc = Expression::make_allocation(array_type, loc);
              alloc->allocation_expression()->set_allocate_on_stack();
	      Type* ptr_type = Type::make_pointer_type(elmt_type);
	      Expression* ptr = Expression::make_unsafe_cast(ptr_type, alloc,
							     loc);
	      Expression* slice =
		Expression::make_slice_value(expr->type(), ptr, len_arg,
					     cap_arg, loc);
              *pexpr = slice;
            }
        }
    }
  return TRAVERSE_CONTINUE;
}

// Check variables and closures do not escape when compiling runtime.

class Check_escape : public Traverse
{
 public:
  Check_escape(Gogo* gogo)
    : Traverse(traverse_expressions | traverse_variables),
      gogo_(gogo)
  { }

  int
  expression(Expression**);

  int
  variable(Named_object*);

 private:
  Gogo* gogo_;
};

int
Check_escape::variable(Named_object* no)
{
  if ((no->is_variable() && no->var_value()->is_in_heap())
      || (no->is_result_variable()
          && no->result_var_value()->is_in_heap()))
    go_error_at(no->location(),
                "%s escapes to heap, not allowed in runtime",
                no->message_name().c_str());
  return TRAVERSE_CONTINUE;
}

int
Check_escape::expression(Expression** pexpr)
{
  Expression* expr = *pexpr;
  Func_expression* fe = expr->func_expression();
  if (fe != NULL && fe->closure() != NULL)
    {
      Node* n = Node::make_node(expr);
      if (n->encoding() == Node::ESCAPE_HEAP)
        go_error_at(expr->location(),
                    "heap-allocated closure, not allowed in runtime");
    }
  return TRAVERSE_CONTINUE;
}

// Collect all writebarrierrec functions.  This is used when compiling
// the runtime package, to propagate //go:nowritebarrierrec.

class Collect_writebarrierrec_functions : public Traverse
{
 public:
  Collect_writebarrierrec_functions(std::vector<Named_object*>* worklist)
    : Traverse(traverse_functions),
      worklist_(worklist)
  { }

 private:
  int
  function(Named_object*);

  // The collected functions are put here.
  std::vector<Named_object*>* worklist_;
};

int
Collect_writebarrierrec_functions::function(Named_object* no)
{
  if (no->is_function()
      && no->func_value()->enclosing() == NULL
      && (no->func_value()->pragmas() & GOPRAGMA_NOWRITEBARRIERREC) != 0)
    {
      go_assert((no->func_value()->pragmas() & GOPRAGMA_MARK) == 0);
      this->worklist_->push_back(no);
    }
  return TRAVERSE_CONTINUE;
}

// Collect all callees of this function.  We only care about locally
// defined, known, functions.

class Collect_callees : public Traverse
{
 public:
  Collect_callees(std::vector<Named_object*>* worklist)
    : Traverse(traverse_expressions),
      worklist_(worklist)
  { }

 private:
  int
  expression(Expression**);

  // The collected callees are put here.
  std::vector<Named_object*>* worklist_;
};

int
Collect_callees::expression(Expression** pexpr)
{
  Call_expression* ce = (*pexpr)->call_expression();
  if (ce != NULL)
    {
      Func_expression* fe = ce->fn()->func_expression();
      if (fe != NULL)
	{
	  Named_object* no = fe->named_object();
	  if (no->package() == NULL && no->is_function())
	    {
	      // The function runtime.systemstack is special, in that
	      // it is a common way to call a function in the runtime:
	      // mark its argument if we can.
	      if (Gogo::unpack_hidden_name(no->name()) != "systemstack")
		this->worklist_->push_back(no);
	      else if (ce->args()->size() > 0)
		{
		  fe = ce->args()->front()->func_expression();
		  if (fe != NULL)
		    {
		      no = fe->named_object();
		      if (no->package() == NULL && no->is_function())
			this->worklist_->push_back(no);
		    }
		}
	    }
	}
    }
  return TRAVERSE_CONTINUE;
}

// When compiling the runtime package, propagate //go:nowritebarrierrec
// annotations.  A function marked as //go:nowritebarrierrec does not
// permit write barriers, and also all the functions that it calls,
// recursively, do not permit write barriers.  Except that a
// //go:yeswritebarrierrec annotation permits write barriers even if
// called by a //go:nowritebarrierrec function.  Here we turn
// //go:nowritebarrierrec into //go:nowritebarrier, as appropriate.

void
Gogo::propagate_writebarrierrec()
{
  std::vector<Named_object*> worklist;
  Collect_writebarrierrec_functions cwf(&worklist);
  this->traverse(&cwf);

  Collect_callees cc(&worklist);

  while (!worklist.empty())
    {
      Named_object* no = worklist.back();
      worklist.pop_back();

      unsigned int pragmas = no->func_value()->pragmas();
      if ((pragmas & GOPRAGMA_MARK) != 0)
	{
	  // We've already seen this function.
	  continue;
	}
      if ((pragmas & GOPRAGMA_YESWRITEBARRIERREC) != 0)
	{
	  // We don't want to propagate //go:nowritebarrierrec into
	  // this function or it's callees.
	  continue;
	}

      no->func_value()->set_pragmas(pragmas
				    | GOPRAGMA_NOWRITEBARRIER
				    | GOPRAGMA_MARK);

      no->func_value()->traverse(&cc);
    }
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
// we replaced the call to typedmemmove with gcWriteBarrier(&A, B).
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
// A slice could be handled with a call to gcWriteBarrier plus two
// integer moves.

// Traverse the IR adding write barriers.

class Write_barriers : public Traverse
{
 public:
  Write_barriers(Gogo* gogo)
    : Traverse(traverse_functions | traverse_variables | traverse_statements),
      gogo_(gogo), function_(NULL), statements_added_()
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
  // Statements introduced.
  Statement_inserter::Statements statements_added_;
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

  Statement_inserter inserter(this->gogo_, var, &this->statements_added_);
  Statement* s = this->gogo_->assign_with_write_barrier(NULL, NULL, &inserter,
							ref, init, loc);
  this->statements_added_.insert(s);

  var->add_preinit_statement(this->gogo_, s);
  var->clear_init();

  return TRAVERSE_CONTINUE;
}

// Insert write barriers for statements.

int
Write_barriers::statement(Block* block, size_t* pindex, Statement* s)
{
  if (this->statements_added_.find(s) != this->statements_added_.end())
    return TRAVERSE_SKIP_COMPONENTS;

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
	Statement_inserter inserter(block, pindex, &this->statements_added_);

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
        this->statements_added_.insert(assign);

	// Replace the old variable declaration statement with the new
	// initialization.
	block->replace_statement(*pindex, assign);
      }
      break;

    case Statement::STATEMENT_ASSIGNMENT:
      {
	Assignment_statement* as = s->assignment_statement();

	if (as->omit_write_barrier())
	  break;

	Expression* lhs = as->lhs();
	Expression* rhs = as->rhs();

	// We may need to emit a write barrier for the assignment.

	if (!this->gogo_->assign_needs_write_barrier(lhs))
	  break;

	// Change the assignment to use a write barrier.
	Function* function = this->function_;
	Location loc = as->location();
	Statement_inserter inserter =
            Statement_inserter(block, pindex, &this->statements_added_);
	Statement* assign = this->gogo_->assign_with_write_barrier(function,
								   block,
								   &inserter,
								   lhs, rhs,
								   loc);
        this->statements_added_.insert(assign);
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
  if (saw_errors())
    return;

  Mark_address_taken mat(this);
  this->traverse(&mat);

  if (this->compiling_runtime() && this->package_name() == "runtime")
    {
      this->propagate_writebarrierrec();

      Check_escape chk(this);
      this->traverse(&chk);
    }

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

      Type* bool_type = Type::lookup_bool_type();
      Array_type* pad_type = Type::make_array_type(this->lookup_global("byte")->type_value(),
						   Expression::make_integer_ul(3, NULL, bloc));
      Type* uint64_type = Type::lookup_integer_type("uint64");
      Type* wb_type = Type::make_builtin_struct_type(5,
						     "enabled", bool_type,
						     "pad", pad_type,
						     "needed", bool_type,
						     "cgo", bool_type,
						     "alignme", uint64_type);

      Variable* var = new Variable(wb_type, NULL,
				    true, false, false, bloc);

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

  // An assignment to a field or an array index is handled like an
  // assignment to the struct.
  while (true)
    {
      // Nothing to do for a type that can not be in the heap, or a
      // pointer to a type that can not be in the heap.  We check this
      // at each level of a struct.
      if (!lhs->type()->in_heap())
	return false;
      if (lhs->type()->points_to() != NULL
	  && !lhs->type()->points_to()->in_heap())
	return false;

      Field_reference_expression* fre = lhs->field_reference_expression();
      if (fre != NULL)
	{
	  lhs = fre->expr();
	  continue;
	}

      Array_index_expression* aie = lhs->array_index_expression();
      if (aie != NULL
	  && aie->end() == NULL
	  && !aie->array()->type()->is_slice_type())
	{
	  lhs = aie->array();
	  continue;
	}

      break;
    }

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

  // For a struct assignment, we don't need a write barrier if all the
  // pointer types can not be in the heap.
  Struct_type* st = lhs->type()->struct_type();
  if (st != NULL)
    {
      bool in_heap = false;
      const Struct_field_list* fields = st->fields();
      for (Struct_field_list::const_iterator p = fields->begin();
	   p != fields->end();
	   p++)
	{
	  Type* ft = p->type();
	  if (!ft->has_pointer())
	    continue;
	  if (!ft->in_heap())
	    continue;
	  if (ft->points_to() != NULL && !ft->points_to()->in_heap())
	    continue;
	  in_heap = true;
	  break;
	}
      if (!in_heap)
	return false;
    }

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
  if (function != NULL && (function->pragmas() & GOPRAGMA_NOWRITEBARRIER) != 0)
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

  if (!Type::are_identical(type, rhs->type(),
			   Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
			   NULL)
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

  Expression* indir =
      Expression::make_dereference(lhs, Expression::NIL_CHECK_DEFAULT, loc);
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
      {
	// These types are all represented by a single pointer.
	Type* uintptr_type = Type::lookup_integer_type("uintptr");
	rhs = Expression::make_unsafe_cast(uintptr_type, rhs, loc);
	call = Runtime::make_call(Runtime::GCWRITEBARRIER, loc, 2, lhs, rhs);
      }
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
  // We pretend that writeBarrier is a uint32, so that we do a
  // 32-bit load.  That is what the gc toolchain does.
  Type* void_type = Type::make_void_type();
  Type* unsafe_pointer_type = Type::make_pointer_type(void_type);
  Type* uint32_type = Type::lookup_integer_type("uint32");
  Type* puint32_type = Type::make_pointer_type(uint32_type);
  Expression* ref = Expression::make_var_reference(wb, loc);
  ref = Expression::make_unary(OPERATOR_AND, ref, loc);
  ref = Expression::make_cast(unsafe_pointer_type, ref, loc);
  ref = Expression::make_cast(puint32_type, ref, loc);
  ref = Expression::make_dereference(ref,
                                     Expression::NIL_CHECK_NOT_NEEDED, loc);
  Expression* zero = Expression::make_integer_ul(0, ref->type(), loc);
  Expression* cond = Expression::make_binary(OPERATOR_EQEQ, ref, zero, loc);

  Block* then_block = new Block(enclosing, loc);
  then_block->add_statement(without);

  Block* else_block = new Block(enclosing, loc);
  else_block->add_statement(with);

  return Statement::make_if_statement(cond, then_block, else_block, loc);
}
