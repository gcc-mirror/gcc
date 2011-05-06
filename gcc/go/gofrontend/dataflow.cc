// dataflow.cc -- Go frontend dataflow.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "gogo.h"
#include "expressions.h"
#include "statements.h"
#include "dataflow.h"

// This class is used to traverse the tree to look for uses of
// variables.

class Dataflow_traverse_expressions : public Traverse
{
 public:
  Dataflow_traverse_expressions(Dataflow* dataflow, Statement* statement)
    : Traverse(traverse_blocks | traverse_expressions),
      dataflow_(dataflow), statement_(statement)
  { }

 protected:
  // Only look at top-level expressions: do not descend into blocks.
  // They will be examined via Dataflow_traverse_statements.
  int
  block(Block*)
  { return TRAVERSE_SKIP_COMPONENTS; }

  int
  expression(Expression**);

 private:
  // The dataflow information.
  Dataflow* dataflow_;
  // The Statement in which we are looking.
  Statement* statement_;
};

// Given an expression, return the Named_object that it refers to, if
// it is a local variable.

static Named_object*
get_var(Expression* expr)
{
  Var_expression* ve = expr->var_expression();
  if (ve == NULL)
    return NULL;
  Named_object* no = ve->named_object();
  go_assert(no->is_variable() || no->is_result_variable());
  if (no->is_variable() && no->var_value()->is_global())
    return NULL;
  return no;
}

// Look for a reference to a variable in an expression.

int
Dataflow_traverse_expressions::expression(Expression** expr)
{
  Named_object* no = get_var(*expr);
  if (no != NULL)
    this->dataflow_->add_ref(no, this->statement_);
  return TRAVERSE_CONTINUE;
}

// This class is used to handle an assignment statement.

class Dataflow_traverse_assignment : public Traverse_assignments
{
 public:
  Dataflow_traverse_assignment(Dataflow* dataflow, Statement* statement)
    : dataflow_(dataflow), statement_(statement)
  { }

 protected:
  void
  initialize_variable(Named_object*);

  void
  assignment(Expression** lhs, Expression** rhs);

  void
  value(Expression**, bool, bool);

 private:
  // The dataflow information.
  Dataflow* dataflow_;
  // The Statement in which we are looking.
  Statement* statement_;
};

// Handle a variable initialization.

void
Dataflow_traverse_assignment::initialize_variable(Named_object* var)
{
  Expression* init = var->var_value()->init();
  this->dataflow_->add_def(var, init, this->statement_, true);
  if (init != NULL)
    {
      Expression* e = init;
      this->value(&e, true, true);
      go_assert(e == init);
    }
}

// Handle an assignment in a statement.

void
Dataflow_traverse_assignment::assignment(Expression** plhs, Expression** prhs)
{
  Named_object* no = get_var(*plhs);
  if (no != NULL)
    {
      Expression* rhs = prhs == NULL ? NULL : *prhs;
      this->dataflow_->add_def(no, rhs, this->statement_, false);
    }
  else
    {
      // If this is not a variable it may be some computed lvalue, and
      // we want to look for references to variables in that lvalue.
      this->value(plhs, false, false);
    }
  if (prhs != NULL)
    this->value(prhs, true, false);
}

// Handle a value in a statement.

void
Dataflow_traverse_assignment::value(Expression** pexpr, bool, bool)
{
  Named_object* no = get_var(*pexpr);
  if (no != NULL)
    this->dataflow_->add_ref(no, this->statement_);
  else
    {
      Dataflow_traverse_expressions dte(this->dataflow_, this->statement_);
      Expression::traverse(pexpr, &dte);
    }
}

// This class is used to traverse the tree to look for statements.

class Dataflow_traverse_statements : public Traverse
{
 public:
  Dataflow_traverse_statements(Dataflow* dataflow)
    : Traverse(traverse_statements),
      dataflow_(dataflow)
  { }

 protected:
  int
  statement(Block*, size_t* pindex, Statement*);

 private:
  // The dataflow information.
  Dataflow* dataflow_;
};

// For each Statement, we look for expressions.

int
Dataflow_traverse_statements::statement(Block* block, size_t* pindex,
					Statement *statement)
{
  Dataflow_traverse_assignment dta(this->dataflow_, statement);
  if (!statement->traverse_assignments(&dta))
    {
      Dataflow_traverse_expressions dte(this->dataflow_, statement);
      statement->traverse(block, pindex, &dte);
    }
  return TRAVERSE_CONTINUE;
}

// Compare variables.

bool
Dataflow::Compare_vars::operator()(const Named_object* no1,
				   const Named_object* no2) const
{
  if (no1->name() < no2->name())
    return true;
  if (no1->name() > no2->name())
    return false;

  // We can have two different variables with the same name.
  source_location loc1 = no1->location();
  source_location loc2 = no2->location();
  if (loc1 < loc2)
    return false;
  if (loc1 > loc2)
    return true;

  if (no1 == no2)
    return false;

  // We can't have two variables with the same name in the same
  // location.
  go_unreachable();
}

// Class Dataflow.

Dataflow::Dataflow()
  : defs_(), refs_()
{
}

// Build the dataflow information.

void
Dataflow::initialize(Gogo* gogo)
{
  Dataflow_traverse_statements dts(this);
  gogo->traverse(&dts);
}

// Add a definition of a variable.

void
Dataflow::add_def(Named_object* var, Expression* val, Statement* statement,
		  bool is_init)
{
  Defs* defnull = NULL;
  std::pair<Defmap::iterator, bool> ins =
    this->defs_.insert(std::make_pair(var, defnull));
  if (ins.second)
    ins.first->second = new Defs;
  Def def;
  def.statement = statement;
  def.val = val;
  def.is_init = is_init;
  ins.first->second->push_back(def);
}

// Add a reference to a variable.

void
Dataflow::add_ref(Named_object* var, Statement* statement)
{
  Refs* refnull = NULL;
  std::pair<Refmap::iterator, bool> ins =
    this->refs_.insert(std::make_pair(var, refnull));
  if (ins.second)
    ins.first->second = new Refs;
  Ref ref;
  ref.statement = statement;
  ins.first->second->push_back(ref);
}

// Return the definitions of a variable.

const Dataflow::Defs*
Dataflow::find_defs(Named_object* var) const
{
  Defmap::const_iterator p = this->defs_.find(var);
  if (p == this->defs_.end())
    return NULL;
  else
    return p->second;
}

// Return the references of a variable.

const Dataflow::Refs*
Dataflow::find_refs(Named_object* var) const
{
  Refmap::const_iterator p = this->refs_.find(var);
  if (p == this->refs_.end())
    return NULL;
  else
    return p->second;
}
