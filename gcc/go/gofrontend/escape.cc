// escape.cc -- Go frontend escape analysis.

// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <fstream>

#include "go-c.h"
#include "go-dump.h"
#include "go-optimize.h"
#include "types.h"
#include "statements.h"
#include "expressions.h"
#include "dataflow.h"
#include "gogo.h"
#include "escape.h"

// Class Node.

Node::Node(Node_classification classification, Named_object* object)
  : classification_(classification), object_(object)
{
  // Give every node a unique ID for representation purposes.
  static int count;
  this->id_ = count++;
}

Node::~Node()
{
}

// Make a call node for FUNCTION.

Node*
Node::make_call(Named_object* function)
{
  return new Call_node(function);
}

// Make a connection node for OBJECT.

Node*
Node::make_connection(Named_object* object, Escapement_lattice e)
{
  return new Connection_node(object, e);
}

// Return this node's label, which will be the name seen in the graphical
// representation.

const std::string&
Node::label()
{
  if (this->label_.empty())
    {
      this->label_ = "[label=\"";
      this->label_ += this->object_->name();
      this->label_ += "\"]";
    }
  return this->label_;
}

// Class Call_node.

Call_node::Call_node(Named_object* function)
  : Node(NODE_CALL, function)
{ go_assert(function->is_function() || function->is_function_declaration()); }

const std::string&
Call_node::name()
{
  if (this->get_name().empty())
    {
      char buf[30];
      snprintf(buf, sizeof buf, "CallNode%d", this->id());
      this->set_name(std::string(buf));
    }
  return this->get_name();
}

// Class Connection_node.

const std::string&
Connection_node::name()
{
  if (this->get_name().empty())
    {
      char buf[30];
      snprintf(buf, sizeof buf, "ConnectionNode%d", this->id());
      this->set_name(std::string(buf));
    }
  return this->get_name();
}

const std::string&
Connection_node::label()
{
  if (this->get_label().empty())
    {
      std::string label = "[label=\"";
      label += this->object()->name();
      label += "\",color=";
      switch (this->escape_state_)
      {
      case ESCAPE_GLOBAL:
	label += "red";
	break;
      case ESCAPE_ARG:
	label += "blue";
	break;
      case ESCAPE_NONE:
	label += "black";
	break;
      }
      label += "]";
      this->set_label(label);
    }
  return this->get_label();
}

// Dump a connection node and its edges to a dump file.

void
Connection_node::dump_connection(Connection_dump_context* cdc)
{
  cdc->write_string(this->name() + this->label());
  cdc->write_c_string("\n");

  for (std::set<Node*>::const_iterator p = this->edges().begin();
       p != this->edges().end();
       ++p)
    {
      cdc->write_string(this->name());
      cdc->write_c_string("->");

      if ((*p)->object()->is_function())
	{
	  char buf[100];
	  snprintf(buf, sizeof buf, "dummy%d[lhead=cluster%d]",
		   (*p)->id(), (*p)->id());
	  cdc->write_c_string(buf);
	}
      else
	cdc->write_string((*p)->name());
      cdc->write_c_string("\n");
    }
}

// The -fgo-dump-calls flag to activate call graph dumps in GraphViz DOT format.

Go_dump call_graph_dump_flag("calls");

// Class Call_dump_context.

Call_dump_context::Call_dump_context(std::ostream* out)
  : ostream_(out), gogo_(NULL)
{ }

// Dump files will be named %basename%.calls.dot

const char* kCallDumpFileExtension = ".calls.dot";

// Dump the call graph in DOT format.

void
Call_dump_context::dump(Gogo* gogo, const char* basename)
{
  std::ofstream* out = new std::ofstream();
  std::string dumpname(basename);
  dumpname += kCallDumpFileExtension;
  out->open(dumpname.c_str());

  if (out->fail())
    {
      error("cannot open %s:%m, -fgo-dump-calls ignored", dumpname.c_str());
      return;
    }

  this->gogo_ = gogo;
  this->ostream_ = out;

  this->write_string("digraph CallGraph {\n");
  std::set<Node*> call_graph = gogo->call_graph();

  // Generate GraphViz nodes for each node.
  for (std::set<Node*>::const_iterator p = call_graph.begin();
       p != call_graph.end();
       ++p)
    {
      this->write_string((*p)->name() + (*p)->label());
      this->write_c_string("\n");
      
      // Generate a graphical representation of the caller-callee relationship.
      std::set<Node*> callees = (*p)->edges();
      for (std::set<Node*>::const_iterator ce = callees.begin();
	   ce != callees.end();
	   ++ce)
	{
	  this->write_string((*p)->name() + "->" + (*ce)->name());
	  this->write_c_string("\n");
	}
    }
  this->write_string("}");
  out->close();
}

// Dump the Call Graph of the program to the dump file.

void Gogo::dump_call_graph(const char* basename)
{
  if (::call_graph_dump_flag.is_enabled())
    {
      Call_dump_context cdc;
      cdc.dump(this, basename);
    }
}

// Implementation of String_dump interface.

void
Call_dump_context::write_c_string(const char* s)
{
  this->ostream() << s;
}

void
Call_dump_context::write_string(const std::string& s)
{
  this->ostream() << s;
}

// The -fgo-dump-conns flag to activate connection graph dumps in
// GraphViz DOT format.

Go_dump connection_graph_dump_flag("conns");

// Class Connection_dump_context.

Connection_dump_context::Connection_dump_context(std::ostream* out)
  : ostream_(out), gogo_(NULL)
{ }

// Dump files will be named %basename%.conns.dot

const char* kConnectionDumpFileExtension = ".conns.dot";

// Dump the connection graph in DOT format.

void
Connection_dump_context::dump(Gogo* gogo, const char* basename)
{
  std::ofstream* out = new std::ofstream();
  std::string dumpname(basename);
  dumpname += kConnectionDumpFileExtension;
  out->open(dumpname.c_str());

  if (out->fail())
    {
      error("cannot open %s:%m, -fgo-dump-conns ignored", dumpname.c_str());
      return;
    }

  this->gogo_ = gogo;
  this->ostream_ = out;

  this->write_string("digraph ConnectionGraph {\n");
  this->write_string("compound=true\n");

  // Dump global objects.
  std::set<Node*> globals = this->gogo_->global_connections();
  this->write_c_string("subgraph globals{\n");
  this->write_c_string("label=\"NonLocalGraph\"\n");
  this->write_c_string("color=red\n");
  for (std::set<Node*>::const_iterator p1 = globals.begin();
       p1 != globals.end();
       ++p1)
    (*p1)->connection_node()->dump_connection(this);
  this->write_c_string("}\n");

  std::set<Node*> roots = this->gogo_->connection_roots();
  for (std::set<Node*>::const_reverse_iterator p1 = roots.rbegin();
       p1 != roots.rend();
       ++p1)
    {
      std::set<Node*> objects = (*p1)->connection_node()->objects();

      char buf[150];
      snprintf(buf, sizeof buf, "subgraph cluster%d", (*p1)->id());
      this->write_c_string(buf);
      this->write_string("{\n");
      snprintf(buf, sizeof buf, "dummy%d[shape=point,style=invis]\n",
	       (*p1)->id());
      this->write_c_string(buf);
      this->write_string("label = \"" + (*p1)->object()->name() + "\"\n");

      for (std::set<Node*>::const_iterator p2 = objects.begin();
	   p2 != objects.end();
	   ++p2)
	(*p2)->connection_node()->dump_connection(this);

      this->write_string("}\n");
    }
  this->write_string("}");
  out->close();
}

void
Gogo::dump_connection_graphs(const char* basename)
{
  if (::connection_graph_dump_flag.is_enabled())
    {
      Connection_dump_context cdc;
      cdc.dump(this, basename);
    }
}

// Implementation of String_dump interface.

void
Connection_dump_context::write_c_string(const char* s)
{
  this->ostream() << s;
}

void
Connection_dump_context::write_string(const std::string& s)
{
  this->ostream() << s;
}

// A traversal class used to build a call graph for this program.

class Build_call_graph : public Traverse
{
 public:
  Build_call_graph(Gogo* gogo)
    : Traverse(traverse_functions
	       | traverse_expressions),
      gogo_(gogo), current_function_(NULL)
  { }

  int
  function(Named_object*);

  int
  expression(Expression**);

 private:
  // The IR.
  Gogo* gogo_;
  // The current function being traversed, for reference when traversing the
  // function body.
  Named_object* current_function_;
};

// Add each function to the call graph and then traverse each function's
// body to find callee functions.

int
Build_call_graph::function(Named_object* fn)
{
  this->gogo_->add_call_node(fn);
  go_assert(this->current_function_ == NULL);
  this->current_function_ = fn;
  fn->func_value()->traverse(this);
  this->current_function_ = NULL;
  return TRAVERSE_CONTINUE;
}

// Find function calls and add them as callees to CURRENT_FUNCTION.

int
Build_call_graph::expression(Expression** pexpr)
{
  if (this->current_function_ == NULL)
    return TRAVERSE_CONTINUE;

  Expression* expr = *pexpr;
  Named_object* fn;
  if (expr->call_expression() != NULL)
    {
      Func_expression* func = expr->call_expression()->fn()->func_expression();
      if (func == NULL)
	{
	  // This is probably a variable holding a function value or a closure.
	  return TRAVERSE_CONTINUE;
	}
      fn = func->named_object();
    }
  else if (expr->func_expression() != NULL)
    fn = expr->func_expression()->named_object();
  else
    return TRAVERSE_CONTINUE;

  Node* caller = this->gogo_->lookup_call_node(this->current_function_);
  go_assert(caller != NULL);

  // Create the callee here if it hasn't been seen yet.  This could also be a
  // function defined in another package.
  Node* callee = this->gogo_->add_call_node(fn);
  caller->add_edge(callee);
  return TRAVERSE_CONTINUE;
}

// Build the call graph.

void
Gogo::build_call_graph()
{
  Build_call_graph build_calls(this);
  this->traverse(&build_calls);
}

// A traversal class used to build a connection graph for each node in the
// call graph.

class Build_connection_graphs : public Traverse
{
 public:
  Build_connection_graphs(Gogo* gogo)
    : Traverse(traverse_variables
	       | traverse_statements),
      gogo_(gogo), dataflow_(new Dataflow), current_function_(NULL)
  {
    // Collect dataflow information for this program.
    this->dataflow_->initialize(this->gogo_);
  }

  void
  set_current_function(Named_object* function)
  { this->current_function_ = function; }

  int
  variable(Named_object*);

  int
  statement(Block*, size_t*, Statement*);


 private:
  // Handle a call EXPR referencing OBJECT.
  void
  handle_call(Named_object* object, Expression* expr);

  // Get the initialization values of a composite literal EXPR.
  Expression_list*
  get_composite_arguments(Expression* expr);

  // Handle defining OBJECT as a composite literal EXPR.
  void
  handle_composite_literal(Named_object* object, Expression* expr);

  // Handle analysis of the left and right operands of a binary expression
  // with respect to OBJECT.
  void
  handle_binary(Named_object* object, Expression* expr);

  // Resolve the outermost named object of EXPR if there is one.
  Named_object*
  resolve_var_reference(Expression* expr);

  // The IR.
  Gogo* gogo_;
  // The Dataflow information for this program.
  Dataflow* dataflow_;
  // The current function whose connection graph is being built.
  Named_object* current_function_;
};

// Given an expression, return the outermost Named_object that it refers to.
// This is used to model the simplification between assignments in our analysis.

Named_object*
Build_connection_graphs::resolve_var_reference(Expression* expr)
{
  bool done = false;
  Expression* orig = expr;
  while (!done)
    {
      // The goal of this loop is to find the variable being referenced, p,
      // when the expression is:
      switch (expr->classification())
      {
      case Expression::EXPRESSION_UNARY:
	// &p or *p
	expr = expr->unary_expression()->operand();
	break;

      case Expression::EXPRESSION_ARRAY_INDEX:
	// p[i][j]
	expr = expr->array_index_expression()->array();
	break;

      case Expression::EXPRESSION_FIELD_REFERENCE:
	// p.i.j
	orig = expr;
	expr = expr->field_reference_expression()->expr();
	break;

      case Expression::EXPRESSION_RECEIVE:
	// <- p
	expr = expr->receive_expression()->channel();
	break;

      case Expression::EXPRESSION_BOUND_METHOD:
	// p.c
	expr = expr->bound_method_expression()->first_argument();
	break;

      case Expression::EXPRESSION_CALL:
	// p.c()
	expr = expr->call_expression()->fn();
	break;

      case Expression::EXPRESSION_TEMPORARY_REFERENCE:
	// This is used after lowering, so try to retrieve the original
	// expression that might have been lowered into a temporary statement.
	expr = expr->temporary_reference_expression()->statement()->init();
	if (expr == NULL)
	  return NULL;
	break;

      case Expression::EXPRESSION_SET_AND_USE_TEMPORARY:
	expr = expr->set_and_use_temporary_expression()->expression();
	break;

      case Expression::EXPRESSION_COMPOUND:
	// p && q
	expr = expr->compound_expression()->init();
	break;

      case Expression::EXPRESSION_CONDITIONAL:
	// if p {
	expr = expr->conditional_expression()->condition();
	break;

      case Expression::EXPRESSION_CONVERSION:
	// T(p)
	expr = expr->conversion_expression()->expr();
	break;

      case Expression::EXPRESSION_TYPE_GUARD:
	// p.(T)
	expr = expr->type_guard_expression()->expr();
	break;

      case Expression::EXPRESSION_UNSAFE_CONVERSION:
	{
	  Expression* e = expr->unsafe_conversion_expression()->expr();
	  if (e->call_result_expression() != NULL
	      && e->call_result_expression()->index() == 0)
	    {
	      // a, ok := p.(T) gets lowered into a call to one of the interface
	      // to type conversion functions instead of a type guard expression.
	      // We only want to make a connection between a and p, the bool
	      // result should not escape because p escapes.
	      e = e->call_result_expression()->call();

	      Named_object* fn =
		e->call_expression()->fn()->func_expression()->named_object();
	      std::string fn_name = fn->name();
	      if (fn->package() == NULL
		  && fn->is_function_declaration()
		  && !fn->func_declaration_value()->asm_name().empty())
		{
		  if (fn_name == "ifaceI2E2"
		      || fn_name == "ifaceI2I2")
		    e = e->call_expression()->args()->at(0);
		  else if (fn_name == "ifaceE2I2"
			   || fn_name == "ifaceI2I2"
			   || fn_name == "ifaceE2T2P"
			   || fn_name == "ifaceI2T2P"
			   || fn_name == "ifaceE2T2"
			   || fn_name == "ifaceI2T2")
		    e = e->call_expression()->args()->at(1);
		}
	    }
	  expr = e;
	}
	break;

      default:
	done = true;
	break;
      }
    }

  Var_expression* ve = expr->var_expression();
  if (ve != NULL)
    {
      Named_object* no = ve->named_object();
      go_assert(no->is_variable() || no->is_result_variable());

      if (no->is_variable()
	  && no->var_value()->is_closure()
	  && this->current_function_->func_value()->needs_closure())
	{
	  // CURRENT_FUNCTION is a closure and NO is being set to a
	  // variable in the enclosing function.
	  Named_object* closure = this->current_function_;

	  // If NO is a closure variable, the expression is a field
	  // reference to the enclosed variable.
	  Field_reference_expression* fre =
	    orig->deref()->field_reference_expression();
	  if (fre == NULL)
	    return NULL;

	  unsigned int closure_index = fre->field_index();
	  no = closure->func_value()->enclosing_var(closure_index - 1);
	}
      return no;
    }
  return NULL;
}

// For a call that references OBJECT, associate the OBJECT argument with the
// appropriate call parameter.

void
Build_connection_graphs::handle_call(Named_object* object, Expression* e)
{
  // Only call expression statements are interesting
  // e.g. 'func(var)' for which we can show var does not escape.
  Call_expression* ce = e->call_expression();
  if (ce == NULL)
    return;
  else if (ce->args() == NULL)
    {
      if (ce->fn()->interface_field_reference_expression() != NULL)
	{
	  // This is a call to an interface method with no arguments. OBJECT
	  // must be the receiver and we assume it escapes.
	  Connection_node* rcvr_node =
	    this->gogo_->add_connection_node(object)->connection_node();
	  rcvr_node->set_escape_state(Node::ESCAPE_ARG);
	}
      return;
    }
  
  // If the function call that references OBJECT is unknown, we must be
  // conservative and assume every argument escapes.  A function call is unknown
  // if it is a call to a function stored in a variable or a call to an
  // interface method.
  if (ce->fn()->func_expression() == NULL)
    {
      for (Expression_list::const_iterator arg = ce->args()->begin();
	   arg != ce->args()->end();
	   ++arg)
	{
	  Named_object* arg_no = this->resolve_var_reference(*arg);
	  if (arg_no != NULL)
	    {
	      Connection_node* arg_node =
		this->gogo_->add_connection_node(arg_no)->connection_node();
	      arg_node->set_escape_state(Node::ESCAPE_ARG);
	    }
	  else if ((*arg)->call_expression() != NULL)
	    this->handle_call(object, *arg);
	}
      return;
    }

  Named_object* callee = ce->fn()->func_expression()->named_object();
  Function_type* fntype;
  if (callee->is_function())
    fntype = callee->func_value()->type();
  else
    fntype = callee->func_declaration_value()->type();

  Node* callee_node = this->gogo_->lookup_connection_node(callee);
  if (callee_node == NULL && callee->is_function())
    {
      // Might be a nested closure that hasn't been analyzed yet.
      Named_object* currfn = this->current_function_;
      callee_node = this->gogo_->add_connection_node(callee);
      this->current_function_ = callee;
      callee->func_value()->traverse(this);
      this->current_function_ = currfn;
    }

  // First find which arguments OBJECT is to CALLEE.  Given a function call,
  // OBJECT could be an argument multiple times e.g. CALLEE(OBJECT, OBJECT).
  // TODO(cmang): This should be done by the Dataflow analysis so we don't have
  // to do it each time we see a function call.  FIXME.
  Expression_list* args = ce->args()->copy();
  if (fntype->is_varargs()
      && args->back()->slice_literal() != NULL)
    {
      // Is the function is varargs, the last argument is lowered into a slice
      // containing all original arguments.  We want to traverse the original
      // arguments here.
      Slice_construction_expression* sce = args->back()->slice_literal();
      for (Expression_list::const_iterator p = sce->vals()->begin();
	   p != sce->vals()->end();
	   ++p)
	{
	  if (*p != NULL)
	    args->push_back(*p);
	}
    }

  // ARG_POSITION is just a counter used to keep track of the index in the list
  // of arguments to this call.  In a method call, the receiver will always be
  // the first argument.  When looking at the function type, it will not be the
  // first element in the parameter list; instead, the receiver will be
  // non-NULL.  For convenience, mark the position of the receiver argument
  // as negative.
  int arg_position = fntype->is_method() ? -1 : 0;
  std::list<int> positions;
  for (Expression_list::const_iterator p = args->begin();
       p != args->end();
       ++p, ++arg_position)
    {
      Expression* arg = *p;

      // An argument might be a chain of method calls, some of which are
      // converted from value to pointer types.  Just remove the unary
      // conversion if it exists.
      if (arg->unary_expression() != NULL)
	arg = arg->unary_expression()->operand();

      // The reference to OBJECT might be in a nested call argument.
      if (arg->call_expression() != NULL)
	this->handle_call(object, arg);

      std::vector<Named_object*> objects;
      if (arg->is_composite_literal()
	  || arg->heap_expression() != NULL)
	{
	  // For a call that has a composite literal as an argument, traverse
	  // the initializers of the composite literal for extra objects to
	  // associate with a parameter in this function.
	  Expression_list* comp_args = this->get_composite_arguments(arg);
	  if (comp_args == NULL)
	    continue;

	  for (size_t i = 0; i < comp_args->size(); ++i)
	    {
	      Expression* comp_arg = comp_args->at(i);
	      if (comp_arg == NULL)
		continue;
	      else if (comp_arg->is_composite_literal()
		       || comp_arg->heap_expression() != NULL)
		{
		  // Of course, there are situations where a composite literal
		  // initialization value is also a composite literal.
		  Expression_list* nested_args =
		    this->get_composite_arguments(comp_arg);
		  if (nested_args != NULL)
		    comp_args->append(nested_args);
		}

	      Named_object* no = this->resolve_var_reference(comp_arg);
	      if (no != NULL)
		objects.push_back(no);
	    }
	}
      else
	{
	  Named_object* arg_no = this->resolve_var_reference(arg);
	  if (arg_no != NULL)
	    objects.push_back(arg_no);
	}

      // There are no variables to consider for this parameter.
      if (objects.empty())
	continue;

      for (std::vector<Named_object*>::const_iterator p1 = objects.begin();
	   p1 != objects.end();
	   ++p1)
	{
	  // If CALLEE is defined in another package and we have imported escape
	  // information about its parameters, update the escape state of this
	  // argument appropriately. If there is no escape information for this
	  // function, we have to assume all arguments escape.
	  if (callee->package() != NULL
	      || fntype->is_builtin())
	    {
	      Node::Escapement_lattice param_escape = Node::ESCAPE_NONE;
	      if (fntype->has_escape_info())
		{
		  if (arg_position == -1)
		    {
		      // Use the escape info from the receiver.
		      param_escape = fntype->receiver_escape_state();
		    }
		  else if (fntype->parameters() != NULL)
		    {
		      const Node::Escape_states* states =
			fntype->parameter_escape_states();

		      int param_size = fntype->parameters()->size();
		      if (arg_position >= param_size)
			{
			  go_assert(fntype->is_varargs());
			  param_escape = states->back();
			}
		      else
			param_escape =
			  fntype->parameter_escape_states()->at(arg_position);
		    }
		}
	      else
		param_escape = Node::ESCAPE_ARG;

	      Connection_node* arg_node =
		this->gogo_->add_connection_node(*p1)->connection_node();
	      if (arg_node->escape_state() > param_escape)
		arg_node->set_escape_state(param_escape);
	    }

	  if (*p1 == object)
	    positions.push_back(arg_position);
	}
    }

  // If OBJECT was not found in CALLEE's arguments, OBJECT is likely a
  // subexpression of one of the arguments e.g. CALLEE(a[OBJECT]).  This call
  // does not give any useful information about whether OBJECT escapes.
  if (positions.empty())
    return;

  // The idea here is to associate the OBJECT in the caller context with the
  // parameter in the callee context.  This also needs to consider varargs.
  // This only works with functions with arguments.
  if (!callee->is_function())
    return;

  // Use the bindings in the callee to lookup the associated parameter.
  const Bindings* callee_bindings = callee->func_value()->block()->bindings();

  // Next find the corresponding named parameters in the function signature.
  const Typed_identifier_list* params = fntype->parameters();
  for (std::list<int>::const_iterator pos = positions.begin();
       params != NULL && pos != positions.end();
       ++pos)
    {
      std::string param_name;
      if (*pos >= 0 && params->size() <= static_cast<size_t>(*pos))
	{
	  // There were more arguments than there are parameters. This must be
	  // varargs and the argument corresponds to the last parameter.
	  go_assert(fntype->is_varargs());
	  param_name = params->back().name();
	}
      else if (*pos < 0)
	{
	  // We adjust the recorded position of method arguments by one to
	  // account for the receiver, so *pos == -1 implies this is the
	  // receiver and this must be a method call.
	  go_assert(fntype->is_method() && fntype->receiver() != NULL);
	  param_name = fntype->receiver()->name();
	}
      else
	param_name = params->at(*pos).name();

      if (Gogo::is_sink_name(param_name) || param_name.empty())
	continue;

      // Get the object for the associated parameter in this binding.
      Named_object* param_no = callee_bindings->lookup_local(param_name);
      go_assert(param_no != NULL);

      // Add an edge from ARG_NODE in the caller context to the PARAM_NODE in
      // the callee context.
      if (object->is_variable() && object->var_value()->is_closure())
	{
	  int position = *pos;
	  if (fntype->is_method())
	    ++position;

	  // Calling a function within a closure with a closure argument.
	  // Resolve the real variable using the closure argument.
	  object = this->resolve_var_reference(ce->args()->at(position));
	}

      Node* arg_node = this->gogo_->add_connection_node(object);
      Node* param_node = this->gogo_->add_connection_node(param_no);
      param_node->add_edge(arg_node);
    }

  // This is a method call with one argument: the receiver.
  if (params == NULL)
    {
      go_assert(positions.size() == 1);
      std::string rcvr_name = fntype->receiver()->name();
      if (Gogo::is_sink_name(rcvr_name) || rcvr_name.empty())
	return;

      Named_object* rcvr_no = callee_bindings->lookup_local(rcvr_name);
      Node* arg_node = this->gogo_->add_connection_node(object);
      Node* rcvr_node = this->gogo_->add_connection_node(rcvr_no);
      rcvr_node->add_edge(arg_node);
    }
}

// Given a composite literal expression, return the initialization values.
// This is used to handle situations where call and composite literal
// expressions have nested composite literals as arguments/initializers.

Expression_list*
Build_connection_graphs::get_composite_arguments(Expression* expr)
{
  // A heap expression is just any expression that takes the address of a
  // composite literal.
  if (expr->heap_expression() != NULL)
    expr = expr->heap_expression()->expr();

  switch (expr->classification())
    {
    case Expression::EXPRESSION_STRUCT_CONSTRUCTION:
      return expr->struct_literal()->vals();

    case Expression::EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
      return expr->array_literal()->vals();

    case Expression::EXPRESSION_SLICE_CONSTRUCTION:
      return expr->slice_literal()->vals();

    case Expression::EXPRESSION_MAP_CONSTRUCTION:
      return expr->map_literal()->vals();

    default:
      return NULL;
    }
}

// Given an OBJECT defined as a composite literal EXPR, create edges between
// OBJECT and all variables referenced in EXPR.

void
Build_connection_graphs::handle_composite_literal(Named_object* object,
						  Expression* expr)
{
  Expression_list* args = this->get_composite_arguments(expr);
  if (args == NULL)
    return;

  std::vector<Named_object*> composite_args;
  for (Expression_list::const_iterator p = args->begin();
       p != args->end();
       ++p)
    {
      if (*p == NULL)
	continue;
      else if ((*p)->call_expression() != NULL)
	this->handle_call(object, *p);
      else if ((*p)->func_expression() != NULL)
	composite_args.push_back((*p)->func_expression()->named_object());
      else if ((*p)->is_composite_literal()
	       || (*p)->heap_expression() != NULL)
	this->handle_composite_literal(object, *p);

      Named_object* no = this->resolve_var_reference(*p);
      if (no != NULL)
	composite_args.push_back(no);
    }

  Node* object_node = this->gogo_->add_connection_node(object);
  for (std::vector<Named_object*>::const_iterator p = composite_args.begin();
       p != composite_args.end();
       ++p)
    {
      Node* arg_node = this->gogo_->add_connection_node(*p);
      object_node->add_edge(arg_node);
    }
}

// Given an OBJECT reference in a binary expression E, analyze the left and
// right operands for possible edges.

void
Build_connection_graphs::handle_binary(Named_object* object, Expression* e)
{
  Binary_expression* be = e->binary_expression();
  go_assert(be != NULL);
  Expression* left = be->left();
  Expression* right = be->right();

  if (left->call_result_expression() != NULL)
    left = left->call_result_expression()->call();
  if (left->call_expression() != NULL)
    this->handle_call(object, left);
  else if (left->binary_expression() != NULL)
    this->handle_binary(object, left);
  if (right->call_result_expression() != NULL)
    right = right->call_result_expression()->call();
  if (right->call_expression() != NULL)
    this->handle_call(object, right);
  else if (right->binary_expression() != NULL)
    this->handle_binary(object, right);
}

// Create connection nodes for each variable in a called function.

int
Build_connection_graphs::variable(Named_object* var)
{
  Node* var_node = this->gogo_->add_connection_node(var);
  Node* root = this->gogo_->lookup_connection_node(this->current_function_);
  go_assert(root != NULL);

  // Add VAR to the set of objects in CURRENT_FUNCTION's connection graph.
  root->connection_node()->add_object(var_node);

  // A function's results always escape.
  if (var->is_result_variable())
    var_node->connection_node()->set_escape_state(Node::ESCAPE_ARG);

  // Create edges from a variable to its definitions.
  const Dataflow::Defs* defs = this->dataflow_->find_defs(var);
  if (defs != NULL)
    {
      for (Dataflow::Defs::const_iterator p = defs->begin();
	   p != defs->end();
	   ++p)
	{
	  Expression* def = p->val;
	  if (def == NULL)
	    continue;

	  if (def->conversion_expression() != NULL)
	    def = def->conversion_expression()->expr();
	  if (def->func_expression() != NULL)
	    {
	      // VAR is being defined as a function object.
	      Named_object* fn = def->func_expression()->named_object();
	      Node* fn_node = this->gogo_->add_connection_node(fn);
	      var_node->add_edge(fn_node);
	    }
	  else if(def->is_composite_literal()
		  || def->heap_expression() != NULL)
	    this->handle_composite_literal(var, def);

	  Named_object* ref = this->resolve_var_reference(def);
	  if (ref == NULL)
	    continue;

	  Node* ref_node = this->gogo_->add_connection_node(ref);
	  var_node->add_edge(ref_node);
	}
    }

  // Create edges from a reference to a variable.
  const Dataflow::Refs* refs = this->dataflow_->find_refs(var);
  if (refs != NULL)
    {
      for (Dataflow::Refs::const_iterator p = refs->begin();
	   p != refs->end();
	   ++p)
	{
	  switch (p->statement->classification())
	  {
	  case Statement::STATEMENT_ASSIGNMENT:
	    {
	      Assignment_statement* assn =
		p->statement->assignment_statement();
	      Named_object* lhs_no = this->resolve_var_reference(assn->lhs());
	      Named_object* rhs_no = this->resolve_var_reference(assn->rhs());

	      Expression* rhs = assn->rhs();
	      if (rhs->is_composite_literal()
		  || rhs->heap_expression() != NULL)
		this->handle_composite_literal(var, rhs);

	      if (rhs->call_result_expression() != NULL)
		{
		  // V's initialization will be a call result if
		  // V, V1 := call(VAR).
		  // There are no useful edges to make from V, but we want
		  // to make sure we handle the call that references VAR.
		  rhs = rhs->call_result_expression()->call();
		}
	      if (rhs->call_expression() != NULL)
		this->handle_call(var, rhs);

	      // If there is no standalone variable on the rhs, this could be a
	      // binary expression, which isn't interesting for analysis or a
	      // composite literal or call expression, which we handled above.
	      // If the underlying variable on the rhs isn't VAR then it is
	      // likely an indexing expression where VAR is the index.
	      if(lhs_no == NULL
		 || rhs_no == NULL
		 || rhs_no != var)
		break;

	      Node* def_node = this->gogo_->add_connection_node(lhs_no);
	      def_node->add_edge(var_node);
	    }
	    break;

	  case Statement::STATEMENT_SEND:
	    {
	      Send_statement* send = p->statement->send_statement();
	      Named_object* chan_no = this->resolve_var_reference(send->channel());
	      Named_object* val_no = resolve_var_reference(send->val());

	      if (chan_no == NULL || val_no == NULL)
		break;

	      Node* chan_node = this->gogo_->add_connection_node(chan_no);
	      Node* val_node = this->gogo_->add_connection_node(val_no);
	      chan_node->add_edge(val_node);
	    }
	    break;

	  case Statement::STATEMENT_EXPRESSION:
	    {
	      Expression* call = p->statement->expression_statement()->expr();
	      if (call->call_result_expression() != NULL)
		call = call->call_result_expression()->call();
	      this->handle_call(var, call);
	    }
	    break;

	  case Statement::STATEMENT_GO:
	  case Statement::STATEMENT_DEFER:
	    // Any variable referenced via a go or defer statement escapes to
	    // a different goroutine.
	    if (var_node->connection_node()->escape_state() > Node::ESCAPE_ARG)
	      var_node->connection_node()->set_escape_state(Node::ESCAPE_ARG);
	    this->handle_call(var, p->statement->thunk_statement()->call());
	    break;

	  case Statement::STATEMENT_IF:
	    {
	      // If this is a reference via an if statement, it is interesting
	      // if there is a function call in the condition.  References in
	      // the then and else blocks would be discovered in an earlier
	      // case.
	      If_statement* if_stmt = p->statement->if_statement();
	      Expression* cond = if_stmt->condition();
	      if (cond->call_expression() != NULL)
		this->handle_call(var, cond);
	      else if (cond->binary_expression() != NULL)
		this->handle_binary(var, cond);
	    }
	    break;

	  case Statement::STATEMENT_VARIABLE_DECLARATION:
	    {
	      // VAR could be referenced as the initialization for another
	      // variable, V e.g. V := call(VAR) or V := &T{field: VAR}.
	      Variable_declaration_statement* decl =
		p->statement->variable_declaration_statement();
	      Named_object* decl_no = decl->var();
	      Variable* v = decl_no->var_value();

	      Expression* init = v->init();
	      if (init == NULL)
		break;

	      if (init->is_composite_literal()
		  || init->heap_expression() != NULL)
		{
		  // Create edges between DECL_NO and each named object in the
		  // composite literal.
		  this->handle_composite_literal(decl_no, init);
		}

	      if (init->call_result_expression() != NULL)
		init = init->call_result_expression()->call();
	      if (init->call_expression() != NULL)
		this->handle_call(var, init);
	      else if (init->binary_expression() != NULL)
		this->handle_binary(var, init);
	    }
	    break;

	  case Statement::STATEMENT_TEMPORARY:
	    {
	      // A call to a function with mutliple results that references VAR
	      // will be lowered into a temporary at this point.  Make sure the
	      // call that references VAR is handled.
	      Expression* init = p->statement->temporary_statement()->init();
	      if (init == NULL)
		break;
	      else if (init->call_result_expression() != NULL)
		{
		  Expression* call = init->call_result_expression()->call();
		  this->handle_call(var, call);
		}
	    }

	  default:
	    break;
	  }
	}
    }
  return TRAVERSE_CONTINUE;
}

// Traverse statements to find interesting references that might have not
// been recorded in the dataflow analysis.  For example, many statements
// in closures are not properly recorded during dataflow analysis.  This should
// handle all of the cases handled above in statements that reference a
// variable.  FIXME.

int
Build_connection_graphs::statement(Block*, size_t*, Statement* s)
{
  switch(s->classification())
  {
  case Statement::STATEMENT_ASSIGNMENT:
    {
      Assignment_statement* assn = s->assignment_statement();
      Named_object* lhs_no = this->resolve_var_reference(assn->lhs());

      if (lhs_no == NULL)
	break;

      Expression* rhs = assn->rhs();
      if (rhs->temporary_reference_expression() != NULL)
	rhs = rhs->temporary_reference_expression()->statement()->init();
      if (rhs == NULL)
	break;

      if (rhs->call_result_expression() != NULL)
	rhs = rhs->call_result_expression()->call();
      if (rhs->call_expression() != NULL)
	{
	  // It's not clear what variables we are trying to find references to
	  // so just use the arguments to this call.
	  Expression_list* args = rhs->call_expression()->args();
	  if (args == NULL)
	    break;

	  for (Expression_list::const_iterator p = args->begin();
	       p != args->end();
	       ++p)
	    {
	      Named_object* no = this->resolve_var_reference(*p);
	      if (no != NULL) {
		Node* lhs_node = this->gogo_->add_connection_node(lhs_no);
		Node* rhs_node = this->gogo_->add_connection_node(no);
		lhs_node->add_edge(rhs_node);
	      }
	    }

	  this->handle_call(lhs_no, rhs);
	}
      else if (rhs->func_expression() != NULL)
	{
	  Node* lhs_node = this->gogo_->add_connection_node(lhs_no);
	  Named_object* fn = rhs->func_expression()->named_object();
	  Node* fn_node = this->gogo_->add_connection_node(fn);
	  lhs_node->add_edge(fn_node);
	}
      else
	{
	  Named_object* rhs_no = this->resolve_var_reference(rhs);
	  if (rhs_no != NULL)
	    {
	      Node* lhs_node = this->gogo_->add_connection_node(lhs_no);
	      Node* rhs_node = this->gogo_->add_connection_node(rhs_no);
	      lhs_node->add_edge(rhs_node);
	    }
	}
    }
    break;

  case Statement::STATEMENT_SEND:
    {
      Send_statement* send = s->send_statement();
      Named_object* chan_no = this->resolve_var_reference(send->channel());
      Named_object* val_no = this->resolve_var_reference(send->val());

      if (chan_no == NULL || val_no == NULL)
	break;

      Node* chan_node = this->gogo_->add_connection_node(chan_no);
      Node* val_node = this->gogo_->add_connection_node(val_no);
      chan_node->add_edge(val_node);
    }
    break;

  case Statement::STATEMENT_EXPRESSION:
    {
      Expression* expr = s->expression_statement()->expr();
      if (expr->call_result_expression() != NULL)
	expr = expr->call_result_expression()->call();
      if (expr->call_expression() != NULL)
	{
	  // It's not clear what variables we are trying to find references to
	  // so just use the arguments to this call.
	  Expression_list* args = expr->call_expression()->args();
	  if (args == NULL)
	    break;

	  for (Expression_list::const_iterator p = args->begin();
	       p != args->end();
	       ++p)
	    {
	      Named_object* no = this->resolve_var_reference(*p);
	      if (no != NULL)
		this->handle_call(no, expr);
	    }
	}
    }
    break;

  case Statement::STATEMENT_GO:
  case Statement::STATEMENT_DEFER:
    {
      // Any variable referenced via a go or defer statement escapes to
      // a different goroutine.
      Expression* call = s->thunk_statement()->call();
      if (call->call_expression() != NULL)
	{
	  // It's not clear what variables we are trying to find references to
	  // so just use the arguments to this call.
	  Expression_list* args = call->call_expression()->args();
	  if (args == NULL)
	    break;

	  for (Expression_list::const_iterator p = args->begin();
	       p != args->end();
	       ++p)
	    {
	      Named_object* no = this->resolve_var_reference(*p);
	      if (no != NULL)
		this->handle_call(no, call);
	    }
	}
    }
    break;

  case Statement::STATEMENT_VARIABLE_DECLARATION:
    {
      Variable_declaration_statement* decl =
	s->variable_declaration_statement();
      Named_object* decl_no = decl->var();
      Variable* v = decl_no->var_value();

      Expression* init = v->init();
      if (init == NULL)
	break;

      if (init->is_composite_literal()
	  || init->heap_expression() != NULL)
	{
	  // Create edges between DECL_NO and each named object in the
	  // composite literal.
	  this->handle_composite_literal(decl_no, init);
	}

      if (init->call_result_expression() != NULL)
	init = init->call_result_expression()->call();
      if (init->call_expression() != NULL)
	{
	  // It's not clear what variables we are trying to find references to
	  // so just use the arguments to this call.
	  Expression_list* args = init->call_expression()->args();
	  if (args == NULL)
	    break;

	  for (Expression_list::const_iterator p = args->begin();
	       p != args->end();
	       ++p)
	    {
	      Named_object* no = this->resolve_var_reference(*p);
	      if (no != NULL)
		this->handle_call(no, init);
	    }
	}
    }
    break;

  default:
    break;
  }

  return TRAVERSE_CONTINUE;
}

// Build the connection graphs for each function present in the call graph.

void
Gogo::build_connection_graphs()
{
  Build_connection_graphs build_conns(this);

  for (std::set<Node*>::const_iterator p = this->call_graph_.begin();
       p != this->call_graph_.end();
       ++p)
    {
      Named_object* func = (*p)->object();
      
      go_assert(func->is_function() || func->is_function_declaration());
      Function_type* fntype;
      if (func->is_function())
	fntype = func->func_value()->type();
      else
	fntype = func->func_declaration_value()->type();
      if (fntype->is_builtin())
	continue;

      this->add_connection_node(func);
      build_conns.set_current_function(func);
      if (func->is_function())
	{
	  // A pointer receiver of a method always escapes from the method.
	  if (fntype->is_method() &&
	      fntype->receiver()->type()->points_to() != NULL)
	    {
	      const Bindings* callee_bindings =
		func->func_value()->block()->bindings();
	      std::string rcvr_name = fntype->receiver()->name();
	      if (Gogo::is_sink_name(rcvr_name) || rcvr_name.empty())
		return;

	      Named_object* rcvr_no = callee_bindings->lookup_local(rcvr_name);
	      Node* rcvr_node = this->add_connection_node(rcvr_no);
	      rcvr_node->connection_node()->set_escape_state(Node::ESCAPE_ARG);
	    }
	  func->func_value()->traverse(&build_conns);
	}
    }
}

void
Gogo::analyze_reachability()
{
  std::list<Node*> worklist;

  // Run reachability analysis on all globally escaping objects.
  for (std::set<Node*>::const_iterator p = this->global_connections_.begin();
       p != this->global_connections_.end();
       ++p)
    worklist.push_back(*p);

  while (!worklist.empty())
    {
      Node* m = worklist.front();
      worklist.pop_front();

      std::set<Node*> reachable = m->edges();
      if (m->object()->is_function()
	  && m->object()->func_value()->needs_closure())
	{
	  // If a closure escapes everything it closes over also escapes.
	  Function* closure = m->object()->func_value();
	  for (size_t i = 0; i < closure->closure_field_count(); i++)
	    {
	      Named_object* enclosed = closure->enclosing_var(i);
	      Node* enclosed_node = this->lookup_connection_node(enclosed);
	      go_assert(enclosed_node != NULL);
	      reachable.insert(enclosed_node);
	    }
	}
      for (std::set<Node*>::iterator n = reachable.begin();
	   n != reachable.end();
	   ++n)
	{
	  // If an object can be reached from a node with ESCAPE_GLOBAL,
	  // it also must ESCAPE_GLOBAL.
	  if ((*n)->connection_node()->escape_state() != Node::ESCAPE_GLOBAL)
	    {
	      (*n)->connection_node()->set_escape_state(Node::ESCAPE_GLOBAL);
	      worklist.push_back(*n);
	    }
	}
    }

  // Run reachability analysis on all objects that escape via arguments.
  for (Named_escape_nodes::const_iterator p =
	 this->named_connection_nodes_.begin();
       p != this->named_connection_nodes_.end();
       ++p)
    {
      if (p->second->connection_node()->escape_state() < Node::ESCAPE_NONE)
	worklist.push_back(p->second);
    }

  while (!worklist.empty())
    {
      Node* m = worklist.front();
      worklist.pop_front();

      std::set<Node*> reachable = m->edges();
      if (m->object()->is_function()
	  && m->object()->func_value()->needs_closure())
	{
	  // If a closure escapes everything it closes over also escapes.
	  Function* closure = m->object()->func_value();
	  for (size_t i = 0; i < closure->closure_field_count(); i++)
	    {
	      Named_object* enclosed = closure->enclosing_var(i);
	      Node* enclosed_node = this->lookup_connection_node(enclosed);
	      go_assert(enclosed_node != NULL);
	      reachable.insert(enclosed_node);
	    }
	}
      for (std::set<Node*>::iterator n = reachable.begin();
	   n != reachable.end();
	   ++n)
	{
	  // If an object can be reached from a node with ESCAPE_ARG,
	  // it is ESCAPE_ARG or ESCAPE_GLOBAL.
	  Node::Escapement_lattice e = m->connection_node()->escape_state();
	  if ((*n)->connection_node()->escape_state() > e)
	    {
	      (*n)->connection_node()->set_escape_state(e);
	      worklist.push_back(*n);
	    }
	}
    }  
}

// Iterate over all functions analyzed in the analysis, recording escape
// information for each receiver and parameter.

void
Gogo::mark_escaping_signatures()
{
  for (std::set<Node*>::const_iterator p = this->call_graph_.begin();
       p != this->call_graph_.end();
       ++p)
    {
      Named_object* fn = (*p)->object();
      if (!fn->is_function())
	continue;

      Function* func = fn->func_value();
      Function_type* fntype = func->type();
      const Bindings* bindings = func->block()->bindings();

      // If this is a method, set the escape state of the receiver.
      if (fntype->is_method())
	{
	  std::string rcvr_name = fntype->receiver()->name();
	  if (rcvr_name.empty() || Gogo::is_sink_name(rcvr_name))
	    fntype->set_receiver_escape_state(Node::ESCAPE_NONE);
	  else
	    {
	      Named_object* rcvr_no = bindings->lookup_local(rcvr_name);
	      go_assert(rcvr_no != NULL);

	      Node* rcvr_node = this->lookup_connection_node(rcvr_no);
	      if (rcvr_node != NULL)
		{
		  Node::Escapement_lattice e =
		    rcvr_node->connection_node()->escape_state();
		  fntype->set_receiver_escape_state(e);
		}
	      else
		fntype->set_receiver_escape_state(Node::ESCAPE_NONE);
	    }
	  fntype->set_has_escape_info();
	}

      const Typed_identifier_list* params = fntype->parameters();
      if (params == NULL)
	continue;

      fntype->set_has_escape_info();
      Node::Escape_states* param_escape_states = new Node::Escape_states;
      for (Typed_identifier_list::const_iterator p1 = params->begin();
	   p1 != params->end();
	   ++p1)
	{
	  std::string param_name = p1->name();
	  if (param_name.empty() || Gogo::is_sink_name(param_name))
	    param_escape_states->push_back(Node::ESCAPE_NONE);
	  else
	    {
	      Named_object* param_no = bindings->lookup_local(param_name);
	      go_assert(param_no != NULL);

	      Node* param_node = this->lookup_connection_node(param_no);
	      if (param_node == NULL)
		{
		  param_escape_states->push_back(Node::ESCAPE_NONE);
		  continue;
		}

	      Node::Escapement_lattice e =
		param_node->connection_node()->escape_state();
	      param_escape_states->push_back(e);
	    }
	}
      go_assert(params->size() == param_escape_states->size());
      fntype->set_parameter_escape_states(param_escape_states);
    }
}

class Optimize_allocations : public Traverse
{
 public:
  Optimize_allocations(Gogo* gogo)
    : Traverse(traverse_variables),
      gogo_(gogo)
  { }

  int
  variable(Named_object*);

 private:
  // The IR.
  Gogo* gogo_;
};

// The -fgo-optimize-alloc flag activates this escape analysis.

Go_optimize optimize_allocation_flag("allocs");

// Propagate escape information for each variable.

int
Optimize_allocations::variable(Named_object* var)
{
  Node* var_node = this->gogo_->lookup_connection_node(var);
  if (var_node == NULL
      || var_node->connection_node()->escape_state() != Node::ESCAPE_NONE)
    return TRAVERSE_CONTINUE;

  if (var->is_variable())
    {
      var->var_value()->set_does_not_escape();
      if (var->var_value()->init() != NULL
	  && var->var_value()->init()->allocation_expression() != NULL)
	{
	  Allocation_expression* alloc =
	    var->var_value()->init()->allocation_expression();
	  alloc->set_allocate_on_stack();
	}
    }

  return TRAVERSE_CONTINUE;
}

// Perform escape analysis on this program and optimize allocations using
// the derived information if -fgo-optimize-allocs.

void
Gogo::optimize_allocations(const char** filenames)
{
  if (!::optimize_allocation_flag.is_enabled() || saw_errors())
    return;

  // Build call graph for this program.
  this->build_call_graph();

  // Dump the call graph for this program if -fgo-dump-calls is enabled.
  this->dump_call_graph(filenames[0]);

  // Build the connection graphs for this program.
  this->build_connection_graphs();

  // Dump the connection graphs if -fgo-dump-connections is enabled.
  this->dump_connection_graphs(filenames[0]);

  // Given the connection graphs for this program, perform a reachability
  // analysis to determine what objects escape.
  this->analyze_reachability();

  // Propagate escape information to variables and variable initializations.
  Optimize_allocations optimize_allocs(this);
  this->traverse(&optimize_allocs);

  // Store escape information for a function's receivers and parameters in the
  // function's signature for use when exporting package information.
  this->mark_escaping_signatures();
}
