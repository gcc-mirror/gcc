// escape.cc -- Go escape analysis (based on Go compiler algorithm).

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <limits>
#include <stack>

#include "gogo.h"
#include "types.h"
#include "expressions.h"
#include "statements.h"
#include "escape.h"

// class Node.

// Return the node's type, if it makes sense for it to have one.

Type*
Node::type() const
{
  if (this->object() != NULL
      && this->object()->is_variable())
    return this->object()->var_value()->type();
  else if (this->object() != NULL
	   && this->object()->is_function())
    return this->object()->func_value()->type();
  else if (this->expr() != NULL)
    return this->expr()->type();
  else
    return NULL;
}

// A helper for reporting; return this node's location.

Location
Node::location() const
{
  if (this->object() != NULL && !this->object()->is_sink())
    return this->object()->location();
  else if (this->expr() != NULL)
    return this->expr()->location();
  else if (this->statement() != NULL)
    return this->statement()->location();
  else
    return Linemap::unknown_location();
}

// Return this node's state, creating it if has not been initialized.

Node::Escape_state*
Node::state(Escape_context* context, Named_object* fn)
{
  if (this->state_ == NULL)
    {
      if (this->expr() != NULL && this->expr()->var_expression() != NULL)
	{
	  // Tie state of variable references to underlying variables.
	  Named_object* var_no = this->expr()->var_expression()->named_object();
	  Node* var_node = Node::make_node(var_no);
	  this->state_ = var_node->state(context, fn);
	}
      else
	{
	  this->state_ = new Node::Escape_state;
	  if (fn == NULL)
	    fn = context->current_function();

	  this->state_->fn = fn;
	}
    }
  go_assert(this->state_ != NULL);
  return this->state_;
}

void
Node::set_encoding(int enc)
{
  this->encoding_ = enc;
  if (this->expr() != NULL
      && this->expr()->var_expression() != NULL)
    {
      // Set underlying object as well.
      Named_object* no = this->expr()->var_expression()->named_object();
      Node::make_node(no)->set_encoding(enc);
    }
}

bool
Node::is_big(Escape_context* context) const
{
  Type* t = this->type();
  if (t == NULL
      || t->is_call_multiple_result_type()
      || t->is_sink_type()
      || t->is_void_type()
      || t->is_abstract())
    return false;

  int64_t size;
  bool ok = t->backend_type_size(context->gogo(), &size);
  bool big = ok && (size < 0 || size > 10 * 1024 * 1024);

  if (this->expr() != NULL)
    {
      if (this->expr()->allocation_expression() != NULL)
	{
	  ok = t->deref()->backend_type_size(context->gogo(), &size);
	  big = big || size <= 0 || size >= (1 << 16);
	}
      else if (this->expr()->call_expression() != NULL)
	{
	  Call_expression* call = this->expr()->call_expression();
	  Func_expression* fn = call->fn()->func_expression();
	  if (fn != NULL
	      && fn->is_runtime_function()
	      && (fn->runtime_code() == Runtime::MAKESLICE1
		  || fn->runtime_code() == Runtime::MAKESLICE2
		  || fn->runtime_code() == Runtime::MAKESLICE1BIG
		  || fn->runtime_code() == Runtime::MAKESLICE2BIG))
	    {
	      // Second argument is length.
	      Expression_list::iterator p = call->args()->begin();
	      ++p;

	      Numeric_constant nc;
	      unsigned long v;
	      if ((*p)->numeric_constant_value(&nc)
		  && nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_VALID)
		big = big || v >= (1 << 16);
	    }
	}
    }

  return big;
}

bool
Node::is_sink() const
{
  if (this->object() != NULL
      && this->object()->is_sink())
    return true;
  else if (this->expr() != NULL
	   && this->expr()->is_sink_expression())
    return true;
  return false;
}

std::map<Named_object*, Node*> Node::objects;
std::map<Expression*, Node*> Node::expressions;
std::map<Statement*, Node*> Node::statements;

// Make a object node or return a cached node for this object.

Node*
Node::make_node(Named_object* no)
{
  if (Node::objects.find(no) != Node::objects.end())
    return Node::objects[no];

  Node* n = new Node(no);
  std::pair<Named_object*, Node*> val(no, n);
  Node::objects.insert(val);
  return n;
}

// Make an expression node or return a cached node for this expression.

Node*
Node::make_node(Expression* e)
{
  if (Node::expressions.find(e) != Node::expressions.end())
    return Node::expressions[e];

  Node* n = new Node(e);
  std::pair<Expression*, Node*> val(e, n);
  Node::expressions.insert(val);
  return n;
}

// Make a statement node or return a cached node for this statement.

Node*
Node::make_node(Statement* s)
{
  if (Node::statements.find(s) != Node::statements.end())
    return Node::statements[s];

  Node* n = new Node(s);
  std::pair<Statement*, Node*> val(s, n);
  Node::statements.insert(val);
  return n;
}

// Returns the maximum of an exisiting escape value
// (and its additional parameter flow flags) and a new escape type.

int
Node::max_encoding(int e, int etype)
{
  if ((e & ESCAPE_MASK) >= etype)
    return e;
  if (etype == Node::ESCAPE_NONE || etype == Node::ESCAPE_RETURN)
    return (e & ~ESCAPE_MASK) | etype;
  return etype;
}

// Return a modified encoding for an input parameter that flows into an
// output parameter.

int
Node::note_inout_flows(int e, int index, Level level)
{
  // Flow+level is encoded in two bits.
  // 00 = not flow, xx = level+1 for 0 <= level <= maxEncodedLevel.
  // 16 bits for Esc allows 6x2bits or 4x3bits or 3x4bits if additional
  // information would be useful.
  if (level.value() <= 0 && level.suffix_value() > 0)
    return Node::max_encoding(e|ESCAPE_CONTENT_ESCAPES, Node::ESCAPE_NONE);
  if (level.value() < 0)
    return Node::ESCAPE_HEAP;
  if (level.value() >  ESCAPE_MAX_ENCODED_LEVEL)
    level = Level::From(ESCAPE_MAX_ENCODED_LEVEL);

  int encoded = level.value() + 1;
  int shift = ESCAPE_BITS_PER_OUTPUT_IN_TAG * index + ESCAPE_RETURN_BITS;
  int old = (e >> shift) & ESCAPE_BITS_MASK_FOR_TAG;
  if (old == 0
      || (encoded != 0 && encoded < old))
    old = encoded;

  int encoded_flow = old << shift;
  if (((encoded_flow >> shift) & ESCAPE_BITS_MASK_FOR_TAG) != old)
    {
      // Failed to encode.  Put this on the heap.
      return Node::ESCAPE_HEAP;
    }

  return (e & ~(ESCAPE_BITS_MASK_FOR_TAG << shift)) | encoded_flow;
}

// Class Escape_context.

Escape_context::Escape_context(Gogo* gogo, bool recursive)
  : gogo_(gogo), current_function_(NULL), recursive_(recursive),
    sink_(Node::make_node(Named_object::make_sink())), loop_depth_(0),
    flood_id_(0), pdepth_(0)
{
  // The sink always escapes to heap and strictly lives outside of the
  // current function i.e. loop_depth == -1.
  this->sink_->set_encoding(Node::ESCAPE_HEAP);
  Node::Escape_state* state = this->sink_->state(this, NULL);
  state->loop_depth = -1;
}

// Initialize the dummy return values for this Node N using the results
// in FNTYPE.

void
Escape_context::init_retvals(Node* n, Function_type* fntype)
{
  if (fntype == NULL || fntype->results() == NULL)
    return;

  Node::Escape_state* state = n->state(this, NULL);
  Location loc = n->location();

  int i = 0;
  char buf[50];
  for (Typed_identifier_list::const_iterator p = fntype->results()->begin();
       p != fntype->results()->end();
       ++p, ++i)
    {
      snprintf(buf, sizeof buf, ".out%d", i);
      Variable* dummy_var = new Variable(p->type(), NULL, false, false,
					 false, loc);
      dummy_var->set_is_used();
      Named_object* dummy_no =
	Named_object::make_variable(buf, NULL, dummy_var);
      Node* dummy_node = Node::make_node(dummy_no);
      // Initialize the state of the dummy output node.
      dummy_node->state(this, NULL);

      // Add dummy node to the retvals of n.
      state->retvals.push_back(dummy_node);
    }
}


// Apply an indirection to N and return the result.
// This really only works if N is an expression node; it essentially becomes
// Node::make_node(n->expr()->deref()).  We need the escape context to set the
// correct loop depth, however.

Node*
Escape_context::add_dereference(Node* n)
{
  // Just return the original node if we can't add an indirection.
  if (n->object() != NULL || n->statement() != NULL)
    return n;

  Node* ind = Node::make_node(n->expr()->deref());
  // Initialize the state if this node doesn't already exist.
  ind->state(this, NULL);
  return ind;
}

void
Escape_context::track(Node* n)
{
  n->set_encoding(Node::ESCAPE_NONE);
  // Initialize this node's state if it hasn't been encountered
  // before.
  Node::Escape_state* state = n->state(this, NULL);
  state->loop_depth = this->loop_depth_;

  this->noesc_.push_back(n);
}

// Return the string representation of an escapement encoding.

std::string
Escape_note::make_tag(int encoding)
{
  char buf[50];
  snprintf(buf, sizeof buf, "esc:0x%x", encoding);
  return buf;
}

// Return the escapement encoding for a string tag.

int
Escape_note::parse_tag(std::string* tag)
{
  if (tag == NULL || tag->substr(0, 4) != "esc:")
    return Node::ESCAPE_UNKNOWN;
  int encoding = (int)strtol(tag->substr(4).c_str(), NULL, 0);
  if (encoding == 0)
    return Node::ESCAPE_UNKNOWN;
  return encoding;
}

// Analyze the program flow for escape information.

void
Gogo::analyze_escape()
{
  // Discover strongly connected groups of functions to analyze for escape
  // information in this package.
  this->discover_analysis_sets();

  for (std::vector<Analysis_set>::iterator p = this->analysis_sets_.begin();
       p != this->analysis_sets_.end();
       ++p)
    {
      std::vector<Named_object*> stack = p->first;
      Escape_context* context = new Escape_context(this, p->second);

      // Analyze the flow of each function; build the connection graph.
      // This is the assign phase.
      for (std::vector<Named_object*>::reverse_iterator fn = stack.rbegin();
           fn != stack.rend();
           ++fn)
	{
	  context->set_current_function(*fn);
	  this->assign_connectivity(context, *fn);
	}

      // Propagate levels across each dst.  This is the flood phase.
      std::set<Node*> dsts = context->dsts();
      for (std::set<Node*>::iterator n = dsts.begin();
           n != dsts.end();
           ++n)
        this->propagate_escape(context, *n);

      // Tag each exported function's parameters with escape information.
      for (std::vector<Named_object*>::iterator fn = stack.begin();
           fn != stack.end();
           ++fn)
        this->tag_function(context, *fn);

      delete context;
    }
}

// Traverse the program, discovering the functions that are roots of strongly
// connected components.  The goal of this phase to produce a set of functions
// that must be analyzed in order.

class Escape_analysis_discover : public Traverse
{
 public:
  Escape_analysis_discover(Gogo* gogo)
    : Traverse(traverse_functions),
      gogo_(gogo), component_ids_()
  { }

  int
  function(Named_object*);

  int
  visit(Named_object*);

  int
  visit_code(Named_object*, int);

 private:
  // A counter used to generate the ID for the function node in the graph.
  static int id;

  // Type used to map functions to an ID in a graph of connected components.
  typedef Unordered_map(Named_object*, int) Component_ids;

  // The Go IR.
  Gogo* gogo_;
  // The list of functions encountered during connected component discovery.
  Component_ids component_ids_;
  // The stack of functions that this component consists of.
  std::stack<Named_object*> stack_;
};

int Escape_analysis_discover::id = 0;

// Visit each function.

int
Escape_analysis_discover::function(Named_object* fn)
{
  this->visit(fn);
  return TRAVERSE_CONTINUE;
}

// Visit a function FN, adding it to the current stack of functions
// in this connected component.  If this is the root of the component,
// create a set of functions to be analyzed later.
//
// Finding these sets is finding strongly connected components
// in the static call graph.  The algorithm for doing that is taken
// from Sedgewick, Algorithms, Second Edition, p. 482, with two
// adaptations.
//
// First, a closure (fn->func_value()->enclosing() == NULL) cannot be the
// root of a connected component.  Refusing to use it as a root
// forces it into the component of the function in which it appears.
// This is more convenient for escape analysis.
//
// Second, each function becomes two virtual nodes in the graph,
// with numbers n and n+1. We record the function's node number as n
// but search from node n+1. If the search tells us that the component
// number (min) is n+1, we know that this is a trivial component: one function
// plus its closures. If the search tells us that the component number is
// n, then there was a path from node n+1 back to node n, meaning that
// the function set is mutually recursive. The escape analysis can be
// more precise when analyzing a single non-recursive function than
// when analyzing a set of mutually recursive functions.

int
Escape_analysis_discover::visit(Named_object* fn)
{
  Component_ids::const_iterator p = this->component_ids_.find(fn);
  if (p != this->component_ids_.end())
    // Already visited.
    return p->second;

  this->id++;
  int id = this->id;
  this->component_ids_[fn] = id;
  this->id++;
  int min = this->id;

  this->stack_.push(fn);
  min = this->visit_code(fn, min);
  if ((min == id || min == id + 1)
      && fn->is_function()
      && fn->func_value()->enclosing() == NULL)
    {
      bool recursive = min == id;
      std::vector<Named_object*> group;

      for (; !this->stack_.empty(); this->stack_.pop())
	{
	  Named_object* n = this->stack_.top();
	  if (n == fn)
	    {
	      this->stack_.pop();
	      break;
	    }

	  group.push_back(n);
	  this->component_ids_[n] = std::numeric_limits<int>::max();
	}
      group.push_back(fn);
      this->component_ids_[fn] = std::numeric_limits<int>::max();

      std::reverse(group.begin(), group.end());
      this->gogo_->add_analysis_set(group, recursive);
    }

  return min;
}

// Helper class for discovery step.  Traverse expressions looking for
// function calls and closures to visit during the discovery step.

class Escape_discover_expr : public Traverse
{
 public:
  Escape_discover_expr(Escape_analysis_discover* ead, int min)
    : Traverse(traverse_expressions),
      ead_(ead), min_(min)
  { }

  int
  min()
  { return this->min_; }

  int
  expression(Expression** pexpr);

 private:
  // The original discovery analysis.
  Escape_analysis_discover* ead_;
  // The minimum component ID in this group.
  int min_;
};

// Visit any calls or closures found when discovering expressions.

int
Escape_discover_expr::expression(Expression** pexpr)
{
  Expression* e = *pexpr;
  Named_object* fn = NULL;
  if (e->call_expression() != NULL
      && e->call_expression()->fn()->func_expression() != NULL)
    {
      // Method call or function call.
      fn = e->call_expression()->fn()->func_expression()->named_object();
    }
  else if (e->func_expression() != NULL
           && e->func_expression()->closure() != NULL)
    {
      // Closure.
      fn = e->func_expression()->named_object();
    }

  if (fn != NULL)
    this->min_ = std::min(this->min_, this->ead_->visit(fn));
  return TRAVERSE_CONTINUE;
}

// Visit the body of each function, returns ID of the minimum connected
// component found in the body.

int
Escape_analysis_discover::visit_code(Named_object* fn, int min)
{
  if (!fn->is_function())
    return min;

  Escape_discover_expr ede(this, min);
  fn->func_value()->traverse(&ede);
  return ede.min();
}

// Discover strongly connected groups of functions to analyze.

void
Gogo::discover_analysis_sets()
{
  Escape_analysis_discover ead(this);
  this->traverse(&ead);
}

// Traverse all label and goto statements and mark the underlying label
// as looping or not looping.

class Escape_analysis_loop : public Traverse
{
 public:
  Escape_analysis_loop()
    : Traverse(traverse_statements)
  { }

  int
  statement(Block*, size_t*, Statement*);
};

int
Escape_analysis_loop::statement(Block*, size_t*, Statement* s)
{
  if (s->label_statement() != NULL)
    s->label_statement()->label()->set_nonlooping();
  else if (s->goto_statement() != NULL)
    {
      if (s->goto_statement()->label()->nonlooping())
        s->goto_statement()->label()->set_looping();
    }
  return TRAVERSE_CONTINUE;
}

// Traversal class used to look at all interesting statements within a function
// in order to build a connectivity graph between all nodes within a context's
// scope.

class Escape_analysis_assign : public Traverse
{
public:
  Escape_analysis_assign(Escape_context* context, Named_object* fn)
    : Traverse(traverse_statements
	       | traverse_expressions),
      context_(context), fn_(fn)
  { }

  // Model statements within a function as assignments and flows between nodes.
  int
  statement(Block*, size_t*, Statement*);

  // Model expressions within a function as assignments and flows between nodes.
  int
  expression(Expression**);

  // Model calls within a function as assignments and flows between arguments
  // and results.
  void
  call(Call_expression* call);

  // Model the assignment of DST to SRC.
  void
  assign(Node* dst, Node* src);

  // Model the assignment of DST to dereference of SRC.
  void
  assign_deref(Node* dst, Node* src);

  // Model the input-to-output assignment flow of one of a function call's
  // arguments, where the flow is encoding in NOTE.
  int
  assign_from_note(std::string* note, const std::vector<Node*>& dsts,
		   Node* src);

  // Record the flow of SRC to DST in DST.
  void
  flows(Node* dst, Node* src);

private:
  // The escape context for this set of functions.
  Escape_context* context_;
  // The current function being analyzed.
  Named_object* fn_;
};

// Model statements within a function as assignments and flows between nodes.

int
Escape_analysis_assign::statement(Block*, size_t*, Statement* s)
{
  // Adjust the loop depth as we enter/exit blocks related to for statements.
  bool is_for_statement = (s->is_block_statement()
                           && s->block_statement()->is_lowered_for_statement());
  if (is_for_statement)
    this->context_->increase_loop_depth();

  s->traverse_contents(this);

  if (is_for_statement)
    this->context_->decrease_loop_depth();

  switch (s->classification())
    {
    case Statement::STATEMENT_VARIABLE_DECLARATION:
      {
	Named_object* var = s->variable_declaration_statement()->var();
	Node* var_node = Node::make_node(var);
	Node::Escape_state* state = var_node->state(this->context_, NULL);
	state->loop_depth = this->context_->loop_depth();

	// Set the loop depth for this declaration.
	if (var->is_variable()
	    && var->var_value()->init() != NULL)
	  {
	    Node* init_node = Node::make_node(var->var_value()->init());
	    this->assign(var_node, init_node);
	  }
      }
      break;

    case Statement::STATEMENT_LABEL:
      {
	if (s->label_statement()->label()->looping())
	  this->context_->increase_loop_depth();
      }
      break;

    case Statement::STATEMENT_SWITCH:
    case Statement::STATEMENT_TYPE_SWITCH:
      // Want to model the assignment of each case variable to the switched upon
      // variable.  This should be lowered into assignment statements; nothing
      // to here if that's the case.
      // TODO(cmang): Verify.
      break;

    case Statement::STATEMENT_ASSIGNMENT:
      {
	Assignment_statement* assn = s->assignment_statement();
	Node* lhs = Node::make_node(assn->lhs());
	Node* rhs = Node::make_node(assn->rhs());

	// TODO(cmang): Add special case for escape analysis no-op:
	// func (b *Buffer) Foo() {
	// 	n, m := ...
	//	b.buf = b.buf[n:m]
	// }
	// This is okay for now, it just means b escapes; it is conservative.
	this->assign(lhs, rhs);
      }
      break;

    case Statement::STATEMENT_SEND:
      {
	Node* sent_node = Node::make_node(s->send_statement()->val());
	this->assign(this->context_->sink(), sent_node);
      }
      break;

    case Statement::STATEMENT_DEFER:
      if (this->context_->loop_depth() == 1)
	break;
      // fallthrough

    case Statement::STATEMENT_GO:
      {
	// Defer f(x) or go f(x).
	// Both f and x escape to the heap.
	Thunk_statement* thunk = s->thunk_statement();
	if (thunk->call()->call_expression() == NULL)
	  break;

	Call_expression* call = thunk->call()->call_expression();
	Node* func_node = Node::make_node(call->fn());
	this->assign(this->context_->sink(), func_node);
	if (call->args() != NULL)
	  {
	    for (Expression_list::const_iterator p = call->args()->begin();
		 p != call->args()->end();
		 ++p)
	      {
		Node* arg_node = Node::make_node(*p);
		this->assign(this->context_->sink(), arg_node);
	      }
	  }
      }
      break;

      // TODO(cmang): Associate returned values with dummy return nodes.

    default:
      break;
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// Model expressions within a function as assignments and flows between nodes.

int
Escape_analysis_assign::expression(Expression** pexpr)
{
  // Big stuff escapes unconditionally.
  Node* n = Node::make_node(*pexpr);
  if ((n->encoding() & ESCAPE_MASK) != int(Node::ESCAPE_HEAP)
      && n->is_big(this->context_))
    {
      n->set_encoding(Node::ESCAPE_HEAP);
      (*pexpr)->address_taken(true);
      this->assign(this->context_->sink(), n);
    }

  if ((*pexpr)->func_expression() == NULL)
    (*pexpr)->traverse_subexpressions(this);

  switch ((*pexpr)->classification())
    {
    case Expression::EXPRESSION_CALL:
      {
	Call_expression* call = (*pexpr)->call_expression();
	this->call(call);

	Func_expression* fe = call->fn()->func_expression();
	if (fe != NULL && fe->is_runtime_function())
	  {
	    switch (fe->runtime_code())
	      {
	      case Runtime::PANIC:
		{
		  // Argument could leak through recover.
		  Node* panic_arg = Node::make_node(call->args()->front());
		  this->assign(this->context_->sink(), panic_arg);
		}
		break;

	      case Runtime::APPEND:
		{
		  // Unlike gc/esc.go, a call to append has already had its
		  // varargs lowered into a slice of arguments.
		  // The content of the appended slice leaks.
		  Node* appended = Node::make_node(call->args()->back());
		  this->assign_deref(this->context_->sink(), appended);

		  // The content of the original slice leaks as well.
		  Node* appendee = Node::make_node(call->args()->back());
		  this->assign_deref(this->context_->sink(), appendee);
		}
		break;

	      case Runtime::COPY:
		{
		  // Lose track of the copied content.
		  Node* copied = Node::make_node(call->args()->back());
		  this->assign_deref(this->context_->sink(), copied);
		}
		break;

	      case Runtime::MAKECHAN:
	      case Runtime::MAKECHANBIG:
	      case Runtime::MAKEMAP:
	      case Runtime::MAKEMAPBIG:
	      case Runtime::MAKESLICE1:
	      case Runtime::MAKESLICE2:
	      case Runtime::MAKESLICE1BIG:
	      case Runtime::MAKESLICE2BIG:
	      case Runtime::BYTE_ARRAY_TO_STRING:
	      case Runtime::INT_ARRAY_TO_STRING:
	      case Runtime::STRING_TO_BYTE_ARRAY:
	      case Runtime::STRING_TO_INT_ARRAY:
	      case Runtime::STRING_PLUS:
	      case Runtime::CONSTRUCT_MAP:
	      case Runtime::INT_TO_STRING:
		{
		  Node* runtime_node = Node::make_node(fe);
		  this->context_->track(runtime_node);
		}
		break;

	      default:
		break;
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_ALLOCATION:
      {
	// Same as above; this is Runtime::NEW.
	Node* alloc_node = Node::make_node(*pexpr);
	this->context_->track(alloc_node);
      }
      break;

    case Expression::EXPRESSION_CONVERSION:
      {
	Type_conversion_expression* tce = (*pexpr)->conversion_expression();
	Node* tce_node = Node::make_node(tce);
	Node* converted = Node::make_node(tce->expr());
	this->context_->track(tce_node);

	this->assign(tce_node, converted);
      }
      break;

    case Expression::EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
    case Expression::EXPRESSION_SLICE_CONSTRUCTION:
      {
	Node* array_node = Node::make_node(*pexpr);
	if ((*pexpr)->slice_literal() != NULL)
	  this->context_->track(array_node);

	Expression_list* vals = ((*pexpr)->slice_literal() != NULL
				 ? (*pexpr)->slice_literal()->vals()
				 : (*pexpr)->array_literal()->vals());

	if (vals != NULL)
	  {
	    // Connect the array to its values.
	    for (Expression_list::const_iterator p = vals->begin();
		 p != vals->end();
		 ++p)
	      if ((*p) != NULL)
		this->assign(array_node, Node::make_node(*p));
	  }
      }
      break;

    case Expression::EXPRESSION_STRUCT_CONSTRUCTION:
      {
	Node* struct_node = Node::make_node(*pexpr);
	Expression_list* vals = (*pexpr)->struct_literal()->vals();
	if (vals != NULL)
	  {
	    // Connect the struct to its values.
	    for (Expression_list::const_iterator p = vals->begin();
		 p != vals->end();
		 ++p)
	      {
		if ((*p) != NULL)
		  this->assign(struct_node, Node::make_node(*p));
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_HEAP:
      {
	Node* pointer_node = Node::make_node(*pexpr);
	Node* lit_node = Node::make_node((*pexpr)->heap_expression()->expr());
	this->context_->track(pointer_node);

	// Connect pointer node to literal node; if the pointer node escapes, so
	// does the literal node.
	this->assign(pointer_node, lit_node);
      }
      break;

    case Expression::EXPRESSION_BOUND_METHOD:
      {
	Node* bound_node = Node::make_node(*pexpr);
	this->context_->track(bound_node);

	Expression* obj = (*pexpr)->bound_method_expression()->first_argument();
	Node* obj_node = Node::make_node(obj);

	// A bound method implies the receiver will be used outside of the
	// lifetime of the method in some way.  We lose track of the receiver.
	this->assign(this->context_->sink(), obj_node);
      }
      break;

    case Expression::EXPRESSION_MAP_CONSTRUCTION:
      {
	Map_construction_expression* mce = (*pexpr)->map_literal();
	Node* map_node = Node::make_node(mce);
	this->context_->track(map_node);

	// All keys and values escape to memory.
	if (mce->vals() != NULL)
	  {
	    for (Expression_list::const_iterator p = mce->vals()->begin();
		 p != mce->vals()->end();
		 ++p)
	      {
		if ((*p) != NULL)
		  this->assign(this->context_->sink(), Node::make_node(*p));
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_FUNC_REFERENCE:
      {
	Func_expression* fe = (*pexpr)->func_expression();
	if (fe->closure() != NULL)
	  {
	    // Connect captured variables to the closure.
	    Node* closure_node = Node::make_node(fe);
	    this->context_->track(closure_node);

	    // A closure expression already exists as the heap expression:
	    // &struct{f func_code, v []*Variable}{...}.
	    // Link closure to the addresses of the variables enclosed.
	    Heap_expression* he = fe->closure()->heap_expression();
	    Struct_construction_expression* sce = he->expr()->struct_literal();

	    // First field is function code, other fields are variable
	    // references.
	    Expression_list::const_iterator p = sce->vals()->begin();
	    ++p;
	    for (; p != sce->vals()->end(); ++p)
	      {
		Node* enclosed_node = Node::make_node(*p);
		Node::Escape_state* state =
		  enclosed_node->state(this->context_, NULL);
		state->loop_depth = this->context_->loop_depth();
		this->assign(closure_node, enclosed_node);
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_UNARY:
      {
	if ((*pexpr)->unary_expression()->op() != OPERATOR_AND)
	  break;

	Node* addr_node = Node::make_node(*pexpr);
	this->context_->track(addr_node);

	Expression* operand = (*pexpr)->unary_expression()->operand();
	Named_object* var = NULL;
	if (operand->var_expression() != NULL)
	  var = operand->var_expression()->named_object();
	else if (operand->enclosed_var_expression() != NULL)
	  var = operand->enclosed_var_expression()->variable();
	else if (operand->temporary_reference_expression() != NULL)
	  {
	    // Found in runtime/chanbarrier_test.go.  The address of a struct
	    // reference is usually a heap expression, except when it is a part
	    // of a case statement.  In that case, it is lowered into a
	    // temporary reference and never linked to the heap expression that
	    // initializes it.  In general, when taking the address of some
	    // temporary, the analysis should really be looking at the initial
	    // value of that temporary.
	    Temporary_reference_expression* tre =
	      operand->temporary_reference_expression();
	    if (tre->statement() != NULL
		&& tre->statement()->temporary_statement()->init() != NULL)
	      {
		Expression* init =
		  tre->statement()->temporary_statement()->init();
		Node* init_node = Node::make_node(init);
		this->assign(addr_node, init_node);
	      }
	  }

	if (var == NULL)
	  break;

	if (var->is_variable()
	    && !var->var_value()->is_parameter())
	  {
	    // For &x, use the loop depth of x if known.
	    Node::Escape_state* addr_state =
	      addr_node->state(this->context_, NULL);
	    Node* operand_node = Node::make_node(operand);
	    Node::Escape_state* operand_state =
	      operand_node->state(this->context_, NULL);
	    if (operand_state->loop_depth != 0)
	      addr_state->loop_depth = operand_state->loop_depth;
	  }
	else if ((var->is_variable()
		  && var->var_value()->is_parameter())
		 || var->is_result_variable())
	  {
	    Node::Escape_state* addr_state =
	      addr_node->state(this->context_, NULL);
	    addr_state->loop_depth = 1;
	  }
      }
      break;

    default:
      break;
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// Model calls within a function as assignments and flows between arguments
// and results.

void
Escape_analysis_assign::call(Call_expression* call)
{
  Func_expression* fn = call->fn()->func_expression();
  Function_type* fntype = call->get_function_type();
  bool indirect = false;

  // Interface method calls or closure calls are indirect calls.
  if (fntype == NULL
      || (fntype->is_method()
	  && fntype->receiver()->type()->interface_type() != NULL)
      || fn == NULL
      || (fn->named_object()->is_function()
	  && fn->named_object()->func_value()->enclosing() != NULL))
    indirect = true;

  Node* call_node = Node::make_node(call);
  std::vector<Node*> arg_nodes;
  if (call->fn()->interface_field_reference_expression() != NULL)
    {
      Interface_field_reference_expression* ifre =
	call->fn()->interface_field_reference_expression();
      Node* field_node = Node::make_node(ifre->expr());
      arg_nodes.push_back(field_node);
    }

  if (call->args() != NULL)
    {
      for (Expression_list::const_iterator p = call->args()->begin();
	   p != call->args()->end();
	   ++p)
	arg_nodes.push_back(Node::make_node(*p));
    }

  if (indirect)
    {
      // We don't know what happens to the parameters through indirect calls.
      // Be conservative and assume they all flow to theSink.
      for (std::vector<Node*>::iterator p = arg_nodes.begin();
           p != arg_nodes.end();
           ++p)
	{
	  this->assign(this->context_->sink(), *p);
	}

      this->context_->init_retvals(call_node, fntype);
      return;
    }

  // If FN is an untagged function.
  if (fn != NULL
      && fn->named_object()->is_function()
      && !fntype->is_tagged())
    {
      Function* f = fn->named_object()->func_value();
      const Bindings* callee_bindings = f->block()->bindings();

      const Typed_identifier_list* results = fntype->results();
      if (results != NULL)
	{
	  // Setup output list on this call node.
	  Node::Escape_state* state = call_node->state(this->context_, NULL);
	  for (Typed_identifier_list::const_iterator p1 = results->begin();
	       p1 != results->end();
	       ++p1)
	    {
	      if (p1->name().empty() || Gogo::is_sink_name(p1->name()))
		continue;

	      Named_object* result_no =
		callee_bindings->lookup_local(p1->name());
	      go_assert(result_no != NULL);
	      Node* result_node = Node::make_node(result_no);
	      state->retvals.push_back(result_node);
	    }
	}

      std::vector<Node*>::iterator p = arg_nodes.begin();
      if (fntype->is_method()
	  && fntype->receiver()->type()->has_pointer())
	{
	  std::string rcvr_name = fntype->receiver()->name();
	  if (rcvr_name.empty() || Gogo::is_sink_name(rcvr_name))
	    ;
	  else
	    {
	      Named_object* rcvr_no =
		callee_bindings->lookup_local(fntype->receiver()->name());
	      go_assert(rcvr_no != NULL);
	      Node* rcvr_node = Node::make_node(rcvr_no);
	      this->assign(rcvr_node, *p);
	    }
	  ++p;
	}

      const Typed_identifier_list* til = fntype->parameters();
      if (til != NULL)
	{
	  for (Typed_identifier_list::const_iterator p1 = til->begin();
	       p1 != til->end();
	       ++p1, ++p)
	    {
	      if (p1->name().empty() || Gogo::is_sink_name(p1->name()))
		continue;

	      Named_object* param_no =
		callee_bindings->lookup_local(p1->name());
	      go_assert(param_no != NULL);
	      Expression* arg = (*p)->expr();
	      if (arg->var_expression() != NULL
		  && arg->var_expression()->named_object() == param_no)
		continue;

	      Node* param_node = Node::make_node(param_no);
	      this->assign(param_node, *p);
	    }

	  for (; p != arg_nodes.end(); ++p)
	    {
	      this->assign(this->context_->sink(), *p);
	    }
	}

      return;
    }

  Node::Escape_state* call_state = call_node->state(this->context_, NULL);
  this->context_->init_retvals(call_node, fntype);

  // Receiver.
  std::vector<Node*>::iterator p = arg_nodes.begin();
  if (fntype->is_method()
      && fntype->receiver()->type()->has_pointer()
      && p != arg_nodes.end())
    {
      // First argument to call will be the receiver.
      std::string* note = fntype->receiver()->note();
      if (fntype->receiver()->type()->points_to() == NULL
	  && (*p)->expr()->unary_expression() != NULL
	  && (*p)->expr()->unary_expression()->op() == OPERATOR_AND)
	{
	  // This is a call to a value method that has been lowered into a call
	  // to a pointer method.  Gccgo generates a pointer method for all
	  // method calls and takes the address of the value passed as the
	  // receiver then immediately dereferences it within the function.
	  // In this case, the receiver does not escape.
	}
      else
	{
	  if (!Type::are_identical(fntype->receiver()->type(),
			       (*p)->expr()->type(), true, NULL))
	    {
	      // This will be converted later, preemptively track it instead
	      // of its conversion expression which will show up in a later pass.
	      this->context_->track(*p);
	    }
	  this->assign_from_note(note, call_state->retvals, *p);
	}
      p++;
    }

  const Typed_identifier_list* til = fntype->parameters();
  if (til != NULL)
    {
      for (Typed_identifier_list::const_iterator pn = til->begin();
	   pn != til->end() && p != arg_nodes.end();
	   ++pn, ++p)
	{
	  if (!Type::are_identical(pn->type(), (*p)->expr()->type(),
				   true, NULL))
	    {
	      // This will be converted later, preemptively track it instead
	      // of its conversion expression which will show up in a later pass.
	      this->context_->track(*p);
	    }

	  // TODO(cmang): Special care for varargs parameter?
	  Type* t = pn->type();
	  if (t != NULL
	      && t->has_pointer())
	    {
	      std::string* note = pn->note();
	      int enc = this->assign_from_note(note, call_state->retvals, *p);
	      if (enc == Node::ESCAPE_NONE
		  && (call->is_deferred()
		      || call->is_concurrent()))
		{
		  // TODO(cmang): Mark the argument as strictly non-escaping.
		}
	    }
	}

      for (; p != arg_nodes.end(); ++p)
	{
	  this->assign(this->context_->sink(), *p);
	}
    }
}

// Model the assignment of DST to SRC.
// Assert that SRC somehow gets assigned to DST.
// DST might need to be examined for evaluations that happen inside of it.
// For example, in [DST]*f(x) = [SRC]y, we lose track of the indirection and
// DST becomes the sink in our model.

void
Escape_analysis_assign::assign(Node* dst, Node* src)
{
  if (dst->expr() != NULL)
    {
      // Analyze the lhs of the assignment.
      // Replace DST with this->context_->sink() if we can't track it.
      Expression* e = dst->expr();
      switch (e->classification())
        {
	case Expression::EXPRESSION_VAR_REFERENCE:
	  {
	    // If DST is a global variable, we have no way to track it.
	    Named_object* var = e->var_expression()->named_object();
	    if (var->is_variable() && var->var_value()->is_global())
	      dst = this->context_->sink();
	  }
	  break;

	case Expression::EXPRESSION_FIELD_REFERENCE:
	  {
	    Expression* strct = e->field_reference_expression()->expr();
	    if (strct->heap_expression() != NULL)
	      {
		// When accessing the field of a struct reference, we lose
		// track of the indirection.
		dst = this->context_->sink();
		break;
	      }

	    // Treat DST.x = SRC as if it were DST = SRC.
	    Node* struct_node = Node::make_node(strct);
	    this->assign(struct_node, src);
	    return;
	  }

	case Expression::EXPRESSION_ARRAY_INDEX:
	  {
	    Array_index_expression* are = e->array_index_expression();
	    if (!are->array()->type()->is_slice_type())
	      {
		// Treat DST[i] = SRC as if it were DST = SRC if DST if a fixed
		// array.
		Node* array_node = Node::make_node(are->array());
		this->assign(array_node, src);
		return;
	      }

	    // Lose track of the slice dereference.
	    dst = this->context_->sink();
	  }
	  break;

	case Expression::EXPRESSION_UNARY:
	  // Lose track of the dereference.
	  if (e->unary_expression()->op() == OPERATOR_MULT)
	    dst = this->context_->sink();
	  break;

	case Expression::EXPRESSION_MAP_INDEX:
	  {
	    // Lose track of the map's key and value.
	    Expression* index = e->map_index_expression()->index();
	    Node* index_node = Node::make_node(index);
	    this->assign(this->context_->sink(), index_node);

	    dst = this->context_->sink();
	  }
	  break;

	default:
	  // TODO(cmang): Add debugging info here: only a few expressions
	  // should leave DST unmodified.
	  break;
        }
    }

  if (src->expr() != NULL)
    {
      Expression* e = src->expr();
      switch (e->classification())
        {
	case Expression::EXPRESSION_VAR_REFERENCE:
	  // DST = var
	case Expression::EXPRESSION_HEAP:
	  // DST = &T{...}.
	case Expression::EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
	case Expression::EXPRESSION_SLICE_CONSTRUCTION:
	  // DST = [...]T{...}.
	case Expression::EXPRESSION_MAP_CONSTRUCTION:
	  // DST = map[T]V{...}.
	case Expression::EXPRESSION_STRUCT_CONSTRUCTION:
	  // DST = T{...}.
	case Expression::EXPRESSION_ALLOCATION:
	  // DST = new(T).
	case Expression::EXPRESSION_BOUND_METHOD:
	  // DST = x.M.
	  this->flows(dst, src);
	  break;

	case Expression::EXPRESSION_UNSAFE_CONVERSION:
	  {
	    Expression* underlying = e->unsafe_conversion_expression()->expr();
	    Node* underlying_node = Node::make_node(underlying);
	    this->assign(dst, underlying_node);
	  }
	  break;

	case Expression::EXPRESSION_ENCLOSED_VAR_REFERENCE:
	  {
	    Named_object* var = e->enclosed_var_expression()->variable();
	    Node* var_node = Node::make_node(var);
	    this->flows(dst, var_node);
	  }
	  break;

	case Expression::EXPRESSION_CALL:
	  {
	    Call_expression* call = e->call_expression();
	    Func_expression* fe = call->fn()->func_expression();
	    if (fe != NULL && fe->is_runtime_function())
	      {
		switch (fe->runtime_code())
		  {
		  case Runtime::APPEND:
		    {
		      // Append returns the first argument.
		      // The subsequent arguments are already leaked because
		      // they are operands to append.
		      Node* appendee = Node::make_node(call->args()->front());
		      this->assign(dst, appendee);
		      break;
		    }

		  case Runtime::MAKECHAN:
		  case Runtime::MAKECHANBIG:
		  case Runtime::MAKEMAP:
		  case Runtime::MAKEMAPBIG:
		  case Runtime::MAKESLICE1:
		  case Runtime::MAKESLICE2:
		  case Runtime::MAKESLICE1BIG:
		  case Runtime::MAKESLICE2BIG:
		    // DST = make(...).
		  case Runtime::BYTE_ARRAY_TO_STRING:
		    // DST = string([]byte{...}).
		  case Runtime::INT_ARRAY_TO_STRING:
		    // DST = string([]int{...}).
		  case Runtime::STRING_TO_BYTE_ARRAY:
		    // DST = []byte(str).
		  case Runtime::STRING_TO_INT_ARRAY:
		    // DST = []int(str).
		  case Runtime::STRING_PLUS:
		    // DST = str1 + str2
		  case Runtime::CONSTRUCT_MAP:
		    // When building a map literal's backend representation.
		    // Likely never seen here and covered in
		    // Expression::EXPRESSION_MAP_CONSTRUCTION.
		  case Runtime::INT_TO_STRING:
		    // DST = string(i).
		  case Runtime::IFACEE2E2:
		  case Runtime::IFACEI2E2:
		  case Runtime::IFACEE2I2:
		  case Runtime::IFACEI2I2:
		  case Runtime::IFACEE2T2P:
		  case Runtime::IFACEI2T2P:
		  case Runtime::IFACEE2T2:
		  case Runtime::IFACEI2T2:
		  case Runtime::CONVERT_INTERFACE:
		    // All versions of interface conversion that might result
		    // from a type assertion.  Some of these are the result of
		    // a tuple type assertion statement and may not be covered
		    // by the case in Expression::EXPRESSION_CONVERSION or
		    // Expression::EXPRESSION_TYPE_GUARD.
		    this->flows(dst, src);
		    break;

		  default:
		    break;
		  }
		break;
	      }
	    else if (fe != NULL
		     && fe->named_object()->is_function()
		     && fe->named_object()->func_value()->is_method()
		     && (call->is_deferred()
			 || call->is_concurrent()))
	      {
		// For a method call thunk, lose track of the call and treat it
		// as if DST = RECEIVER.
		Node* rcvr_node = Node::make_node(call->args()->front());
		this->assign(dst, rcvr_node);
		break;
	      }

	    // TODO(cmang): Handle case from issue 4529.
	    // Node* call_node = Node::make_node(e);
	    // Node::Escape_state* call_state = call_node->state(this->context_, NULL);
	    // std::vector<Node*> retvals = call_state->retvals;
	    // for (std::vector<Node*>::const_iterator p = retvals.begin();
	    //	    p != retvals.end();
	    //	    ++p)
	    //	 this->flows(dst, *p);
	  }
	  break;

	case Expression::EXPRESSION_FUNC_REFERENCE:
	  if (e->func_expression()->closure() != NULL)
	    {
	      // If SRC is a reference to a function closure, DST flows into
	      // the underyling closure variable.
	      Expression* closure = e->func_expression()->closure();
	      Node* closure_node = Node::make_node(closure);
	      this->flows(dst, closure_node);
	    }
	  break;

	case Expression::EXPRESSION_FIELD_REFERENCE:
	  {
	    // A non-pointer can't escape from a struct.
	    if (!e->type()->has_pointer())
	      break;
	  }

	case Expression::EXPRESSION_CONVERSION:
	case Expression::EXPRESSION_TYPE_GUARD:
	case Expression::EXPRESSION_ARRAY_INDEX:
	case Expression::EXPRESSION_STRING_INDEX:
	  {
	    Expression* left = NULL;
	    if (e->field_reference_expression() != NULL)
	      {
		left = e->field_reference_expression()->expr();
		if (left->unary_expression() != NULL
		    && left->unary_expression()->op() == OPERATOR_MULT)
		  {
		    // DST = (*x).f
		    this->flows(dst, src);
		    break;
		  }
	      }
	    else if (e->conversion_expression() != NULL)
	      left = e->conversion_expression()->expr();
	    else if (e->type_guard_expression() != NULL)
	      left = e->type_guard_expression()->expr();
	    else if (e->array_index_expression() != NULL)
	      {
		Array_index_expression* aie = e->array_index_expression();
		if (e->type()->is_slice_type())
		  left = aie->array();
		else if (!aie->array()->type()->is_slice_type())
		  {
		    // Indexing an array preserves the input value.
		    Node* array_node = Node::make_node(aie->array());
		    this->assign(dst, array_node);
		    break;
		  }
		else
		  {
		    this->flows(dst, src);
		    break;
		  }
	      }
	    else if (e->string_index_expression() != NULL)
	      {
		String_index_expression* sie = e->string_index_expression();
		if (e->type()->is_slice_type())
		  left = sie->string();
		else if (!sie->string()->type()->is_slice_type())
		  {
		    // Indexing a string preserves the input value.
		    Node* string_node = Node::make_node(sie->string());
		    this->assign(dst, string_node);
		    break;
		  }
		else
		  {
		    this->flows(dst, src);
		    break;
		  }
	      }
	    go_assert(left != NULL);

	    // Conversions, field access, and slicing all preserve the input
	    // value.
	    Node* left_node = Node::make_node(left);
	    this->assign(dst, left_node);
	  }
	  break;

	case Expression::EXPRESSION_BINARY:
	  {
	    switch (e->binary_expression()->op())
	      {
	      case OPERATOR_PLUS:
	      case OPERATOR_MINUS:
	      case OPERATOR_XOR:
	      case OPERATOR_MULT:
	      case OPERATOR_DIV:
	      case OPERATOR_MOD:
	      case OPERATOR_LSHIFT:
	      case OPERATOR_RSHIFT:
	      case OPERATOR_AND:
	      case OPERATOR_BITCLEAR:
		{
		  Node* left = Node::make_node(e->binary_expression()->left());
		  this->assign(dst, left);
		  Node* right = Node::make_node(e->binary_expression()->right());
		  this->assign(dst, right);
		}
		break;

	      default:
		break;
	      }
	  }
	  break;

	case Expression::EXPRESSION_UNARY:
	  {
	    switch (e->unary_expression()->op())
	      {
	      case OPERATOR_PLUS:
	      case OPERATOR_MINUS:
	      case OPERATOR_XOR:
		{
		    Node* op_node =
		      Node::make_node(e->unary_expression()->operand());
		    this->assign(dst, op_node);
		}
		break;

	      case OPERATOR_MULT:
		// DST = *x
	      case OPERATOR_AND:
		// DST = &x
		this->flows(dst, src);
		break;

	      default:
		break;
	      }
	  }
	  break;

	case Expression::EXPRESSION_TEMPORARY_REFERENCE:
	  {
	    Statement* temp = e->temporary_reference_expression()->statement();
	    if (temp != NULL
		&& temp->temporary_statement()->init() != NULL)
	      {
		Expression* init = temp->temporary_statement()->init();
		Node* init_node = Node::make_node(init);
		this->assign(dst, init_node);
	      }
	  }
	  break;

	default:
	  // TODO(cmang): Add debug info here; this should not be reachable.
	  // For now, just to be conservative, we'll just say dst flows to src.
	  break;
	}
    }
}

// Model the assignment of DST to an indirection of SRC.

void
Escape_analysis_assign::assign_deref(Node* dst, Node* src)
{
  if (src->expr() != NULL)
    {
      switch (src->expr()->classification())
        {
	case Expression::EXPRESSION_BOOLEAN:
	case Expression::EXPRESSION_STRING:
	case Expression::EXPRESSION_INTEGER:
	case Expression::EXPRESSION_FLOAT:
	case Expression::EXPRESSION_COMPLEX:
	case Expression::EXPRESSION_NIL:
	case Expression::EXPRESSION_IOTA:
	  // No need to try indirections on literal values
	  // or numeric constants.
	  return;

	default:
	  break;
        }
    }

  this->assign(dst, this->context_->add_dereference(src));
}

// Model the input-to-output assignment flow of one of a function call's
// arguments, where the flow is encoded in NOTE.

int
Escape_analysis_assign::assign_from_note(std::string* note,
					 const std::vector<Node*>& dsts,
					 Node* src)
{
  int enc = Escape_note::parse_tag(note);
  if (src->expr() != NULL)
    {
      switch (src->expr()->classification())
        {
	case Expression::EXPRESSION_BOOLEAN:
	case Expression::EXPRESSION_STRING:
	case Expression::EXPRESSION_INTEGER:
	case Expression::EXPRESSION_FLOAT:
	case Expression::EXPRESSION_COMPLEX:
	case Expression::EXPRESSION_NIL:
	case Expression::EXPRESSION_IOTA:
	  // There probably isn't a note for a literal value.  Literal values
	  // usually don't escape unless we lost track of the value some how.
	  return enc;

	default:
	  break;
        }
    }

  if (enc == Node::ESCAPE_UNKNOWN)
    {
      // Lost track of the value.
      this->assign(this->context_->sink(), src);
      return enc;
    }
  else if (enc == Node::ESCAPE_NONE)
    return enc;

  // If the content inside a parameter (reached via indirection) escapes to
  // the heap, mark it.
  if ((enc & ESCAPE_CONTENT_ESCAPES) != 0)
    this->assign(this->context_->sink(), this->context_->add_dereference(src));

  int save_enc = enc;
  enc >>= ESCAPE_RETURN_BITS;
  for (std::vector<Node*>::const_iterator p = dsts.begin();
       enc != 0 && p != dsts.end();
       ++p)
    {
      // Prefer the lowest-level path to the reference (for escape purposes).
      // Two-bit encoding (for example. 1, 3, and 4 bits are other options)
      //  01 = 0-level
      //  10 = 1-level, (content escapes),
      //  11 = 2-level, (content of content escapes).
      int bits = enc & ESCAPE_BITS_MASK_FOR_TAG;
      if (bits > 0)
	{
	  Node* n = src;
	  for (int i = 0; i < bits - 1; ++i)
	    {
	      // Encode level > 0 as indirections.
	      n = this->context_->add_dereference(n);
	    }
	  this->assign(*p, n);
	}
      enc >>= ESCAPE_BITS_PER_OUTPUT_IN_TAG;
    }

  // If there are too many outputs to fit in the tag, that is handled on the
  // encoding end as ESCAPE_HEAP, so there is no need to check here.
  return save_enc;
}

// Record the flow of SRC to DST in DST.

void
Escape_analysis_assign::flows(Node* dst, Node* src)
{
  // Don't bother capturing the flow from scalars.
  if (src->expr() != NULL
      && !src->expr()->type()->has_pointer())
    return;

  // Don't confuse a blank identifier with the sink.
  if (dst->is_sink() && dst != this->context_->sink())
    return;

  Node::Escape_state* dst_state = dst->state(this->context_, NULL);
  Node::Escape_state* src_state = src->state(this->context_, NULL);
  if (dst == src
      || dst_state == src_state
      || dst_state->flows.find(src) != dst_state->flows.end()
      || src_state->flows.find(dst) != src_state->flows.end())
    return;

  if (dst_state->flows.empty())
    this->context_->add_dst(dst);

  dst_state->flows.insert(src);
}

// Build a connectivity graph between nodes in the function being analyzed.

void
Gogo::assign_connectivity(Escape_context* context, Named_object* fn)
{
  // Must be defined outside of this package.
  if (!fn->is_function())
    return;

  int save_depth = context->loop_depth();
  context->set_loop_depth(1);

  Escape_analysis_assign ea(context, fn);
  Function::Results* res = fn->func_value()->result_variables();
  if (res != NULL)
    {
      for (Function::Results::const_iterator p = res->begin();
	   p != res->end();
	   ++p)
	{
	  Node* res_node = Node::make_node(*p);
	  Node::Escape_state* res_state = res_node->state(context, fn);
	  res_state->loop_depth = 0;

	  // If this set of functions is recursive, we lose track of the return values.
	  // Just say that the result flows to the sink.
	  if (context->recursive())
	    ea.flows(context->sink(), res_node);
	}
    }

  const Bindings* callee_bindings = fn->func_value()->block()->bindings();
  Function_type* fntype = fn->func_value()->type();
  Typed_identifier_list* params = (fntype->parameters() != NULL
				   ? fntype->parameters()->copy()
				   : new Typed_identifier_list);
  if (fntype->receiver() != NULL)
    params->push_back(*fntype->receiver());

  for (Typed_identifier_list::const_iterator p = params->begin();
       p != params->end();
       ++p)
    {
      if (p->name().empty() || Gogo::is_sink_name(p->name()))
	continue;

      Named_object* param_no = callee_bindings->lookup_local(p->name());
      go_assert(param_no != NULL);
      Node* param_node = Node::make_node(param_no);
      Node::Escape_state* param_state = param_node->state(context, fn);
      param_state->loop_depth = 1;

      if (!p->type()->has_pointer())
        continue;

      // External function?  Parameters must escape unless //go:noescape is set.
      // TODO(cmang): Implement //go:noescape directive.
      if (fn->package() != NULL)
	param_node->set_encoding(Node::ESCAPE_HEAP);
      else
	param_node->set_encoding(Node::ESCAPE_NONE);

      // TODO(cmang): Track this node in no_escape list.
    }

  Escape_analysis_loop el;
  fn->func_value()->traverse(&el);

  fn->func_value()->traverse(&ea);
  context->set_loop_depth(save_depth);
}

class Escape_analysis_flood
{
 public:
  Escape_analysis_flood(Escape_context* context)
    : context_(context)
  { }

  // Use the escape information in dst to update the escape information in src
  // and src's upstream.
  void
  flood(Level, Node* dst, Node* src, int);

 private:
  // The escape context for the group of functions being flooded.
  Escape_context* context_;
};

// Whenever we hit a dereference node, the level goes up by one, and whenever
// we hit an address-of, the level goes down by one. as long as we're on a
// level > 0 finding an address-of just means we're following the upstream
// of a dereference, so this address doesn't leak (yet).
// If level == 0, it means the /value/ of this node can reach the root of this
// flood so if this node is an address-of, its argument should be marked as
// escaping iff its current function and loop depth are different from the
// flood's root.
// Once an object has been moved to the heap, all of its upstream should be
// considered escaping to the global scope.
// This is an implementation of gc/esc.go:escwalkBody.

void
Escape_analysis_flood::flood(Level level, Node* dst, Node* src,
			     int extra_loop_depth)
{
  // No need to flood src if it is a literal.
  if (src->expr() != NULL)
    {
      switch (src->expr()->classification())
        {
	case Expression::EXPRESSION_BOOLEAN:
	case Expression::EXPRESSION_STRING:
	case Expression::EXPRESSION_INTEGER:
	case Expression::EXPRESSION_FLOAT:
	case Expression::EXPRESSION_COMPLEX:
	case Expression::EXPRESSION_NIL:
	case Expression::EXPRESSION_IOTA:
	  return;

	default:
	  break;
        }
    }

  Node::Escape_state* src_state = src->state(this->context_, NULL);
  if (src_state->flood_id == this->context_->flood_id())
    {
      // Esclevels are vectors, do not compare as integers,
      // and must use "min" of old and new to guarantee
      // convergence.
      level = level.min(src_state->level);
      if (level == src_state->level)
	{
	  // Have we been here already with an extraloopdepth,
	  // or is the extraloopdepth provided no improvement on
	  // what's already been seen?
	  if (src_state->max_extra_loop_depth >= extra_loop_depth
	      || src_state->loop_depth >= extra_loop_depth)
	    return;
	  src_state->max_extra_loop_depth = extra_loop_depth;
	}
    }
  else
    src_state->max_extra_loop_depth = -1;

  src_state->flood_id = this->context_->flood_id();
  src_state->level = level;
  int mod_loop_depth = std::max(extra_loop_depth, src_state->loop_depth);

  this->context_->increase_pdepth();

  // Input parameter flowing into output parameter?
  Named_object* src_no = NULL;
  if (src->expr() != NULL && src->expr()->var_expression() != NULL)
    src_no = src->expr()->var_expression()->named_object();
  else
    src_no = src->object();
  bool src_is_param = (src_no != NULL
		       && src_no->is_variable()
		       && src_no->var_value()->is_parameter());

  Named_object* dst_no = NULL;
  if (dst->expr() != NULL && dst->expr()->var_expression() != NULL)
    dst_no = dst->expr()->var_expression()->named_object();
  else
    dst_no = dst->object();
  bool dst_is_result = dst_no != NULL && dst_no->is_result_variable();

  if (src_is_param
      && dst_is_result
      && (src->encoding() & ESCAPE_MASK) < int(Node::ESCAPE_SCOPE)
      && dst->encoding() != Node::ESCAPE_HEAP)
    {
      // This case handles:
      // 1. return in
      // 2. return &in
      // 3. tmp := in; return &tmp
      // 4. return *in
      if ((src->encoding() & ESCAPE_MASK) != Node::ESCAPE_RETURN)
	{
	  int enc =
	    Node::ESCAPE_RETURN | (src->encoding() & ESCAPE_CONTENT_ESCAPES);
	  src->set_encoding(enc);
	}

      int enc = Node::note_inout_flows(src->encoding(),
				       dst_no->result_var_value()->index(),
				       level);
      src->set_encoding(enc);

      // In gc/esc.go:escwalkBody, this is a goto to the label for recursively
      // flooding the connection graph.  Inlined here for convenience.
      level = level.copy();
      for (std::set<Node*>::const_iterator p = src_state->flows.begin();
	   p != src_state->flows.end();
	   ++p)
	this->flood(level, dst, *p, extra_loop_depth);
      return;
    }

  // If parameter content escape to heap, set ESCAPE_CONTENT_ESCAPES.
  // Note minor confusion around escape from pointer-to-struct vs
  // escape from struct.
  if (src_is_param
      && dst->encoding() == Node::ESCAPE_HEAP
      && (src->encoding() & ESCAPE_MASK) < int(Node::ESCAPE_SCOPE)
      && level.value() > 0)
    {
      int enc =
	Node::max_encoding((src->encoding() | ESCAPE_CONTENT_ESCAPES),
			   Node::ESCAPE_NONE);
      src->set_encoding(enc);
    }

  // A src object leaks if its value or address is assigned to a dst object
  // in a different scope (at a different loop depth).
  Node::Escape_state* dst_state = dst->state(this->context_, NULL);
  bool src_leaks = (level.value() <= 0
		    && level.suffix_value() <= 0
		    && dst_state->loop_depth < mod_loop_depth);

  if (src_is_param
      && (src_leaks || dst_state->loop_depth < 0)
      && (src->encoding() & ESCAPE_MASK) < int(Node::ESCAPE_SCOPE))
    {
      if (level.suffix_value() > 0)
	{
	  int enc =
	    Node::max_encoding((src->encoding() | ESCAPE_CONTENT_ESCAPES),
			       Node::ESCAPE_NONE);
	  src->set_encoding(enc);
	}
      else
	{
	  src->set_encoding(Node::ESCAPE_SCOPE);
	}
    }
  else if (src->expr() != NULL)
    {
      Expression* e = src->expr();
      if (e->enclosed_var_expression() != NULL)
	{
	  Node* enclosed_node =
	    Node::make_node(e->enclosed_var_expression()->variable());
	  this->flood(level, dst, enclosed_node, -1);
	}
      else if (e->heap_expression() != NULL
	  || (e->unary_expression() != NULL
	      && e->unary_expression()->op() == OPERATOR_AND))
	{
	  // Pointer literals and address-of expressions.
	  Expression* underlying;
	  if (e->heap_expression())
	    underlying = e->heap_expression()->expr();
	  else
	    underlying = e->unary_expression()->operand();
	  Node* underlying_node = Node::make_node(underlying);

	  // If the address leaks, the underyling object must be moved
	  // to the heap.
	  underlying->address_taken(src_leaks);
	  if (src_leaks)
	    {
	      src->set_encoding(Node::ESCAPE_HEAP);
	      this->flood(level.decrease(), dst,
			  underlying_node, mod_loop_depth);
	      extra_loop_depth = mod_loop_depth;
	    }
	  else
	    {
	      // Decrease the level each time we take the address of the object.
	      this->flood(level.decrease(), dst, underlying_node, -1);
	    }
	}
      else if (e->slice_literal() != NULL)
	{
	  Slice_construction_expression* slice = e->slice_literal();
	  if (slice->vals() != NULL)
	    {
	      for (Expression_list::const_iterator p = slice->vals()->begin();
		   p != slice->vals()->end();
		   ++p)
		{
		  if ((*p) != NULL)
		    this->flood(level.decrease(), dst, Node::make_node(*p), -1);
		}
	    }
	  if (src_leaks)
	    {
	      src->set_encoding(Node::ESCAPE_HEAP);
	      extra_loop_depth = mod_loop_depth;
	    }
	}
      else if (e->call_expression() != NULL)
	{
	  Call_expression* call = e->call_expression();
	  if (call->fn()->func_expression() != NULL)
	    {
	      Func_expression* func = call->fn()->func_expression();
	      if (func->is_runtime_function())
		{
		  switch (func->runtime_code())
		    {
		    case Runtime::APPEND:
		      {
			// Propagate escape information to appendee.
			Expression* appendee = call->args()->front();
			this->flood(level, dst, Node::make_node(appendee), -1);
		      }
		      break;

		    case Runtime::MAKECHAN:
		    case Runtime::MAKECHANBIG:
		    case Runtime::MAKEMAP:
		    case Runtime::MAKEMAPBIG:
		    case Runtime::MAKESLICE1:
		    case Runtime::MAKESLICE2:
		    case Runtime::MAKESLICE1BIG:
		    case Runtime::MAKESLICE2BIG:
		    case Runtime::BYTE_ARRAY_TO_STRING:
		    case Runtime::INT_ARRAY_TO_STRING:
		    case Runtime::STRING_TO_BYTE_ARRAY:
		    case Runtime::STRING_TO_INT_ARRAY:
		    case Runtime::STRING_PLUS:
		    case Runtime::CONSTRUCT_MAP:
		    case Runtime::INT_TO_STRING:
		    case Runtime::CONVERT_INTERFACE:
		      // All runtime calls that involve allocation of memory
		      // except new.  Runtime::NEW gets lowered into an
		      // allocation expression.
		      if (src_leaks)
			{
			  src->set_encoding(Node::ESCAPE_HEAP);
			  extra_loop_depth = mod_loop_depth;
			}
		      break;

		    default:
		      break;
		    }
		}
	      else if (src_leaks
		       && (func->closure() != NULL
			   || func->bound_method_expression() != NULL))
		{
		  // A closure or bound method; we lost track of actual function
		  // so if this leaks, this call must be done on the heap.
		  src->set_encoding(Node::ESCAPE_HEAP);
		}
	    }
	}
      else if (e->allocation_expression() != NULL && src_leaks)
	{
	  // Calls to Runtime::NEW get lowered into an allocation expression.
	  src->set_encoding(Node::ESCAPE_HEAP);
	}
      else if ((e->field_reference_expression() != NULL
		&& e->field_reference_expression()->expr()->unary_expression() == NULL)
	       || e->type_guard_expression() != NULL
	       || (e->array_index_expression() != NULL
		   && e->type()->is_slice_type())
	       || (e->string_index_expression() != NULL
		   && e->type()->is_slice_type()))
	{
	  Expression* underlying;
	  if (e->field_reference_expression() != NULL)
	    underlying = e->field_reference_expression()->expr();
	  else if (e->type_guard_expression() != NULL)
	    underlying = e->type_guard_expression()->expr();
	  else if (e->array_index_expression() != NULL)
	    underlying = e->array_index_expression()->array();
	  else
	    underlying = e->string_index_expression()->string();

	  Node* underlying_node = Node::make_node(underlying);
	  this->flood(level, dst, underlying_node, -1);
	}
      else if ((e->field_reference_expression() != NULL
		&& e->field_reference_expression()->expr()->unary_expression() != NULL)
	       || e->array_index_expression() != NULL
	       || e->map_index_expression() != NULL
	       || (e->unary_expression() != NULL
		   && e->unary_expression()->op() == OPERATOR_MULT))
	{
	  Expression* underlying;
	  if (e->field_reference_expression() != NULL)
	    {
	      underlying = e->field_reference_expression()->expr();
	      underlying = underlying->unary_expression()->operand();
	    }
	  else if (e->array_index_expression() != NULL)
	    {
	      underlying = e->array_index_expression()->array();
	      if (!underlying->type()->is_slice_type())
		{
		  Node* underlying_node = Node::make_node(underlying);
		  this->flood(level, dst, underlying_node, 1);
		}
	    }
	  else if (e->map_index_expression() != NULL)
	    underlying = e->map_index_expression()->map();
	  else
	    underlying = e->unary_expression()->operand();

	  // Increase the level for a dereference.
	  Node* underlying_node = Node::make_node(underlying);
	  this->flood(level.increase(), dst, underlying_node, -1);
	}

      // TODO(cmang): Add case for Issue #10466.
    }

  level = level.copy();
  for (std::set<Node*>::const_iterator p = src_state->flows.begin();
       p != src_state->flows.end();
       ++p)
    this->flood(level, dst, *p, extra_loop_depth);

  this->context_->decrease_pdepth();
}

// Propagate escape information across the nodes modeled in this Analysis_set.
// This is an implementation of gc/esc.go:escflood.

void
Gogo::propagate_escape(Escape_context* context, Node* dst)
{
  Node::Escape_state* state = dst->state(context, NULL);
  Escape_analysis_flood eaf(context);
  for (std::set<Node*>::const_iterator p = state->flows.begin();
       p != state->flows.end();
       ++p)
    {
      context->increase_flood_id();
      eaf.flood(Level::From(0), dst, *p, -1);
    }
}

class Escape_analysis_tag
{
 public:
  Escape_analysis_tag(Escape_context* context)
    : context_(context)
  { }

  // Add notes to the function's type about the escape information of its
  // input parameters.
  void
  tag(Named_object* fn);

 private:
  Escape_context* context_;
};

void
Escape_analysis_tag::tag(Named_object* fn)
{
  // External functions are assumed unsafe
  // unless //go:noescape is given before the declaration.
  if (fn->package() != NULL || !fn->is_function())
    {
      // TODO(cmang): Implement //go:noescape directive for external functions;
      // mark input parameters as not escaping.
      return;
    }

  Function_type* fntype = fn->func_value()->type();
  Bindings* bindings = fn->func_value()->block()->bindings();

  if (fntype->is_method()
      && !fntype->receiver()->name().empty()
      && !Gogo::is_sink_name(fntype->receiver()->name()))
    {
      Named_object* rcvr_no = bindings->lookup(fntype->receiver()->name());
      go_assert(rcvr_no != NULL);
      Node* rcvr_node = Node::make_node(rcvr_no);
      switch ((rcvr_node->encoding() & ESCAPE_MASK))
	{
	case Node::ESCAPE_NONE: // not touched by flood
	case Node::ESCAPE_RETURN:
	  if (fntype->receiver()->type()->has_pointer())
	    // Don't bother tagging for scalars.
	    fntype->add_receiver_note(rcvr_node->encoding());
	  break;

	case Node::ESCAPE_HEAP: // flooded, moved to heap.
	case Node::ESCAPE_SCOPE: // flooded, value leaves scope.
	  break;

	default:
	  break;
	}
    }

  int i = 0;
  if (fntype->parameters() != NULL)
    {
      const Typed_identifier_list* til = fntype->parameters();
      for (Typed_identifier_list::const_iterator p = til->begin();
	   p != til->end();
	   ++p, ++i)
	{
	  if (p->name().empty() || Gogo::is_sink_name(p->name()))
	    continue;

	  Named_object* param_no = bindings->lookup(p->name());
	  go_assert(param_no != NULL);
	  Node* param_node = Node::make_node(param_no);
	  switch ((param_node->encoding() & ESCAPE_MASK))
	    {
	    case Node::ESCAPE_NONE: // not touched by flood
	    case Node::ESCAPE_RETURN:
	      if (p->type()->has_pointer())
		// Don't bother tagging for scalars.
		fntype->add_parameter_note(i, param_node->encoding());
	      break;

	    case Node::ESCAPE_HEAP: // flooded, moved to heap.
	    case Node::ESCAPE_SCOPE: // flooded, value leaves scope.
	      break;

	    default:
	      break;
	    }
	}
    }
  fntype->set_is_tagged();
}

// Tag each top-level function with escape information that will be used to
// retain analysis results across imports.

void
Gogo::tag_function(Escape_context* context, Named_object* fn)
{
  Escape_analysis_tag eat(context);
  eat.tag(fn);
}
