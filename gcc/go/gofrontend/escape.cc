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

// Class Escape_context.

Escape_context::Escape_context(Gogo* gogo, bool recursive)
  : gogo_(gogo), current_function_(NULL), recursive_(recursive),
    sink_(Node::make_node(Named_object::make_sink())), loop_depth_(0)
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

// Build a connectivity graph between nodes in the function being analyzed.

void
Gogo::assign_connectivity(Escape_context*, Named_object*)
{
  // TODO(cmang): Model the flow analysis of input parameters and results for a
  // function.
  // TODO(cmang): Analyze the current function's body.
}

// Propagate escape information across the nodes modeled in this Analysis_set.

void
Gogo::propagate_escape(Escape_context*, Node*)
{
  // TODO(cmang): Do a breadth-first traversal of a node's upstream, adjusting
  // the Level appropriately.
}


// Tag each top-level function with escape information that will be used to
// retain analysis results across imports.

void
Gogo::tag_function(Escape_context*, Named_object*)
{
  // TODO(cmang): Create escape information notes for each input and output
  // parameter in a given function.
  // Escape_analysis_tag eat(context, fn);
  // this->traverse(&eat);
}
