// escape.h -- Go escape analysis (based on Go compiler algorithm).

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_ESCAPE_H
#define GO_ESCAPE_H

#include "gogo.h"

class Named_object;
class Expression;
class Statement;
class Escape_context;

// There can be loops in the escape graph that lead to arbitrary recursions.
// See comment in gc/esc.go.
static const int MIN_LEVEL = -2;

// Level models the escapement of a Node using two integers that are computed
// by backwards-analyzing the flow of a function from its sink and increasing or
// decreasing based on dereferences and addressing, respectively.
// One integer, known as the level's VALUE (think absolute value), is just the
// sum of indirections (via referencing or dereferencing) applied to the Node.
// The second, known as the level's SUFFIX_VALUE, is the amount of indirections
// applied after some data has been copied from the node.  When accessing a
// field F of an object O and then applying indirections, for example, the field
// access O.F is assumed to copy that data from O before applying indirections.
// With this, even if O.F escapes, it might mean that the content of O escape,
// but not the object O itself.

class Level
{
public:
  Level()
    : value_(0), suffix_value_(0)
  { }

  Level(int value, int suffix)
    : value_(value), suffix_value_(suffix)
  { }

  // Return this level's value.
  int
  value() const
  { return this->value_; }

  // Return this level's suffix value.
  int
  suffix_value() const
  { return this->suffix_value_; }

  // Increase the level because a node is referenced.
  Level
  increase() const
  {
    if (this->value_ <= MIN_LEVEL)
      return Level(MIN_LEVEL, 0);

    return Level(this->value_ + 1, this->suffix_value_ + 1);
  }

  // Decrease the level because a node is dereferenced.
  Level
  decrease() const
  {
    if (this->value_ <= MIN_LEVEL)
      return Level(MIN_LEVEL, 0);

    return Level(this->value_ - 1, this->suffix_value_ - 1);
  }

  // Model a node being copied.
  Level
  copy() const
  {
    return Level(this->value_, std::max(this->suffix_value_, 0));
  }

  // Return a level with the minimum values of this level and l.
  Level
  min(const Level& l) const
  {
    return Level(std::min(this->value_, l.value()),
                 std::min(this->suffix_value_, l.suffix_value()));
  }

  // Compare two levels for equality.
  bool
  operator==(const Level& l) const
  {
    return (this->value_ == l.value()
	    && this->suffix_value_ == l.suffix_value());
  }

  // Create a level from an integer value.
  static Level
  From(int i)
  {
    if (i <= MIN_LEVEL)
      return Level(MIN_LEVEL, 0);
    return Level(i, 0);
  }

private:
  // The sum of all indirects (-1) and references (+1) applied to a Node.
  int value_;
  // The sum of all indirects (-1) abd references (+1) applied to a copied Node.
  int suffix_value_;
};

// A node in the escape graph.  This node is an alias to a particular node
// in the Go parse tree.  Specifically, it can represent an expression node,
// a statement node, or a named object node (a variable or function).

class Node
{
 public:
  // This classification represents type of nodes in the Go parse tree that are
  // interesting during the analysis.
  enum Node_classification
    {
      NODE_OBJECT,
      NODE_EXPRESSION,
      NODE_STATEMENT
    };

  // The state necessary to keep track of how a node escapes.
  struct Escape_state
  {
    // The current function.
    Named_object* fn;
    // A list of source nodes that flow into this node.
    std::set<Node*> flows;
    // If the node is a function call, the list of nodes returned.
    std::vector<Node*> retvals;
    // The node's loop depth.
    int loop_depth;
    // There is an extra loop depth in the flood phase used to account for
    // variables referenced across closures.  This is the maximum value of the
    // extra loop depth seen during the flood that touches this node.
    int max_extra_loop_depth;
    // The node's level.
    Level level;
    // An ID given to a node when it is encountered as a flow from the current
    // dst node.  This is used to avoid infinite recursion of cyclic nodes.
    int flood_id;

    Escape_state()
      : fn(NULL), loop_depth(0), max_extra_loop_depth(0), flood_id(0)
    { }
  };

  // Note: values in this enum appear in export data, and therefore MUST NOT
  // change.
  enum Escapement_encoding
  {
    ESCAPE_UNKNOWN,
    // Does not escape to heap, result, or parameters.
    ESCAPE_NONE,
    // Is returned or reachable from a return statement.
    ESCAPE_RETURN,
    // Allocated in an inner loop, assigned to an outer loop,
    // which allows construction of non-escaping but arbitrarily large linked
    // data structures (i.e., not eligible for allocation in a fixed-size stack
    // stack frame).
    ESCAPE_SCOPE,
    // Reachable from the heap.
    ESCAPE_HEAP,
    // By construction will not escape.
    ESCAPE_NEVER
  };

  // Multiple constructors for each classification.
  Node(Named_object* no)
    : classification_(NODE_OBJECT), state_(NULL), encoding_(ESCAPE_UNKNOWN)
  { this->u_.object_val = no; }

  Node(Expression* e)
    : classification_(NODE_EXPRESSION), state_(NULL), encoding_(ESCAPE_UNKNOWN)
  { this->u_.expression_val = e; }

  Node(Statement* s)
    : classification_(NODE_STATEMENT), state_(NULL), encoding_(ESCAPE_UNKNOWN)
  { this->u_.statement_val = s; }

  // Return this node's type.
  Type*
  type() const;

  // Return this node's location.
  Location
  location() const;

  // Return this node's escape state.
  Escape_state*
  state(Escape_context* context, Named_object* fn);

  // Return this node's escape encoding.
  int
  encoding() const
  { return this->encoding_; }

  // Set the node's escape encoding.
  void
  set_encoding(int enc);

  // Is this node a sink?
  bool
  is_sink() const;

  // Methods to return the underlying value in the Node union.
  Named_object*
  object() const
  {
    return (this->classification_ == NODE_OBJECT
            ? this->u_.object_val
            : NULL);
  }

  Expression*
  expr() const
  {
    return (this->classification_ == NODE_EXPRESSION
            ? this->u_.expression_val
            : NULL);
  }

  Statement*
  statement() const
  {
    return (this->classification_ == NODE_STATEMENT
            ? this->u_.statement_val
            : NULL);
  }

  // Static creation methods for each value supported in the union.
  static Node*
  make_node(Named_object*);

  static Node*
  make_node(Expression*);

  static Node*
  make_node(Statement*);

  // Return the maximum of an existing escape encoding E and a new
  // escape type.
  static int
  max_encoding(int e, int etype);

 private:
  // The classification of this Node.
  Node_classification classification_;
  // The value union.
  union
  {
    // If NODE_OBJECT.
    Named_object* object_val;
    // If NODE_EXPRESSION.
    Expression* expression_val;
    // If NODE_STATEMENT.
    Statement* statement_val;
  } u_;
  // The node's escape state.
  Escape_state* state_;
  // The node's escape encoding.
  // The encoding:
  // | Return Encoding: (width - ESCAPE_RETURN_BITS) |
  // | Content Escapes bit: 1 |
  // | Escapement_encoding: ESCAPE_BITS |
  int encoding_;

  // Cache all the Nodes created via Node::make_node to make the API simpler.
  static std::map<Named_object*, Node*> objects;
  static std::map<Expression*, Node*> expressions;
  static std::map<Statement*, Node*> statements;
};

// The amount of bits used for the escapement encoding.
static const int ESCAPE_BITS = 3;

// Mask used to extract encoding.
static const int ESCAPE_MASK = (1 << ESCAPE_BITS) - 1;

// Value obtained by indirect of parameter escapes to heap.
static const int ESCAPE_CONTENT_ESCAPES = 1 << ESCAPE_BITS;

// The amount of bits used in encoding of return values.
static const int ESCAPE_RETURN_BITS = ESCAPE_BITS + 1;

// For each output, the number of bits for a tag.
static const int ESCAPE_BITS_PER_OUTPUT_IN_TAG = 3;

// The bit max to extract a single tag.
static const int ESCAPE_BITS_MASK_FOR_TAG = (1 << ESCAPE_BITS_PER_OUTPUT_IN_TAG) - 1;

// The largest level that can be stored in a tag.
static const int ESCAPE_MAX_ENCODED_LEVEL = ESCAPE_BITS_MASK_FOR_TAG - 1;

// A helper for converting escape notes from encoded integers to a
// textual format and vice-versa.

class Escape_note
{
 public:
  // Return the string representation of an escapement encoding.
  static std::string
  make_tag(int encoding);

  // Return the escapement encoding for a string tag.
  static int
  parse_tag(std::string* tag);
};

// The escape context for a set of functions being analyzed.

class Escape_context
{
 public:
  Escape_context(Gogo* gogo, bool recursive);

  // Return the Go IR.
  Gogo*
  gogo() const
  { return this->gogo_; }

  // Return the current function being analyzed.
  Named_object*
  current_function() const
  { return this->current_function_; }

  // Change the function being analyzed.
  void
  set_current_function(Named_object* fn)
  { this->current_function_ = fn; }

  // Return true if this is the context for a mutually recursive set of functions.
  bool
  recursive() const
  { return this->recursive_; }

  // Return the special sink node for this context.
  Node*
  sink()
  { return this->sink_; }

  // Return the current loop depth.
  int
  loop_depth() const
  { return this->loop_depth_; }

  // Increase the loop depth.
  void
  increase_loop_depth()
  { this->loop_depth_++; }

  // Decrease the loop depth.
  void
  decrease_loop_depth()
  { this->loop_depth_--; }

  void
  set_loop_depth(int depth)
  { this->loop_depth_ = depth; }

  // Return the destination nodes encountered in this context.
  const std::set<Node*>&
  dsts() const
  { return this->dsts_; }

  // Add a destination node.
  void
  add_dst(Node* dst)
  { this->dsts_.insert(dst); }

  // Return the nodes initially marked as non-escaping before flooding.
  const std::vector<Node*>&
  non_escaping_nodes() const
  { return this->noesc_; }

  // Initialize the dummy return values for this Node N using the results
  // in FNTYPE.
  void
  init_retvals(Node* n, Function_type* fntype);

  // Return the indirection of Node N.
  Node*
  add_dereference(Node* n);

  // Keep track of possibly non-escaping node N.
  void
  track(Node* n);

  int
  flood_id() const
  { return this->flood_id_; }

  void
  increase_flood_id()
  { this->flood_id_++; }

  int
  pdepth() const
  { return this->pdepth_; }

  void
  increase_pdepth()
  { this->pdepth_++; }

  void
  decrease_pdepth()
  { this->pdepth_--; }

 private:
  // The Go IR.
  Gogo* gogo_;
  // The current function being analyzed.
  Named_object* current_function_;
  // Return whether this is the context for a recursive function or a group of mutually
  // recursive functions.
  bool recursive_;
  // The sink for this escape context.  Nodes whose reference objects created
  // outside the current function are assigned to the sink as well as nodes that
  // the analysis loses track of.
  Node* sink_;
  // Used to detect nested loop scopes.
  int loop_depth_;
  // All the destination nodes considered in this set of analyzed functions.
  std::set<Node*> dsts_;
  // All the nodes that were noted as possibly not escaping in this context.
  std::vector<Node*> noesc_;
  // An ID given to each dst and the flows discovered through DFS of that dst.
  // This is used to avoid infinite recursion from nodes that point to each
  // other within the flooding phase.
  int flood_id_;
  // The current level of recursion within a flooded section; used to debug.
  int pdepth_;
};

#endif // !defined(GO_ESCAPE_H)
