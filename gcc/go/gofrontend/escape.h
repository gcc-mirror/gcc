// escape.h -- Go frontend escape analysis.     -*- C++ -*-

// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_ESCAPE_H
#define GO_ESCAPE_H

#include "go-system.h"
#include "string-dump.h"

class Call_node;
class Connection_node;
class Connection_dump_context;
class Gogo;
class Named_object;

// A basic escape analysis implementation for the Go frontend based on the
// algorithm from "Escape Analysis for Java" by Choi et. al in OOPSLA '99.
// This is a simplified version of the flow insensitive analysis with the goal
// of reducing the overhead cost of garbage collection by allocating objects
// on the stack when they do not escape the function they were created in.
//
// A major simplification is that the analysis only implements what Choi refers
// to as "deferred edges" which are used to used model assignments that copy
// references from one variable to another e.g. a := b.  It is unnecessary to
// consider assignments to the fields of an object because, in general, if a
// field of an object escapes and must be heap-allocated, there is no way to
// heap-allocate that escaping field without heap-allocating the entire object.
// That is, for some globally escaping object GVAR, if there is an assignment
// of the form GVAR = t.f such that field f of object t escapes, it is likely
// that t must be heap-allocated as well.  In the analysis, this assignment
// will be simplified to GVAR = t, which is imprecise but has the same effect.

// This is a general graph node representing a named object used in a call graph
// or connection graph.  In a call graph, each named object is either a Function
// or Function_declaration representing a function called during the program
// execution (which isn't necessarily every function declared).  In a connection
// graph, there is a node for each node in the call graph, which forms the root
// of that connection graph.  Each connection graph root contains nodes whose
// objects are either variables used in the function defintion or are nested
// closures created within the function definition.  The connection graph is
// a way of modeling the connectivity between all objects created in a given
// function as well as understanding the relationship between input arguments
// in the caller and the formal parameters in the callee.

class Node
{
 public:
  enum Node_classification
  {
    NODE_CALL,
    NODE_CONNECTION
  };

  virtual ~Node();

  // Make a call node for FUNCTION.
  static Node*
  make_call(Named_object* function);

  // Make a connection node for OBJECT.
  // Note: values in this enum appear in export data, and therefore MUST NOT
  // change.
  enum Escapement_lattice
  {
    // ESCAPE_GLOBAL means that the object escapes all functions globally.
    ESCAPE_GLOBAL = 0,
    // ESCAPE_ARG with respect to a function means that the object escapes that
    // function it is created in via the function's arguments or results.
    ESCAPE_ARG = 1,
    // ESCAPE_NONE means that the object does not escape the function in which
    // it was created.
    ESCAPE_NONE = 2
  };

  // A list of states usually corresponding to a list of function parameters.
  typedef std::vector<Escapement_lattice> Escape_states;

  static Node*
  make_connection(Named_object* object, Escapement_lattice e);

  // Return the node classification.
  Node_classification
  classification() const
  { return this->classification_; }

  // Return whether this is a call node.
  bool
  is_call() const
  { return this->classification_ == NODE_CALL; }

  // Return whether this is a connection node.
  bool
  is_connection() const
  { return this->classification_ == NODE_CONNECTION; }

  // If this is a connection node, return the Connection_node.
  // Otherwise, return NULL.
  Connection_node*
  connection_node()
  { return this->convert<Connection_node, NODE_CONNECTION>(); }

  // Return this node's unique id.
  unsigned int
  id() const
  { return this->id_; }

  // Return this node's generated name for GraphViz.
  virtual const std::string&
  name() = 0;

  // Return this node's generated label for GraphViz.
  virtual const std::string&
  label();

  // Return the object this node represents.
  Named_object*
  object() const
  { return this->object_; }

  void
  add_edge(Node* v)
  { this->edges_.insert(v); }

  const std::set<Node*>&
  edges() const
  { return this->edges_; }

 protected:
  Node(Node_classification, Named_object* object);

  const std::string&
  get_name() const
  { return this->name_; }

  void
  set_name(const std::string& name)
  { this->name_ = name; }

  const std::string&
  get_label() const
  { return this->label_; }

  void
  set_label(const std::string& label)
  { this->label_ = label; }

 private:
  template<typename Node_class,
	   Node_classification node_classification>
  const Node_class*
  convert() const
  {
    return (this->classification_ == node_classification
	    ? static_cast<const Node_class*>(this)
	    : NULL);
  }

  template<typename Node_class,
	   Node_classification node_classification>
  Node_class*
  convert()
  {
    return (this->classification_ == node_classification
	    ? static_cast<Node_class*>(this)
	    : NULL);
  }

  // The classification of this node.
  Node_classification classification_;
  // A unique ID for this node.
  unsigned int id_;
  // The name for this node e.g. "Node<ID>" used as a GraphViz identifier.
  std::string name_;
  // The label for this node in the GraphViz representation.
  std::string label_;
  // The object represented by this node.
  Named_object* object_;
  // A distinct set of nodes that this node has edges to.
  std::set<Node*> edges_;
};


// A node representing a function that might be called during program execution.

class Call_node : public Node
{
 public:
  Call_node(Named_object* function);

  const std::string&
  name();
};

// A node representing an object in the connection graph built for each function
// in the call graph.

class Connection_node : public Node
{
 public:
  Connection_node(Named_object* object, Escapement_lattice e)
    : Node(NODE_CONNECTION, object),
      escape_state_(e)
  { }

  // Return this node's generated name for GraphViz.
  const std::string&
  name();

  // Return this node's generated label for GraphViz.
  const std::string&
  label();

  // Return the escape state for this node.
  Escapement_lattice
  escape_state() const
  { return this->escape_state_; }

  // Set the escape state for this node.
  void
  set_escape_state(Escapement_lattice e)
  { this->escape_state_ = e; }

  // Return the objects inside of this connection graph.
  // This is empty for all connection nodes that are not the root of a
  // connection graph.  Each node in the call graph is a root of a connection
  // graph.
  const std::set<Node*>&
  objects() const
  { return this->objects_; }

  void
  add_object(Node* object)
  { this->objects_.insert(object); }

  void
  dump_connection(Connection_dump_context*);

 private:
  // The escapement of this node.
  Escapement_lattice escape_state_;
  // The set of nodes contained within this connection node.  If this node is
  // not a root of a connection graph, this will be empty.
  std::set<Node*> objects_;
};

// This class implements fgo-dump-calls. The Call graph dump of a Go program.

class Call_dump_context : public String_dump
{
 public:
  Call_dump_context(std::ostream* out = NULL);

  // Initialize the dump context.
  void
  dump(Gogo*, const char* basename);

  // Get dump output stream.
  std::ostream&
  ostream()
  { return *this->ostream_; }

  // Implementation of String_dump interface.
  void
  write_c_string(const char*);

  void
  write_string(const std::string&);

 private:
  // Stream on output dump file.
  std::ostream* ostream_;

  Gogo* gogo_;
};

// This class implements fgo-dump-conns.  The connection graph dump of
// the functions called in a Go program.

class Connection_dump_context : public String_dump
{
 public:
  Connection_dump_context(std::ostream* out = NULL);

  // Initialize the dump context.
  void
  dump(Gogo*, const char* basename);

  // Get dump output stream.
  std::ostream&
  ostream()
  { return *this->ostream_; }

  // Implementation of String_dump interface.
  void
  write_c_string(const char*);

  void
  write_string(const std::string&);

 private:
  // Stream on output dump file.
  std::ostream* ostream_;

  Gogo* gogo_;
};

#endif // !defined(GO_ESCAPE_H)
