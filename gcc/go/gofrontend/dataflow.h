// dataflow.h -- Go frontend dataflow.    -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_DATAFLOW_H
#define GO_DATAFLOW_H

class Expression;
class Named_object;
class Statement;

// Dataflow information about the Go program.

class Dataflow
{
 public:
  // A variable definition.
  struct Def
  {
    // The statement where the variable is defined.
    Statement* statement;
    // The value to which the variable is set.  This may be NULL.
    Expression* val;
    // Whether this is an initialization of the variable.
    bool is_init;
  };

  // A variable reference.
  struct Ref
  {
    // The statement where the variable is referenced.
    Statement* statement;
  };

  // A list of defs.
  typedef std::vector<Def> Defs;

  // A list of refs.
  typedef std::vector<Ref> Refs;

  Dataflow();

  // Initialize the dataflow information.
  void
  initialize(Gogo*);

  // Add a definition of a variable.  STATEMENT assigns a value to
  // VAR.  VAL is the value if it is known, NULL otherwise.
  void
  add_def(Named_object* var, Expression* val, Statement* statement,
	  bool is_init);

  // Add a reference to a variable.  VAR is the variable, and
  // STATEMENT is the statement which refers to it.
  void
  add_ref(Named_object* var, Statement* statement);

  // Return the definitions of VAR--the places where it is set.
  const Defs*
  find_defs(Named_object* var) const;

  // Return the references to VAR--the places where it is used.
  const Refs*
  find_refs(Named_object* var) const;

 private:
  // Order variables in the map.
  struct Compare_vars
  {
    bool
    operator()(const Named_object*, const Named_object*) const;
  };

  // Map from variables to a list of defs of the variable.  We use a
  // map rather than a hash table because the order in which we
  // process variables may affect the resulting code.
  typedef std::map<Named_object*, Defs*, Compare_vars> Defmap;

  // Map from variables to a list of refs to the vairable.
  typedef std::map<Named_object*, Refs*, Compare_vars> Refmap;

  // Variable defs.
  Defmap defs_;
  // Variable refs;
  Refmap refs_;
};


#endif // !defined(GO_DATAFLOW_H)
