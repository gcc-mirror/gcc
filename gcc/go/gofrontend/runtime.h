// runtime.h -- runtime functions called by generated code  -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_RUNTIME_H
#define GO_RUNTIME_H

class Gogo;
class Type;
class Named_object;
class Call_expression;

class Runtime
{
 public:

  // The runtime functions which may be called by generated code.
  enum Function
  {

#define DEF_GO_RUNTIME(CODE, NAME, PARAMS, RESULTS) CODE ,

#include "runtime.def"

#undef DEF_GO_RUNTIME

    // Number of runtime functions.
    NUMBER_OF_FUNCTIONS
  };

  // Make a call to a runtime function.
  static Call_expression*
  make_call(Gogo*, Function, Location, int, ...);

  // Convert all the types used by runtime functions to the backend
  // representation.
  static void
  convert_types(Gogo*);

  // Return the runtime code for a named builtin function.
  static Function
  name_to_code(const std::string&);

 private:
  static Named_object*
  runtime_declaration(Function);
};

#endif // !defined(GO_BUILTINS_H)
