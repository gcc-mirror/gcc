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

  static Call_expression*
  make_call(Function, source_location, int, ...);

  static void
  convert_types(Gogo*);

  static Type*
  map_iteration_type();

 private:
  static Named_object*
  runtime_declaration(Function);
};

#endif // !defined(GO_BUILTINS_H)
