// escape.h -- Go escape analysis (based on Go compiler algorithm).

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_ESCAPE_H
#define GO_ESCAPE_H

class Named_object;

// The escape context for a set of functions being analyzed.

class Escape_context
{
 public:
  Escape_context(bool recursive)
    : current_function_(NULL), recursive_(recursive)
  { }

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

 private:
  // The current function being analyzed.
  Named_object* current_function_;
  // Return whether this is the context for a recursive function or a group of mutually
  // recursive functions.
  bool recursive_;
};

#endif // !defined(GO_ESCAPE_H)
