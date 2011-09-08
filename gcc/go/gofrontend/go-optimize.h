// go-optimize.h -- Go frontend optimizer flags.    -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_OPTIMIZE_H
#define GO_OPTIMIZE_H

// This class manages different arguments to -fgo-optimize-XXX.  If you
// want to create a new optimization, create a variable of this type with the
// name to use for XXX.  You can then use is_enabled to see whether
// the -fgo-optimize-XXX option was used on the command line.

class Go_optimize
{
 public:
  Go_optimize(const char* name);

  // Whether this optimizaiton was enabled.
  bool
  is_enabled() const
  { return this->is_enabled_; }

  // Enable an optimization by name.  Return true if found.
  static bool
  enable_by_name(const char*);

 private:
  // The next optimize flag.  These are not in any order.
  Go_optimize* next_;
  // The name of this optimization pass.
  const char* name_;
  // Whether this dump was enabled.
  bool is_enabled_;
};

#endif // !defined(GO_OPTIMIZE_H)
