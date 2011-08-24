// go-optimize.cc -- Go frontend optimizer flags.

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "go-optimize.h"

namespace {

// The list of optimizations.

Go_optimize* optimizations;

} // End empty namespace.

// Create a new optimization.

Go_optimize::Go_optimize(const char* name)
  : next_(optimizations), name_(name), is_enabled_(false)
{
  optimizations = this;
}

// Enable an optimization by name.

bool
Go_optimize::enable_by_name(const char* name)
{
  bool is_all = strcmp(name, "all") == 0;
  bool found = false;
  for (Go_optimize* p = optimizations; p != NULL; p = p->next_)
    {
      if (is_all || strcmp(name, p->name_) == 0)
	{
	  p->is_enabled_ = true;
	  found = true;
	}
    }
  return found;
}

// Enable an optimization.  Return 1 if this is a real name, 0 if not.

GO_EXTERN_C
int
go_enable_optimize(const char* name)
{
  return Go_optimize::enable_by_name(name) ? 1 : 0;
}
