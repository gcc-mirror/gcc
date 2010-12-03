// go-dump.cc -- Go frontend debug dumps.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "go-dump.h"

namespace {

// The list of dumps.

Go_dump* dumps;

} // End empty namespace.

// Create a new dump.

Go_dump::Go_dump(const char* name)
  : next_(dumps), name_(name), is_enabled_(false)
{
  dumps = this;
}

// Enable a dump by name.

bool
Go_dump::enable_by_name(const char* name)
{
  bool is_all = strcmp(name, "all") == 0;
  bool found = false;
  for (Go_dump* p = dumps; p != NULL; p = p->next_)
    {
      if (is_all || strcmp(name, p->name_) == 0)
	{
	  p->is_enabled_ = true;
	  found = true;
	}
    }
  return found;
}

// Enable a dump.  Return 1 if this is a real name, 0 if not.

GO_EXTERN_C
int
go_enable_dump(const char* name)
{
  return Go_dump::enable_by_name(name) ? 1 : 0;
}
