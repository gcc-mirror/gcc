// go-dump.h -- Go frontend debug dumps.    -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_DUMP_H
#define GO_DUMP_H

// This class manages different arguments to -fgo-dump-XXX.  If you
// want to create a new dump, create a variable of this type with the
// name to use for XXX.  You can then use is_enabled to see whether
// the -fgo-dump-XXX option was used on the command line.

class Go_dump
{
 public:
  Go_dump(const char* name);

  // Whether this dump was enabled.
  bool
  is_enabled() const
  { return this->is_enabled_; }

  // Enable a dump by name.  Return true if the dump was found.
  static bool
  enable_by_name(const char*);

 private:
  // The next dump.  These are not in any order.
  Go_dump* next_;
  // The name of this dump.
  const char* name_;
  // Whether this dump was enabled.
  bool is_enabled_;
};

#endif // !defined(GO_DUMP_H)
