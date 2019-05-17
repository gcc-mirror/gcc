// string-dump.h -- Abstract base class for dumping strings.    -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_STRING_DUMP_H
#define GO_STRING_DUMP_H

// This abstract class provides an interface strings for whatever purpose.
// Used for example for exporting and dumping objects.

class String_dump
{
 public:
  // Write a string. Implements the String_dump interface.
  virtual void
  write_string(const std::string& s) = 0;

  // Implementors should override this member, to dump a formatted c string.
  virtual void
  write_c_string(const char*) = 0;
};

#endif  // GO_STRING_DUMP_H
