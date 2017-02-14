// go-sha1.h -- GCC specific sha1 checksum utilities.   -*- C++ -*-

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_SHA1_H
#define GO_SHA1_H

#include "go-system.h"

//
// Interface class for computation of SHA1 checksums. Front end requests
// one of these objects from the back end to use for computing
// checksums (each back end tends to have a different SHA1 implementation).
// Back ends are expected to create a new class that derives from this
// one containing an implementation.
//

class Go_sha1_helper
{
 public:
  virtual ~Go_sha1_helper() { }
  virtual void process_bytes(const void* buffer, size_t len) = 0;
  virtual std::string finish() = 0;
  static const int checksum_len = 20;
};

// Call to create and return a new sha1 helper (this routine defined
// by the backend). Caller is responsible for deletion.
extern Go_sha1_helper* go_create_sha1_helper();

#endif // !defined(GO_SHA1_H)
