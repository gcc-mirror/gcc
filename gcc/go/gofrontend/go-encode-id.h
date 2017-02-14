// go-encode-id.h -- Go identifier encoding utilities  -*- C++ -*-

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_ENCODE_ID_H
#define GO_ENCODE_ID_H

#include "backend.h"

// Given an identifier corresponding to a function or variable,
// this helper returns TRUE if the identifier needs special
// encoding to be used as an ASM name (symbol), FALSE if the name
// is OK as is.
extern bool
go_id_needs_encoding(const std::string& str);

// Encodes the specified identifier for ASM name safety, returning a
// string with the encoded value.
extern std::string
go_encode_id(const std::string &id);

// Returns the empty string if the specified name needs encoding,
// otherwise invokes go_encode_id() on the name and returns the
// result.
extern std::string
go_selectively_encode_id(const std::string &id);

#endif // !defined(GO_ENCODE_ID_H)
