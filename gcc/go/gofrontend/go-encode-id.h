// go-encode-id.h -- Go identifier encoding utilities  -*- C++ -*-

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_ENCODE_ID_H
#define GO_ENCODE_ID_H

#include "backend.h"

// Given an identifier that will appear in assembly code, this helper
// returns TRUE if the identifier needs special encoding to be used as
// an ASM name, FALSE if the name is OK as is.
extern bool
go_id_needs_encoding(const std::string& str);

// Encodes the specified identifier for ASM name safety, returning a
// string with the encoded value.
extern std::string
go_encode_id(const std::string &id);

// Decodes an encoded ID, returning the original string handed off to
// go_encode_id().
extern std::string
go_decode_id(const std::string &id);

// Returns the empty string if the specified name needs encoding,
// otherwise invokes go_encode_id on the name and returns the result.
extern std::string
go_selectively_encode_id(const std::string &id);

// Encodes a struct tag that appears in a type literal encoding.
extern std::string
go_mangle_struct_tag(const std::string& tag);

// Encode a package path.  A package path can contain any arbitrary
// character, including '.'.  go_encode_id expects that any '.' will
// be inserted by name mangling in a controlled manner.  So first
// translate any '.' using the same .x encoding as used by
// go_mangle_struct_tag.
extern std::string
go_mangle_pkgpath(const std::string& pkgpath);

#endif // !defined(GO_ENCODE_ID_H)
