// rust-linemap.h -- interface to location tracking   -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// Use of this source code is governed by a BSD-style
// license that can be found in the '../go/gofrontend/LICENSE' file.

#ifndef RUST_LINEMAP_H
#define RUST_LINEMAP_H

#include "rust-system.h"

// The backend must define a type named Location which holds
// information about a location in a source file.  The only thing the
// frontend does with instances of Location is pass them back to the
// backend interface.  The Location type must be assignable, and it
// must be comparable: i.e., it must support operator= and operator<.
// The type is normally passed by value rather than by reference, and
// it should support that efficiently.  The type should be defined in
// "rust-location.h".
#include "rust-location.h"

// The Linemap class is a pure abstract interface, plus some static
// convenience functions.  The backend must implement the interface.

/* TODO: probably better to replace linemap implementation as pure abstract
 * interface with some sort of compile-time switch (macros or maybe templates if
 * doable without too much extra annoyance) as to the definition of the methods
 * or whatever. This is to improve performance, as virtual function calls would
 * otherwise have to be made in tight loops like in the lexer. */

class Linemap
{
public:
  Linemap () : in_file_ (false)
  {
    // Only one instance of Linemap is allowed to exist.
    rust_assert (Linemap::instance_ == NULL);
    Linemap::instance_ = this;
  }

  ~Linemap () { Linemap::instance_ = NULL; }

  // Subsequent Location values will come from the file named
  // FILE_NAME, starting at LINE_BEGIN.  Normally LINE_BEGIN will be
  // 0, but it will be non-zero if the Rust source has a //line comment.
  void start_file (const char *file_name, unsigned int line_begin);

  // Stop generating Location values.  This will be called after all
  // input files have been read, in case any cleanup is required.
  void stop ();

protected:
  // The single existing instance of Linemap.
  static Linemap *instance_;

public:
  // Following are convenience static functions, which allow us to
  // access some virtual functions without explicitly passing around
  // an instance of Linemap.

  // Produce a human-readable description of a Location, e.g.
  // "foo.rust:10". Returns an empty string for predeclared, builtin or
  // unknown locations.
  static std::string location_to_string (location_t loc);

private:
  // Whether we are currently reading a file.
  bool in_file_;
};

#endif // !defined(RUST_LINEMAP_H)
