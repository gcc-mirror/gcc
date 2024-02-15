// rust-gcc.cc -- Rust frontend to gcc IR.
// Copyright (C) 2011-2024 Free Software Foundation, Inc.
// Contributed by Ian Lance Taylor, Google.
// forked from gccgo

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// This has to be included outside of extern "C", so we have to
// include it here before tree.h includes it later.

#ifndef RUST_GCC
#define RUST_GCC

#include "rust-location.h"

// Bvariable is a bit more complicated, because of zero-sized types.
// The GNU linker does not permit dynamic variables with zero size.
// When we see such a variable, we generate a version of the type with
// non-zero size.  However, when referring to the global variable, we
// want an expression of zero size; otherwise, if, say, the global
// variable is passed to a function, we will be passing a
// non-zero-sized value to a zero-sized value, which can lead to a
// miscompilation.

class Bvariable
{
public:
  Bvariable (tree t) : t_ (t), orig_type_ (NULL) {}

  Bvariable (tree t, tree orig_type) : t_ (t), orig_type_ (orig_type) {}

  // Get the tree for use as an expression.
  tree get_tree (location_t) const;

  // Get the actual decl;
  tree get_decl () const { return this->t_; }

  // Create an error variable.  This is used for cases which should
  // not occur in a correct program, in order to keep the compilation
  // going without crashing.
  static Bvariable *error_variable ();

private:
  tree t_;
  tree orig_type_;
};

#endif // RUST_GCC
