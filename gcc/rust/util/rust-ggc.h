// Copyright (C) 2025 Free Software Foundation, Inc.

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

#ifndef RUST_GGC_H
#define RUST_GGC_H

#include "rust-system.h"
#include "tree.h"

namespace Rust {

namespace GGC {

class Ident
{
  tree inner;

public:
  Ident (const char *str);
  Ident (const std::string &str);

  bool operator== (const Ident &other) const { return inner == other.inner; }
  bool operator== (const std::string &other) const;

  const char *c_str () const { return IDENTIFIER_POINTER (inner); }
  size_t size () const { return IDENTIFIER_LENGTH (inner); }

  bool empty () const { return !size (); }

  std::string as_string () const
  {
    return std::string (c_str (), c_str () + size ());
  }

  tree as_tree () const { return inner; }
};

static inline bool
operator== (const std::string &a, const Ident &b)
{
  return b == a;
}

} // namespace GGC

} // namespace Rust

#endif // RUST_GGC_H
