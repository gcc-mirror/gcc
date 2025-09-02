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

#include "rust-ggc.h"
#include "stringpool.h"

namespace Rust {

namespace GGC {

Ident::Ident (const char *str) : inner (get_identifier (str)) {}

Ident::Ident (const std::string &str)
  : inner (get_identifier_with_length (str.c_str (), str.length ()))
{}

bool
Ident::operator== (const std::string &other) const
{
  // maybe_get_identifier_with_length doesn't seem to exist
  return maybe_get_identifier (other.c_str ()) == inner;
}

} // namespace GGC

} // namespace Rust
