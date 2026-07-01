// Copyright (C) 2026 Free Software Foundation, Inc.

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

#ifndef RUST_AST_IDENTIFIER_H
#define RUST_AST_IDENTIFIER_H

#include "rust-token.h"

namespace Rust {
class Identifier
{
public:
  // Create dummy identifier
  Identifier () : ident (""), loc (UNDEF_LOCATION) {}
  // Create identifier with dummy location
  Identifier (std::string ident, location_t loc = UNDEF_LOCATION)
    : ident (ident), loc (loc)
  {}
  // Create identifier from token
  Identifier (const_TokenPtr token)
    : ident (token->get_str ()), loc (token->get_locus ())
  {}

  Identifier (const Identifier &) = default;
  Identifier (Identifier &&) = default;
  Identifier &operator= (const Identifier &) = default;
  Identifier &operator= (Identifier &&) = default;

  location_t get_locus () const { return loc; }
  const std::string &as_string () const { return ident; }

  bool empty () const { return ident.empty (); }

  bool operator== (const Identifier &other) const
  {
    return ident == other.ident;
  }

  operator const std::string & () const { return ident; }

private:
  std::string ident;
  location_t loc;
};

std::ostream &operator<< (std::ostream &os, Identifier const &i);

} // namespace Rust

#endif
