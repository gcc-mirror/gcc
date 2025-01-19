// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-ast-dump.h"
#include "rust-expr.h"

namespace Rust {
namespace AST {

Dump::Dump (std::ostream &stream) : stream (stream), indentation (Indent ()) {}

bool
Dump::require_spacing (TokenPtr previous, TokenPtr current)
{
  if (previous == nullptr)
    {
      return false;
    }

  switch (current->get_id ())
    {
    case EXCLAM:
    case DOT_DOT:
    case DOT_DOT_EQ:
    case SCOPE_RESOLUTION:
    case LEFT_PAREN:
    case LEFT_ANGLE:
    case LEFT_SQUARE:
    case RIGHT_SQUARE:
    case RIGHT_PAREN:
    case SEMICOLON:
    case COMMA:
    case DOT:
      return false;
    default:
      break;
    }

  switch (previous->get_id ())
    {
    case SCOPE_RESOLUTION:
    case DOLLAR_SIGN:
    case LEFT_SQUARE:
    case LEFT_PAREN:
    case AMP:
    case DOT:
      return false;
    default:
      return true;
    }
}

void
Dump::debug (Visitable &v)
{
  Dump dump (std::cerr);
  dump.process (v);
}

void
Dump::go (AST::Crate &crate)
{
  process (crate);
}

void
Dump::go (AST::Item &item)
{
  process (item);
}

} // namespace AST
} // namespace Rust

void
debug (Rust::AST::Visitable &v)
{
  Rust::AST::Dump::debug (v);
}
