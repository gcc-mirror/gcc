// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

// Common definitions useful throughout the Rust frontend.

#ifndef RUST_COMMON
#define RUST_COMMON
#include "rust-system.h"
#include <string>

namespace Rust {

enum class Mutability
{
  Imm,
  Mut
};

enum class Unsafety
{
  Unsafe,
  Normal
};

enum class Const
{
  Yes,
  No,
};

enum class Async
{
  Yes,
  No
};

enum BoundPolarity
{
  RegularBound,
  NegativeBound,
  AntiBound,
};

enum AsyncConstStatus
{
  NONE,
  CONST_FN,
  ASYNC_FN
};

inline std::string
enum_to_str (Mutability mut)
{
  std::string str;
  switch (mut)
    {
    case Mutability::Imm:
      return "Imm";
    case Mutability::Mut:
      return "Mut";
    }
  gcc_unreachable ();
}

} // namespace Rust

#endif // RUST_COMMON
