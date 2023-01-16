// Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

namespace Rust {

enum Mutability
{
  Imm,
  Mut
};

enum Unsafety
{
  Unsafe,
  Normal
};

enum Polarity
{
  Positive,
  Negative
};

enum AsyncConstStatus
{
  NONE,
  CONST_FN,
  ASYNC_FN
};

} // namespace Rust

#endif // RUST_COMMON
