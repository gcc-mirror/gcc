// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_BORROW_CHECK_H
#define RUST_HIR_BORROW_CHECK_H

#include "rust-hir.h"

namespace Rust {
namespace HIR {

class BorrowChecker
{
  bool enable_dump_bir;

public:
  explicit BorrowChecker (bool enable_dump_bir)
    : enable_dump_bir (enable_dump_bir)
  {}

  /** Perform borrow-checking using polonius on an entire crate */
  void go (HIR::Crate &crate);
};

} // namespace HIR
} // namespace Rust

#endif // RUST_HIR_BORROW_CHECK_H
