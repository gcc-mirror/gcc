// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-borrow-checker.h"
#include "rust-function-collector.h"
#include "rust-bir.h"
#include "rust-bir-visitor.h"

namespace Rust {
namespace HIR {

void
BorrowChecker::go (HIR::Crate &crate)
{
  FunctionCollector collector;
  collector.go (crate);

  for (auto func ATTRIBUTE_UNUSED : collector.get_functions ())
    {
    }

  for (auto closure ATTRIBUTE_UNUSED : collector.get_closures ())
    {
    }
}

} // namespace HIR
} // namespace Rust