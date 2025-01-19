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

#ifndef RUST_MAKE_UNIQUE_H
#define RUST_MAKE_UNIQUE_H

#include "rust-system.h"

namespace Rust {

template <typename T, typename... Ts>
std::unique_ptr<T>
make_unique (Ts &&...params)
{
  return std::unique_ptr<T> (new T (std::forward<Ts> (params)...));
}

} // namespace Rust

#endif // RUST_MAKE_UNIQUE_H
