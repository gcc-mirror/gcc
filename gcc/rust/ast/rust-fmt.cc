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

#include "rust-fmt.h"

namespace Rust {
namespace Fmt {

Pieces
Pieces::collect (const std::string &to_parse)
{
  auto piece_slice = collect_pieces (to_parse.c_str ());

  rust_debug ("[ARTHUR] %p, %lu", (void *) piece_slice.ptr, piece_slice.len);

  // this performs multiple copies, can we avoid them maybe?
  auto pieces
    = std::vector (piece_slice.ptr, piece_slice.ptr + piece_slice.len);

  rust_debug ("[ARTHUR] %p, %lu", (void *) pieces.data (), pieces.size ());

  return Pieces{};
}

} // namespace Fmt
} // namespace Rust
