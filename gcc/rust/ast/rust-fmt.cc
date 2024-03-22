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

#include "rust-fmt.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Fmt {

Pieces
Pieces::collect (std::string &&to_parse, bool append_newline)
{
  auto piece_slice = collect_pieces (to_parse.c_str (), append_newline);

  // this performs multiple copies, can we avoid them maybe?
  // TODO: Instead of just creating a vec of, basically, `ffi::Piece`s, we
  // should transform them into the proper C++ type which we can work with. so
  // transform all the strings into C++ strings? all the Option<T> into
  // tl::optional<T>?
  auto pieces = std::vector<Piece> (piece_slice.base_ptr,
				    piece_slice.base_ptr + piece_slice.len);

  return Pieces (std::move (pieces), piece_slice, std::move (to_parse));
}

Pieces::~Pieces () { destroy_pieces (slice); }

Pieces::Pieces (const Pieces &other)
  : pieces_vector (other.pieces_vector), to_parse (other.to_parse)
{
  slice = clone_pieces (other.slice.base_ptr, other.slice.len, other.slice.cap);
}

Pieces &
Pieces::operator= (const Pieces &other)
{
  slice = clone_pieces (other.slice.base_ptr, other.slice.len, other.slice.cap);
  to_parse = other.to_parse;

  return *this;
}

Pieces::Pieces (Pieces &&other)
  : pieces_vector (std::move (other.pieces_vector)),
    slice (
      clone_pieces (other.slice.base_ptr, other.slice.len, other.slice.cap)),
    to_parse (std::move (other.to_parse))
{}

} // namespace Fmt
} // namespace Rust
