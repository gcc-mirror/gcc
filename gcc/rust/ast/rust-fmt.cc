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

#include "rust-fmt.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Fmt {

std::string
ffi::RustHamster::to_string () const
{
  return std::string (ptr, len);
}

Pieces
Pieces::collect (const std::string &to_parse, bool append_newline)
{
  auto handle = ffi::collect_pieces (to_parse.c_str (), append_newline);

  // this performs multiple copies, can we avoid them maybe?
  // TODO: Instead of just creating a vec of, basically, `ffi::Piece`s, we
  // should transform them into the proper C++ type which we can work with. so
  // transform all the strings into C++ strings? all the Option<T> into
  // tl::optional<T>?
  auto pieces_vector = std::vector<ffi::Piece> (handle.piece_slice.base_ptr,
						handle.piece_slice.base_ptr
						  + handle.piece_slice.len);

  return Pieces (handle, std::move (pieces_vector));
}

Pieces::~Pieces () { ffi::destroy_pieces (handle); }

Pieces::Pieces (const Pieces &other) : pieces_vector (other.pieces_vector)
{
  handle = ffi::clone_pieces (other.handle);
}

Pieces &
Pieces::operator= (const Pieces &other)
{
  handle = ffi::clone_pieces (other.handle);
  pieces_vector = other.pieces_vector;

  return *this;
}

Pieces::Pieces (Pieces &&other)
  : pieces_vector (std::move (other.pieces_vector)),
    handle (clone_pieces (other.handle))
{}

} // namespace Fmt
} // namespace Rust
