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
#include "rust-diagnostics.h"

namespace Rust {
namespace Fmt {

Pieces
Pieces::collect (std::string &&to_parse, bool append_newline)
{
  auto piece_slice = collect_pieces (to_parse.c_str (), append_newline);

  rust_debug ("[ARTHUR] %p, %lu", (const void *) piece_slice.base_ptr,
	      piece_slice.len);

  // this performs multiple copies, can we avoid them maybe?
  // auto pieces = std::vector<Piece> (piece_slice.base_ptr,
  // 	     piece_slice.base_ptr + piece_slice.len);

  return Pieces (piece_slice, std::move (to_parse));
}

Pieces::~Pieces ()
{
  std::cerr << "Arthur: destoying pieces. this: " << (void *) this
	    << " slice: " << slice.base_ptr << std::endl;
  destroy_pieces (slice);
}

Pieces::Pieces (const Pieces &other) : to_parse (other.to_parse)
{
  slice = clone_pieces (other.slice.base_ptr, other.slice.len, other.slice.cap);
  std::cerr << "Arthur: copying pieces: other.to_parse: "
	    << (void *) other.to_parse.c_str ()
	    << " ours to_parse: " << (void *) to_parse.c_str () << std::endl;
  // auto pieces = std::vector (slice.base_ptr, slice.base_ptr + slice.len);
}

Pieces &
Pieces::operator= (const Pieces &other)
{
  slice = clone_pieces (other.slice.base_ptr, other.slice.len, other.slice.cap);
  to_parse = other.to_parse;

  return *this;
}

Pieces::Pieces (Pieces &&other)
  : slice (
    clone_pieces (other.slice.base_ptr, other.slice.len, other.slice.cap)),
    to_parse (std::move (other.to_parse))
{
  std::cerr << "Arthur: moving pieces. to_parse: " << (void *) to_parse.c_str ()
	    << " base_ptr/slice: " << (void *) slice.base_ptr << std::endl;
}

} // namespace Fmt
} // namespace Rust
