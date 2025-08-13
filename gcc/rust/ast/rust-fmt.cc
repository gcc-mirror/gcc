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
Pieces::collect (const std::string &to_parse, bool append_newline,
		 ffi::ParseMode parse_mode)
{
  Pieces ret (to_parse, ffi::FFIVec<ffi::Piece> ());
  ret.data->second = ffi::collect_pieces (ffi::RustHamster (ret.data->first),
					  append_newline, parse_mode);
  return ret;
}

} // namespace Fmt
} // namespace Rust
