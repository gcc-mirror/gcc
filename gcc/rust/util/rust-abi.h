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

#ifndef RUST_ABI_OPTIONS_H
#define RUST_ABI_OPTIONS_H

#include "rust-system.h"

namespace Rust {

enum ABI
{
  UNKNOWN,
  RUST,
  INTRINSIC,
  C,
  CDECL,
  STDCALL,
  FASTCALL,
  WIN_64,
  SYSV64
};

extern Rust::ABI
get_abi_from_string (const std::string &abi);

extern std::string
get_string_from_abi (Rust::ABI abi);

} // namespace Rust

#endif // RUST_ABI_OPTIONS_H
