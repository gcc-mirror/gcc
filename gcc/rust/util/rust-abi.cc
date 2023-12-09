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

#include "rust-abi.h"

namespace Rust {

Rust::ABI
get_abi_from_string (const std::string &abi)
{
  if (abi.compare ("rust") == 0)
    return Rust::ABI::RUST;
  else if (abi.compare ("rust-call") == 0)
    return Rust::ABI::RUST;
  else if (abi.compare ("Rust") == 0)
    return Rust::ABI::RUST;
  else if (abi.compare ("rust-intrinsic") == 0)
    return Rust::ABI::INTRINSIC;
  else if (abi.compare ("C") == 0)
    return Rust::ABI::C;
  else if (abi.compare ("cdecl") == 0)
    return Rust::ABI::CDECL;
  else if (abi.compare ("stdcall") == 0)
    return Rust::ABI::STDCALL;
  else if (abi.compare ("fastcall") == 0)
    return Rust::ABI::FASTCALL;
  else if (abi.compare ("sysv64") == 0)
    return Rust::ABI::SYSV64;
  else if (abi.compare ("win64") == 0)
    return Rust::ABI::WIN_64;

  return Rust::ABI::UNKNOWN;
}

std::string
get_string_from_abi (Rust::ABI abi)
{
  switch (abi)
    {
    case Rust::ABI::RUST:
      return "rust";
    case Rust::ABI::INTRINSIC:
      return "rust-intrinsic";
    case Rust::ABI::C:
      return "C";
    case Rust::ABI::CDECL:
      return "cdecl";
    case Rust::ABI::STDCALL:
      return "stdcall";
    case Rust::ABI::FASTCALL:
      return "fastcall";
    case Rust::ABI::SYSV64:
      return "sysv64";
    case Rust::ABI::WIN_64:
      return "win64";

    case Rust::ABI::UNKNOWN:
      return "unknown";
    }
  return "unknown";
}

} // namespace Rust
