// Copyright (C) 2026 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_PLATFORM_INTRINSIC_H
#define RUST_COMPILE_PLATFORM_INTRINSIC_H

namespace Rust {
namespace Compile {

class PlatformIntrinsic
{
public:
  static tree compile_call (Context *ctx, TyTy::FnType *fntype,
			    const std::vector<tree> &arguments,
			    location_t locus);

private:
  enum class OpKind
  {
    BINARY,
    COMPARISON,
  };

  struct PlatformIntrinsicMapping
  {
    OpKind kind;
    tree_code code;
  };
  static const std::map<std::string, PlatformIntrinsicMapping>
    platform_intrinsics;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_PLATFORM_INTRINSIC_H
