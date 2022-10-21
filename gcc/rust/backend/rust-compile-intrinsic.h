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

#ifndef RUST_COMPILE_INTRINSIC
#define RUST_COMPILE_INTRINSIC

#include "rust-compile-context.h"
#include "langhooks.h"

namespace Rust {
namespace Compile {

class Intrinsics
{
public:
  Intrinsics (Context *ctx);

  tree compile (TyTy::FnType *fntype);

private:
  Context *ctx;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_INTRINSIC
