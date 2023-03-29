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

#ifndef RUST_CONSTEXPR
#define RUST_CONSTEXPR

#include "rust-system.h"
#include "tree.h"

namespace Rust {
namespace Compile {

extern tree fold_expr (tree);
extern void
maybe_save_constexpr_fundef (tree fun);

} // namespace Compile
} // namespace Rust

#endif // RUST_CONSTEXPR
