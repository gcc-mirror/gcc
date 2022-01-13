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

class ConstCtx
{
public:
  static tree fold (tree);

  tree constexpr_expression (tree);
  tree eval_binary_expression (tree);
  tree eval_call_expression (tree);
  tree constexpr_fn_retval (tree);
  tree eval_store_expression (tree);

private:
  ConstCtx ();

  HOST_WIDE_INT constexpr_ops_count;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_CONSTEXPR
