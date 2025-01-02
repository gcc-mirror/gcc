// Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_FORMAT_ARGS
#define RUST_AST_LOWER_FORMAT_ARGS

#include "rust-ast-full-decls.h"
#include "rust-hir-full-decls.h"

namespace Rust {
namespace HIR {

class FormatArgsLowering
{
public:
  FormatArgsLowering ();
  HIR::Expr *go (AST::FormatArgs &fmt);

private:
};

} // namespace HIR
} // namespace Rust

#endif // ! RUST_AST_LOWER_FORMAT_ARGS
