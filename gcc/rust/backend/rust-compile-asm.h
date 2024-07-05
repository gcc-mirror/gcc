
// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_ASM
#define RUST_COMPILE_ASM

#include "rust-compile-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Compile {

class CompileAsm
{
public:
  static const int ASM_TREE_ARRAY_LENGTH = 5;
  static tree add_stmt (tree);
  static tree asm_build_asm_stmt (HIR::InlineAsm &);
  static tree asm_build_expr (HIR::InlineAsm &);
  static tree asm_build_stmt (location_t, enum tree_code,
			      const std::array<tree, ASM_TREE_ARRAY_LENGTH> &);
  static location_t asm_get_locus (HIR::InlineAsm &);
  static tree asm_construct_string_tree (HIR::InlineAsm &);
  static tree asm_construct_outputs (HIR::InlineAsm &);
  static tree asm_construct_inputs (HIR::InlineAsm &);
  static tree asm_construct_clobber_tree (HIR::InlineAsm &);
  static tree asm_construct_label_tree (HIR::InlineAsm &);

  static bool asm_is_simple (HIR::InlineAsm &);

  static bool asm_is_inline (HIR::InlineAsm &);
};
} // namespace Compile
} // namespace Rust
#endif // RUST_COMPILE_ASM
