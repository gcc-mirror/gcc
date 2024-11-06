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

class CompileAsm : private HIRCompileBase
{
private:
  // RELEVANT MEMBER FUNCTIONS

  // The limit is 5 because it stands for the 5 things that the C version of
  //  build_asm_expr accepts: string, output, input, clobber and label.
  // The function signature is
  //
  // tree
  // build_asm_expr (location_t loc, tree string, tree outputs, tree inputs,
  //		tree clobbers, tree labels, bool simple, bool is_inline)
  static const int ASM_TREE_ARRAY_LENGTH = 5;
  tree asm_build_stmt (location_t,
		       const std::array<tree, ASM_TREE_ARRAY_LENGTH> &);

  tree asm_construct_string_tree (HIR::InlineAsm &);
  tree asm_construct_outputs (HIR::InlineAsm &);
  tree asm_construct_inputs (HIR::InlineAsm &);
  tree asm_construct_clobber_tree (HIR::InlineAsm &);
  tree asm_construct_label_tree (HIR::InlineAsm &);

public:
  // WE WILL OPEN THIS UP WHEN WE WANT TO ADD A DEDICATED PASS OF HIR'S ASM
  // translation.
  // static tree Compile (HIR::Expr *expr, Context *ctx);

  CompileAsm (Context *ctx);

  tree tree_codegen_asm (HIR::InlineAsm &);
};
} // namespace Compile
} // namespace Rust
#endif // RUST_COMPILE_ASM
