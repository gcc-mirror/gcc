// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_LOWER
#define RUST_HIR_LOWER

#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-hir-full.h"

namespace Rust {
namespace HIR {

/* Checks whether the name of a field already exists.  Returns true
   and produces an error if so.  */
bool
struct_field_name_exists (std::vector<HIR::StructField> &fields,
			  HIR::StructField &new_field);
class ASTLowering
{
public:
  static HIR::Crate Resolve (AST::Crate &astCrate);
  ~ASTLowering ();

private:
  ASTLowering (AST::Crate &astCrate);
  HIR::Crate go ();

  AST::Crate &astCrate;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_HIR_LOWER
