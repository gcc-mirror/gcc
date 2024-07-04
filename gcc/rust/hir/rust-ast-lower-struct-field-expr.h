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

#ifndef RUST_AST_LOWER_STRUCT_FIELD_EXPR
#define RUST_AST_LOWER_STRUCT_FIELD_EXPR

#include "rust-diagnostics.h"
#include "rust-ast-lower-base.h"

namespace Rust {
namespace HIR {

class ASTLowerStructExprField : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::StructExprField *translate (AST::StructExprField *field)
  {
    ASTLowerStructExprField compiler;
    field->accept_vis (compiler);
    rust_assert (compiler.translated != nullptr);

    compiler.mappings->insert_hir_struct_field (compiler.translated);
    compiler.mappings->insert_location (
      compiler.translated->get_mappings ().get_hirid (), field->get_locus ());

    return compiler.translated;
  }

  ~ASTLowerStructExprField () {}

  void visit (AST::StructExprFieldIdentifierValue &field) override;

  void visit (AST::StructExprFieldIndexValue &field) override;

  void visit (AST::StructExprFieldIdentifier &field) override;

private:
  ASTLowerStructExprField () : translated (nullptr) {}

  HIR::StructExprField *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_STRUCT_FIELD_EXPR
