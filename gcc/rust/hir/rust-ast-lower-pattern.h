// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_PATTERN
#define RUST_AST_LOWER_PATTERN

#include "rust-ast-lower-base.h"

namespace Rust {
namespace HIR {

class ASTLoweringPattern : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::Pattern *translate (AST::Pattern *pattern)
  {
    ASTLoweringPattern resolver;
    pattern->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);

    resolver.mappings->insert_hir_pattern (resolver.translated);
    resolver.mappings->insert_location (
      resolver.translated->get_pattern_mappings ().get_hirid (),
      pattern->get_locus ());

    return resolver.translated;
  }

  void visit (AST::IdentifierPattern &pattern) override;

  void visit (AST::PathInExpression &pattern) override;

  void visit (AST::StructPattern &pattern) override;

  void visit (AST::TupleStructPattern &pattern) override;

  void visit (AST::WildcardPattern &pattern) override;

  void visit (AST::TuplePattern &pattern) override;

  void visit (AST::LiteralPattern &pattern) override;

  void visit (AST::RangePattern &pattern) override;

private:
  ASTLoweringPattern () : translated (nullptr) {}

  HIR::Pattern *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_PATTERN
