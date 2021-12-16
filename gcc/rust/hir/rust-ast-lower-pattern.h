// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

    resolver.mappings->insert_hir_pattern (
      resolver.translated->get_pattern_mappings ().get_crate_num (),
      resolver.translated->get_pattern_mappings ().get_hirid (),
      resolver.translated);
    resolver.mappings->insert_location (
      resolver.translated->get_pattern_mappings ().get_crate_num (),
      resolver.translated->get_pattern_mappings ().get_hirid (),
      pattern->get_locus ());

    return resolver.translated;
  }

  void visit (AST::IdentifierPattern &pattern) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, pattern.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    std::unique_ptr<Pattern> to_bind;
    translated
      = new HIR::IdentifierPattern (mapping, pattern.get_ident (),
				    pattern.get_locus (), pattern.get_is_ref (),
				    pattern.get_is_mut () ? Mutability::Mut
							  : Mutability::Imm,
				    std::move (to_bind));
  }

  void visit (AST::PathInExpression &pattern) override;

  void visit (AST::StructPattern &pattern) override;

  void visit (AST::TupleStructPattern &pattern) override;

private:
  ASTLoweringPattern () : translated (nullptr) {}

  HIR::Pattern *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_PATTERN
