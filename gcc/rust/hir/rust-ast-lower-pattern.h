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

#ifndef RUST_AST_LOWER_PATTERN
#define RUST_AST_LOWER_PATTERN

#include "rust-ast-lower-base.h"
#include "rust-diagnostics.h"

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
    return resolver.translated;
  }

  virtual ~ASTLoweringPattern () override {}

  void visit (AST::IdentifierPattern &pattern)
  {
    std::unique_ptr<Pattern> to_bind;
    translated
      = new HIR::IdentifierPattern (pattern.get_ident (), pattern.get_locus (),
				    pattern.get_is_ref (),
				    pattern.get_is_mut (), std::move (to_bind));
  }

private:
  ASTLoweringPattern () : translated (nullptr) {}

  HIR::Pattern *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_PATTERN
