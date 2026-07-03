// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-system.h"
#include "rust-identifier-path.h"
#include "rust-pattern.h"

namespace Rust {
namespace Resolver2_0 {

IdentifierPathPass::IdentifierPathPass (NameResolutionContext &ctx,
					std::set<NodeId> ident_path_to_convert)
  : ctx (&ctx), ident_path_to_convert (std::move (ident_path_to_convert))
{}

void
IdentifierPathPass::go (AST::Crate &crate, NameResolutionContext &ctx,
			std::set<NodeId> ident_path_to_convert)
{
  IdentifierPathPass pass (ctx, std::move (ident_path_to_convert));
  pass.visit (crate);
}

void
IdentifierPathPass::reseat (std::unique_ptr<AST::Pattern> &ptr)
{
  if (ptr->get_pattern_kind () != AST::Pattern::Kind::Identifier)
    {
      // bail out, but make sure to still visit
      visit (ptr);
      return;
    }

  auto ident_pat = static_cast<AST::IdentifierPattern *> (ptr.get ());

  if (ident_path_to_convert.find (ident_pat->get_node_id ())
      != ident_path_to_convert.end ())
    {
      std::vector<AST::PathExprSegment> segments;
      segments.emplace_back (ident_pat->get_ident ().as_string (),
			     ident_pat->get_locus ());
      ptr = std::make_unique<AST::PathInExpression> (
	std::move (segments), std::vector<AST::Attribute> (),
	ident_pat->get_locus ());
    }
  else
    {
      visit (ptr);
    }
}

} // namespace Resolver2_0
} // namespace Rust
