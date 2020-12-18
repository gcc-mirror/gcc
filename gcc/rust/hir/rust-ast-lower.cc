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

#include "rust-ast-lower.h"
#include "rust-ast-lower-item.h"

namespace Rust {
namespace HIR {

ASTLowering::ASTLowering (AST::Crate &astCrate) : astCrate (astCrate) {}

ASTLowering::~ASTLowering () {}

HIR::Crate
ASTLowering::Resolve (AST::Crate &astCrate)
{
  ASTLowering resolver (astCrate);
  return resolver.go ();
}

HIR::Crate
ASTLowering::go ()
{
  std::vector<std::unique_ptr<HIR::Item> > items;
  std::vector<HIR::Attribute> inner_attrs;
  bool has_utf8bom = false;
  bool has_shebang = false;

  for (auto it = astCrate.items.begin (); it != astCrate.items.end (); it++)
    {
      auto translated = ASTLoweringItem::translate (it->get ());
      if (translated != nullptr)
	items.push_back (std::unique_ptr<HIR::Item> (translated));
    }

  auto mappings = Analysis::Mappings::get ();
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, UNKNOWN_NODEID,
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  return HIR::Crate (std::move (items), std::move (inner_attrs), mapping,
		     has_utf8bom, has_shebang);
}

} // namespace HIR
} // namespace Rust
