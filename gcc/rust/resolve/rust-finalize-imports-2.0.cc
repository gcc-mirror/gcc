// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "rust-finalize-imports-2.0.h"
#include "rust-default-resolver.h"
#include "rust-hir-map.h"
#include "rust-name-resolution-context.h"
#include "rust-rib.h"
#include "rust-system.h"
#include "rust-toplevel-name-resolver-2.0.h"

namespace Rust {
namespace Resolver2_0 {

void
GlobbingVisitor::go (AST::GlobContainer *container)
{
  switch (container->get_glob_container_kind ())
    {
    case AST::GlobContainer::Kind::Module:
      visit_container (static_cast<AST::Module &> (*container).get_node_id ());
      break;
    case AST::GlobContainer::Kind::Crate:
      visit_container (static_cast<AST::Crate &> (*container).get_node_id ());
      break;
    case AST::GlobContainer::Kind::Enum:
      visit_container (static_cast<AST::Enum &> (*container).get_node_id ());
      break;
    default:
      rust_unreachable ();
    }
}

template <typename T>
void
GlobbingVisitor::visit_container (T &stack, NodeId nodeid)
{
  auto rib = stack.dfs_rib (stack.root, nodeid);
  if (rib.has_value ())
    glob_definitions (stack.peek (), rib.value ());
}

void
GlobbingVisitor::visit_container (NodeId nodeid)
{
  visit_container (ctx.values, nodeid);
  visit_container (ctx.types, nodeid);
  visit_container (ctx.macros, nodeid);
  visit_container (ctx.labels, nodeid);
}

void
GlobbingVisitor::glob_definitions (Rib &dst, Rib &src)
{
  for (auto &ent : src.get_values ())
    {
      auto globbed = glob_definition (ent.second);
      if (globbed.has_value ())
	{
	  auto res = dst.insert (ent.first, globbed.value ());
	  // inserting a globbed definition should (?) always succeed
	  // TODO: double check
	  rust_assert (res.has_value ()
		       || res.error ().existing
			    == globbed.value ().get_node_id ());
	  dirty |= res.has_value ();
	}
    }
}

tl::optional<Rib::Definition>
GlobbingVisitor::glob_definition (const Rib::Definition &def)
{
  // TODO: normal error?
  rust_assert (!def.is_ambiguous ());

  return Rib::Definition::Globbed (def.get_node_id ());
}

} // namespace Resolver2_0
} // namespace Rust
