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

#include "rust-reachability.h"

namespace Rust {
namespace Privacy {
void
ReachabilityVisitor::visit (HIR::Module &mod)
{
  for (auto &item : mod.get_items ())
    {
      // FIXME: How do we refactor this pattern into something more ergonomic?
      // FIXME: Add helper functions
      // FIXME: Is that what we want to do? Yes? Only visit the items with
      // visibility?
      if (item->get_hir_kind () == HIR::VIS_ITEM)
	{
	  auto vis_item = static_cast<HIR::VisItem *> (item.get ());
	  vis_item->accept_vis (*this);
	}
    }
}

void
ReachabilityVisitor::visit (HIR::ExternCrate &crate)
{}

void
ReachabilityVisitor::visit (HIR::UseDeclaration &use_decl)
{}

void
ReachabilityVisitor::visit (HIR::Function &func)
{}

void
ReachabilityVisitor::visit (HIR::TypeAlias &type_alias)
{}

void
ReachabilityVisitor::visit (HIR::StructStruct &struct_item)
{
  auto struct_reach = ReachLevel::Unreachable;
  // FIXME: This feels very wrong. Should we check for `has_visibility`
  // beforehand? Is it just private otherwise? Should the `HIR::Visibility` also
  // keep variants for private items?
  if (struct_item.get_visibility ().get_vis_type () == HIR::Visibility::NONE)
    struct_reach = ReachLevel::Reachable;

  struct_reach
    = ctx.update_reachability (struct_item.get_mappings (), struct_reach);

  // FIXME: We need to also visit the fields as they might have their own set
  // of reachability levels

  for (auto &field : struct_item.get_fields ())
    ctx.update_reachability (field.get_mappings (), struct_reach);

  // FIXME: How do we get the constructor from `struct_item`? We need to update
  // its visibility as well. Probably by keeping a reference to the TypeCtx?
}

void
ReachabilityVisitor::visit (HIR::TupleStruct &tuple_struct)
{}

void
ReachabilityVisitor::visit (HIR::Enum &enum_item)
{}

void
ReachabilityVisitor::visit (HIR::Union &union_item)
{}

void
ReachabilityVisitor::visit (HIR::ConstantItem &const_item)
{}

void
ReachabilityVisitor::visit (HIR::StaticItem &static_item)
{}

void
ReachabilityVisitor::visit (HIR::Trait &trait)
{}

void
ReachabilityVisitor::visit (HIR::ImplBlock &impl)
{}

void
ReachabilityVisitor::visit (HIR::ExternBlock &block)
{}
} // namespace Privacy
} // namespace Rust
