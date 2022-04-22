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

#include "rust-visibility-resolver.h"
#include "rust-hir.h"
#include "rust-hir-item.h"

namespace Rust {
namespace Privacy {

VisibilityResolver::VisibilityResolver (Analysis::Mappings &mappings)
  : mappings (mappings)
{}

void
VisibilityResolver::go (HIR::Crate &crate)
{
  module_stack.push_back (crate.get_mappings ().get_defid ());
  mappings.insert_visibility (crate.get_mappings ().get_defid (),
			      ModuleVisibility::create_public ());

  for (auto &item : crate.items)
    {
      if (item->get_hir_kind () == HIR::Node::VIS_ITEM)
	{
	  auto vis_item = static_cast<HIR::VisItem *> (item.get ());
	  vis_item->accept_vis (*this);
	}
    }
}

bool
VisibilityResolver::resolve_visibility (const HIR::Visibility &visibility,
					ModuleVisibility &to_resolve)
{
  switch (visibility.get_vis_type ())
    {
    case HIR::Visibility::PRIVATE:
      to_resolve = ModuleVisibility::create_restricted (peek_module ());
      return true;
    case HIR::Visibility::PUBLIC:
      // FIXME: We need to handle the restricted path here
      to_resolve = ModuleVisibility::create_public ();
      return true;
    default:
      return false;
    }
}

DefId
VisibilityResolver::peek_module ()
{
  // We're always inserting a top module - the crate
  // But we have to check otherwise `.back()` is UB
  if (module_stack.empty ())
    gcc_unreachable ();

  return module_stack.back ();
}

void
VisibilityResolver::visit (HIR::Module &mod)
{
  module_stack.push_back (mod.get_mappings ().get_defid ());

  for (auto &item : mod.get_items ())
    {
      if (item->get_hir_kind () == HIR::Node::VIS_ITEM)
	{
	  auto vis_item = static_cast<HIR::VisItem *> (item.get ());
	  vis_item->accept_vis (*this);
	}
    }

  module_stack.pop_back ();
}

void
VisibilityResolver::visit (HIR::ExternCrate &crate)
{}

void
VisibilityResolver::visit (HIR::UseDeclaration &use_decl)
{}

void
VisibilityResolver::visit (HIR::Function &func)
{}

void
VisibilityResolver::visit (HIR::TypeAlias &type_alias)
{}

void
VisibilityResolver::visit (HIR::StructStruct &struct_item)
{}

void
VisibilityResolver::visit (HIR::TupleStruct &tuple_struct)
{}

void
VisibilityResolver::visit (HIR::Enum &enum_item)
{}

void
VisibilityResolver::visit (HIR::Union &union_item)
{}

void
VisibilityResolver::visit (HIR::ConstantItem &const_item)
{}

void
VisibilityResolver::visit (HIR::StaticItem &static_item)
{}

void
VisibilityResolver::visit (HIR::Trait &trait)
{}

void
VisibilityResolver::visit (HIR::ImplBlock &impl)
{}

void
VisibilityResolver::visit (HIR::ExternBlock &block)
{}

} // namespace Privacy
} // namespace Rust
