// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#include "rust-ast.h"
#include "rust-hir.h"
#include "rust-hir-item.h"

namespace Rust {
namespace Privacy {

VisibilityResolver::VisibilityResolver (Analysis::Mappings &mappings,
					Resolver::Resolver &resolver)
  : mappings (mappings), resolver (resolver)
{}

void
VisibilityResolver::go (HIR::Crate &crate)
{
  mappings.insert_visibility (crate.get_mappings ().get_nodeid (),
			      ModuleVisibility::create_public ());

  current_module = crate.get_mappings ().get_defid ();

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
VisibilityResolver::resolve_module_path (const HIR::SimplePath &restriction,
					 DefId &id)
{
  // We need, from the restriction, to figure out the actual Module it
  // belongs to.

  NodeId ast_node_id = restriction.get_mappings ().get_nodeid ();

  auto invalid_path
    = Error (restriction.get_locus (),
	     "cannot use non-module path as privacy restrictor");

  NodeId ref_node_id = UNKNOWN_NODEID;
  if (!resolver.lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      invalid_path.emit ();
      return false;
    }
  // FIXME: Add a hint here if we can find the path in another scope, such as
  // a type or something else
  // TODO: For the hint, can we point to the original item's definition if
  // present?

  HirId ref;
  rust_assert (mappings.lookup_node_to_hir (ref_node_id, &ref));

  auto module = mappings.lookup_module (ref);
  if (!module)
    {
      invalid_path.emit ();
      return false;
    }

  // Fill in the resolved `DefId`
  id = module->get_mappings ().get_defid ();

  return true;
}

bool
VisibilityResolver::resolve_visibility (const HIR::Visibility &visibility,
					ModuleVisibility &to_resolve)
{
  switch (visibility.get_vis_type ())
    {
    case HIR::Visibility::PRIVATE:
      to_resolve = ModuleVisibility::create_restricted (current_module);
      return true;
    case HIR::Visibility::PUBLIC:
      to_resolve = ModuleVisibility::create_public ();
      return true;
      case HIR::Visibility::RESTRICTED: {
	// FIXME: We also need to handle 2015 vs 2018 edition conflicts
	auto id = UNKNOWN_DEFID;
	auto result = resolve_module_path (visibility.get_path (), id);
	to_resolve = ModuleVisibility::create_restricted (id);
	return result;
      }
    default:
      gcc_unreachable ();
      return false;
    }
}

void
VisibilityResolver::resolve_and_update (const HIR::VisItem *item)
{
  ModuleVisibility module_vis;
  if (!resolve_visibility (item->get_visibility (), module_vis))
    return; // we will already have emitted errors

  mappings.insert_visibility (item->get_mappings ().get_nodeid (), module_vis);
}

void
VisibilityResolver::visit (HIR::Module &mod)
{
  auto old_module = current_module;
  current_module = mod.get_mappings ().get_defid ();

  for (auto &item : mod.get_items ())
    {
      if (item->get_hir_kind () == HIR::Node::VIS_ITEM)
	{
	  auto vis_item = static_cast<HIR::VisItem *> (item.get ());
	  vis_item->accept_vis (*this);
	}
    }

  current_module = old_module;
}

void
VisibilityResolver::visit (HIR::ExternCrate &)
{}

void
VisibilityResolver::visit (HIR::UseDeclaration &)
{}

void
VisibilityResolver::visit (HIR::Function &func)
{
  resolve_and_update (&func);
}

void
VisibilityResolver::visit (HIR::TypeAlias &type_alias)
{
  resolve_and_update (&type_alias);
}

void
VisibilityResolver::visit (HIR::StructStruct &struct_item)
{
  resolve_and_update (&struct_item);
}

void
VisibilityResolver::visit (HIR::TupleStruct &tuple_struct)
{
  resolve_and_update (&tuple_struct);
}

void
VisibilityResolver::visit (HIR::Enum &enum_item)
{
  ModuleVisibility vis;
  if (!resolve_visibility (enum_item.get_visibility (), vis))
    return;

  mappings.insert_visibility (enum_item.get_mappings ().get_nodeid (), vis);
  for (auto &variant : enum_item.get_variants ())
    mappings.insert_visibility (variant->get_mappings ().get_nodeid (), vis);
}

void
VisibilityResolver::visit (HIR::Union &)
{}

void
VisibilityResolver::visit (HIR::ConstantItem &const_item)
{
  resolve_and_update (&const_item);
}

void
VisibilityResolver::visit (HIR::StaticItem &static_item)
{
  resolve_and_update (&static_item);
}

void
VisibilityResolver::visit (HIR::Trait &trait)
{
  ModuleVisibility vis;
  if (!resolve_visibility (trait.get_visibility (), vis))
    return;

  mappings.insert_visibility (trait.get_mappings ().get_nodeid (), vis);
  for (auto &item : trait.get_trait_items ())
    mappings.insert_visibility (item->get_mappings ().get_nodeid (), vis);
}

void
VisibilityResolver::visit (HIR::ImplBlock &impl)
{
  for (auto &item : impl.get_impl_items ())
    {
      HIR::VisItem *vis_item;
      switch (item->get_impl_item_type ())
	{
	case HIR::ImplItem::FUNCTION:
	  vis_item = static_cast<HIR::Function *> (item.get ());
	  break;
	case HIR::ImplItem::TYPE_ALIAS:
	  vis_item = static_cast<HIR::TypeAlias *> (item.get ());
	  break;
	case HIR::ImplItem::CONSTANT:
	  vis_item = static_cast<HIR::ConstantItem *> (item.get ());
	  break;
	default:
	  gcc_unreachable ();
	  return;
	}
      vis_item->accept_vis (*this);
    }
}

void
VisibilityResolver::visit (HIR::ExternBlock &)
{}

} // namespace Privacy
} // namespace Rust
