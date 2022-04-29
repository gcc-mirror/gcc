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
      invalid_path.emit_error ();
      return false;
    }
  // FIXME: Add a hint here if we can find the path in another scope, such as
  // a type or something else
  // TODO: For the hint, can we point to the original item's definition if
  // present?

  Resolver::Definition def;
  rust_assert (resolver.lookup_definition (ref_node_id, &def));

  // FIXME: Is that what we want?
  ref_node_id = def.parent;

  HirId ref;
  rust_assert (
    mappings.lookup_node_to_hir (restriction.get_mappings ().get_crate_num (),
				 ref_node_id, &ref));

  auto module
    = mappings.lookup_module (restriction.get_mappings ().get_crate_num (),
			      ref);

  if (!module)
    {
      invalid_path.emit_error ();
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
      to_resolve = ModuleVisibility::create_restricted (peek_module ());
      return true;
    case HIR::Visibility::PUBLIC:
      to_resolve = ModuleVisibility::create_public ();
      return true;
    case HIR::Visibility::RESTRICTED:
      // FIXME: We also need to handle 2015 vs 2018 edition conflicts
      to_resolve = ModuleVisibility::create_public ();
      return resolve_module_path (visibility.get_path (),
				  to_resolve.get_module_id ());
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

  mappings.insert_visibility (item->get_mappings ().get_defid (), module_vis);
}

DefId
VisibilityResolver::peek_module ()
{
  // We're always inserting a top module - the crate
  // But we have to check otherwise `.back()` is UB
  rust_assert (!module_stack.empty ());

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

  mappings.insert_visibility (enum_item.get_mappings ().get_defid (), vis);
  for (auto &variant : enum_item.get_variants ())
    mappings.insert_visibility (variant->get_mappings ().get_defid (), vis);
}

void
VisibilityResolver::visit (HIR::Union &union_item)
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

  mappings.insert_visibility (trait.get_mappings ().get_defid (), vis);
  for (auto &item : trait.get_trait_items ())
    mappings.insert_visibility (item->get_mappings ().get_defid (), vis);
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
VisibilityResolver::visit (HIR::ExternBlock &block)
{}

} // namespace Privacy
} // namespace Rust
