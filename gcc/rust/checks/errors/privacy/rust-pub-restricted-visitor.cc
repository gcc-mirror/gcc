// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-pub-restricted-visitor.h"
#include "rust-hir.h"
#include "rust-hir-item.h"

namespace Rust {
namespace Privacy {

bool
PubRestrictedVisitor::is_restriction_valid (NodeId item_id,
					    const location_t locus)
{
  ModuleVisibility visibility;

  // If there is no visibility in the mappings, then the item is private and
  // does not contain any restriction
  // FIXME: Is that correct?
  if (!mappings.lookup_visibility (item_id, visibility))
    return true;

  for (auto mod = module_stack.rbegin (); mod != module_stack.rend (); mod++)
    if (*mod == visibility.get_module_id ())
      return true;

  rust_error_at (locus, "restricted path is not an ancestor of the "
			"current module");
  return false;
}

PubRestrictedVisitor::PubRestrictedVisitor (Analysis::Mappings &mappings)
  : mappings (mappings)
{}

void
PubRestrictedVisitor::go (HIR::Crate &crate)
{
  // The `crate` module will always be present
  module_stack.emplace_back (crate.get_mappings ().get_defid ());

  // FIXME: When do we insert `super`? `self`?
  // We need wrapper function for these

  for (auto &item : crate.get_items ())
    {
      if (item->get_hir_kind () == HIR::Node::VIS_ITEM)
	{
	  auto vis_item = static_cast<HIR::VisItem *> (item.get ());
	  vis_item->accept_vis (*this);
	}
    }
}

void
PubRestrictedVisitor::visit (HIR::Module &mod)
{
  // FIXME: We need to update `super` and `self` here
  module_stack.push_back (mod.get_mappings ().get_defid ());

  is_restriction_valid (mod.get_mappings ().get_nodeid (), mod.get_locus ());

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
PubRestrictedVisitor::visit (HIR::ExternCrate &crate)
{
  is_restriction_valid (crate.get_mappings ().get_nodeid (),
			crate.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::UseDeclaration &use_decl)
{
  is_restriction_valid (use_decl.get_mappings ().get_nodeid (),
			use_decl.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::Function &func)
{
  is_restriction_valid (func.get_mappings ().get_nodeid (), func.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::TypeAlias &type_alias)
{
  is_restriction_valid (type_alias.get_mappings ().get_nodeid (),
			type_alias.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::StructStruct &struct_item)
{
  is_restriction_valid (struct_item.get_mappings ().get_nodeid (),
			struct_item.get_locus ());
  // FIXME: Check fields here as well
}

void
PubRestrictedVisitor::visit (HIR::TupleStruct &tuple_struct)
{
  is_restriction_valid (tuple_struct.get_mappings ().get_nodeid (),
			tuple_struct.get_locus ());
  // FIXME: Check fields here as well
}

void
PubRestrictedVisitor::visit (HIR::Enum &enum_item)
{
  is_restriction_valid (enum_item.get_mappings ().get_nodeid (),
			enum_item.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::Union &union_item)
{
  is_restriction_valid (union_item.get_mappings ().get_nodeid (),
			union_item.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::ConstantItem &const_item)
{
  is_restriction_valid (const_item.get_mappings ().get_nodeid (),
			const_item.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::StaticItem &static_item)
{
  is_restriction_valid (static_item.get_mappings ().get_nodeid (),
			static_item.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::Trait &trait)
{
  is_restriction_valid (trait.get_mappings ().get_nodeid (),
			trait.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::ImplBlock &impl)
{
  is_restriction_valid (impl.get_mappings ().get_nodeid (), impl.get_locus ());
}

void
PubRestrictedVisitor::visit (HIR::ExternBlock &block)
{
  is_restriction_valid (block.get_mappings ().get_nodeid (),
			block.get_locus ());
}

} // namespace Privacy
} // namespace Rust
