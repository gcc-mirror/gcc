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
#include "rust-tyty.h"

namespace Rust {
namespace Privacy {

static HIR::VisItem *
maybe_get_vis_item (std::unique_ptr<HIR::Item> &item)
{
  if (item->get_hir_kind () != HIR::Node::VIS_ITEM)
    return nullptr;

  return static_cast<HIR::VisItem *> (item.get ());
}

void
ReachabilityVisitor::visit_generic_predicates (
  const std::vector<std::unique_ptr<HIR::GenericParam>> &generics,
  ReachLevel item_reach)
{
  if (item_reach == ReachLevel::Unreachable)
    return;

  for (auto &generic : generics)
    {
      if (generic->get_kind () == HIR::GenericParam::TYPE)
	{
	  TyTy::BaseType *generic_ty = nullptr;
	  rust_assert (
	    ty_ctx.lookup_type (generic->get_mappings ().get_hirid (),
				&generic_ty));

	  // FIXME: Can we really get anything else than a TyTy::PARAM here?
	  // Should we change this to an assertion instead?
	  if (generic_ty->get_kind () == TyTy::PARAM)
	    {
	      auto generic_param = static_cast<TyTy::ParamType *> (generic_ty);
	      for (const auto &bound : generic_param->get_specified_bounds ())
		{
		  const auto trait = bound.get ()->get_hir_trait_ref ();
		  ctx.update_reachability (trait->get_mappings (), item_reach);
		}
	    }
	}
    }
}

void
ReachabilityVisitor::visit (HIR::Module &mod)
{
  for (auto &item : mod.get_items ())
    {
      // FIXME: Is that what we want to do? Yes? Only visit the items with
      // visibility?
      //
      // Imagine if we had `maybe_get_vis_item(item)?->accept_vis(*this)` ;)
      auto vis_item = maybe_get_vis_item (item);
      if (vis_item)
	vis_item->accept_vis (*this);
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
  if (struct_item.get_visibility ().is_public ())
    struct_reach = current_level;

  struct_reach
    = ctx.update_reachability (struct_item.get_mappings (), struct_reach);

  auto old_level = current_level;
  current_level = struct_reach;

  visit_generic_predicates (struct_item.get_generic_params (), struct_reach);

  if (struct_reach != ReachLevel::Unreachable)
    {
      for (auto &field : struct_item.get_fields ())
	if (field.get_visibility ().is_public ())
	  ctx.update_reachability (field.get_field_type ()->get_mappings (),
				   struct_reach);
    }

  current_level = old_level;
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

// FIXME: How can we visit Blocks in the current configuration? Have a full
// visitor?
} // namespace Privacy
} // namespace Rust
