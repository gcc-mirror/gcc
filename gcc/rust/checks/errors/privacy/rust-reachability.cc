// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

ReachLevel
ReachabilityVisitor::get_reachability_level (
  const HIR::Visibility &item_visibility)
{
  return item_visibility.is_public () ? current_level : ReachLevel::Unreachable;
}

void
ReachabilityVisitor::visit_generic_predicates (
  const std::vector<std::unique_ptr<HIR::GenericParam>> &generics,
  ReachLevel item_reach)
{
  if (item_reach == ReachLevel::Unreachable)
    return;

  for (const auto &generic : generics)
    {
      if (generic->get_kind () == HIR::GenericParam::GenericKind::TYPE)
	{
	  TyTy::BaseType *generic_ty = nullptr;
	  auto ok = ty_ctx.lookup_type (generic->get_mappings ().get_hirid (),
					&generic_ty);
	  rust_assert (ok);
	  rust_assert (generic_ty->get_kind () == TyTy::PARAM);

	  auto generic_param = static_cast<TyTy::ParamType *> (generic_ty);
	  for (const auto &bound : generic_param->get_specified_bounds ())
	    {
	      const auto trait = bound.get ()->get_hir_trait_ref ();
	      ctx.update_reachability (trait->get_mappings (), item_reach);
	    }
	}
    }
}

void
ReachabilityVisitor::visit (HIR::Module &mod)
{
  auto reach = get_reachability_level (mod.get_visibility ());
  reach = ctx.update_reachability (mod.get_mappings (), reach);

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
{
  auto reach = get_reachability_level (crate.get_visibility ());
  reach = ctx.update_reachability (crate.get_mappings (), reach);
}

void
ReachabilityVisitor::visit (HIR::UseDeclaration &use_decl)
{
  auto reach = get_reachability_level (use_decl.get_visibility ());
  reach = ctx.update_reachability (use_decl.get_mappings (), reach);
}

void
ReachabilityVisitor::visit (HIR::Function &func)
{
  auto fn_reach = get_reachability_level (func.get_visibility ());

  fn_reach = ctx.update_reachability (func.get_mappings (), fn_reach);
  visit_generic_predicates (func.get_generic_params (), fn_reach);
}

void
ReachabilityVisitor::visit (HIR::TypeAlias &type_alias)
{
  auto type_reach = get_reachability_level (type_alias.get_visibility ());

  visit_generic_predicates (type_alias.get_generic_params (), type_reach);
}

void
ReachabilityVisitor::visit (HIR::StructStruct &struct_item)
{
  auto struct_reach = get_reachability_level (struct_item.get_visibility ());

  struct_reach
    = ctx.update_reachability (struct_item.get_mappings (), struct_reach);

  auto old_level = current_level;
  current_level = struct_reach;

  visit_generic_predicates (struct_item.get_generic_params (), struct_reach);

  if (struct_reach != ReachLevel::Unreachable)
    {
      for (auto &field : struct_item.get_fields ())
	if (field.get_visibility ().is_public ())
	  ctx.update_reachability (field.get_field_type ().get_mappings (),
				   struct_reach);
    }

  current_level = old_level;
}

void
ReachabilityVisitor::visit (HIR::TupleStruct &)
{}

void
ReachabilityVisitor::visit (HIR::Enum &enum_item)
{
  auto enum_reach = get_reachability_level (enum_item.get_visibility ());

  enum_reach = ctx.update_reachability (enum_item.get_mappings (), enum_reach);
  visit_generic_predicates (enum_item.get_generic_params (), enum_reach);

  for (const auto &variant : enum_item.get_variants ())
    {
      auto variant_reach
	= ctx.update_reachability (variant->get_mappings (), enum_reach);

      switch (variant->get_enum_item_kind ())
	{
	  case HIR::EnumItem::Tuple: {
	    // Should we update the fields only if they are public? Similarly to
	    // what we do in the ReachabilityVisitor for HIR::TupleStruct?
	    auto tuple_variant
	      = static_cast<HIR::EnumItemTuple *> (variant.get ());
	    for (const auto &field : tuple_variant->get_tuple_fields ())
	      ctx.update_reachability (field.get_mappings (), variant_reach);
	    break;
	  }
	  case HIR::EnumItem::Struct: {
	    // Should we update the fields only if they are public? Similarly to
	    // what we do in the ReachabilityVisitor for HIR::StructStruct?
	    auto struct_variant
	      = static_cast<HIR::EnumItemStruct *> (variant.get ());
	    for (const auto &field : struct_variant->get_struct_fields ())
	      ctx.update_reachability (field.get_mappings (), variant_reach);
	    break;
	  }
	// Nothing nested to visit in that case
	case HIR::EnumItem::Named:
	case HIR::EnumItem::Discriminant:
	  break;
	}
    }
}

void
ReachabilityVisitor::visit (HIR::Union &union_item)
{
  auto union_reach = get_reachability_level (union_item.get_visibility ());

  union_reach
    = ctx.update_reachability (union_item.get_mappings (), union_reach);
  visit_generic_predicates (union_item.get_generic_params (), union_reach);
}

void
ReachabilityVisitor::visit (HIR::ConstantItem &const_item)
{
  auto reach = get_reachability_level (const_item.get_visibility ());
  reach = ctx.update_reachability (const_item.get_mappings (), reach);
}

void
ReachabilityVisitor::visit (HIR::StaticItem &static_item)
{
  auto reach = get_reachability_level (static_item.get_visibility ());
  reach = ctx.update_reachability (static_item.get_mappings (), reach);
}

void
ReachabilityVisitor::visit (HIR::Trait &trait)
{
  auto trait_reach = get_reachability_level (trait.get_visibility ());

  trait_reach = ctx.update_reachability (trait.get_mappings (), trait_reach);
  visit_generic_predicates (trait.get_generic_params (), trait_reach);
}

void
ReachabilityVisitor::visit (HIR::ImplBlock &impl)
{
  auto impl_reach = get_reachability_level (impl.get_visibility ());

  impl_reach = ctx.update_reachability (impl.get_mappings (), impl_reach);
  visit_generic_predicates (impl.get_generic_params (), impl_reach);
}

void
ReachabilityVisitor::visit (HIR::ExternBlock &)
{}

// FIXME: How can we visit Blocks in the current configuration? Have a full
// visitor?
} // namespace Privacy
} // namespace Rust
