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

#ifndef RUST_HIR_TYPE_CHECK_ITEM
#define RUST_HIR_TYPE_CHECK_ITEM

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-stmt.h"
#include "rust-hir-trait-resolve.h"
#include "rust-tyty-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckItem : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void Resolve (HIR::Item *item)
  {
    TypeCheckItem resolver;
    item->accept_vis (resolver);
  }

  void visit (HIR::ImplBlock &impl_block) override
  {
    TraitReference *trait_reference = &TraitReference::error_node ();
    if (impl_block.has_trait_ref ())
      {
	std::unique_ptr<HIR::TypePath> &ref = impl_block.get_trait_ref ();
	trait_reference = TraitResolver::Resolve (*ref.get ());
	rust_assert (!trait_reference->is_error ());
      }

    TyTy::BaseType *self = nullptr;
    if (!context->lookup_type (
	  impl_block.get_type ()->get_mappings ().get_hirid (), &self))
      {
	rust_error_at (impl_block.get_locus (),
		       "failed to resolve Self for ImplBlock");
	return;
      }

    bool is_trait_impl_block = !trait_reference->is_error ();

    std::vector<std::reference_wrapper<const TraitItemReference>>
      trait_item_refs;
    for (auto &impl_item : impl_block.get_impl_items ())
      {
	if (!is_trait_impl_block)
	  TypeCheckImplItem::Resolve (impl_item.get (), self);
	else
	  {
	    auto &trait_item_ref
	      = TypeCheckImplItemWithTrait::Resolve (impl_item.get (), self,
						     *trait_reference);
	    trait_item_refs.push_back (trait_item_ref);
	  }
      }

    bool impl_block_missing_trait_items
      = is_trait_impl_block
	&& trait_reference->size () != trait_item_refs.size ();
    if (impl_block_missing_trait_items)
      {
	// filter the missing impl_items
	std::vector<std::reference_wrapper<const TraitItemReference>>
	  missing_trait_items;
	for (auto &trait_item_ref : trait_reference->get_trait_items ())
	  {
	    bool found = false;
	    for (const TraitItemReference &implemented_trait_item :
		 trait_item_refs)
	      {
		std::string trait_item_name = trait_item_ref.get_identifier ();
		std::string impl_item_name
		  = implemented_trait_item.get_identifier ();
		found = trait_item_name.compare (impl_item_name) == 0;
		if (found)
		  break;
	      }

	    bool is_required_trait_item = !trait_item_ref.is_optional ();
	    if (!found && is_required_trait_item)
	      missing_trait_items.push_back (trait_item_ref);
	  }

	if (missing_trait_items.size () > 0)
	  {
	    std::string missing_items_buf;
	    RichLocation r (impl_block.get_locus ());
	    for (size_t i = 0; i < missing_trait_items.size (); i++)
	      {
		bool has_more = (i + 1) < missing_trait_items.size ();
		const TraitItemReference &missing_trait_item
		  = missing_trait_items.at (i);
		missing_items_buf += missing_trait_item.get_identifier ()
				     + (has_more ? ", " : "");
		r.add_range (missing_trait_item.get_locus ());
	      }

	    rust_error_at (r, "missing %s in implementation of trait %<%s%>",
			   missing_items_buf.c_str (),
			   trait_reference->get_name ().c_str ());
	  }
      }

    if (is_trait_impl_block)
      {
	trait_reference->clear_associated_types ();

	AssociatedImplTrait associated (trait_reference, &impl_block, self,
					context);
	context->insert_associated_trait_impl (
	  impl_block.get_mappings ().get_hirid (), std::move (associated));
	context->insert_associated_impl_mapping (
	  trait_reference->get_mappings ().get_hirid (), self,
	  impl_block.get_mappings ().get_hirid ());
      }
  }

  void visit (HIR::Function &function) override
  {
    TyTy::BaseType *lookup;
    if (!context->lookup_type (function.get_mappings ().get_hirid (), &lookup))
      {
	rust_error_at (function.get_locus (), "failed to lookup function type");
	return;
      }

    if (lookup->get_kind () != TyTy::TypeKind::FNDEF)
      {
	rust_error_at (function.get_locus (),
		       "found invalid type for function [%s]",
		       lookup->as_string ().c_str ());
	return;
      }

    // need to get the return type from this
    TyTy::FnType *resolved_fn_type = static_cast<TyTy::FnType *> (lookup);
    auto expected_ret_tyty = resolved_fn_type->get_return_type ();
    context->push_return_type (expected_ret_tyty);

    auto block_expr_ty
      = TypeCheckExpr::Resolve (function.get_definition ().get (), false);

    context->pop_return_type ();

    if (block_expr_ty->get_kind () != TyTy::NEVER)
      expected_ret_tyty->unify (block_expr_ty);
  }

  void visit (HIR::Module &module) override
  {
    for (auto &item : module.get_items ())
      TypeCheckItem::Resolve (item.get ());
  }

private:
  TypeCheckItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ITEM
