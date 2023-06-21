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

#include "rust-type-util.h"
#include "rust-diagnostics.h"
#include "rust-hir-type-check.h"
#include "rust-name-resolver.h"
#include "rust-hir-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check-implitem.h"

namespace Rust {
namespace Resolver {

bool
query_type (HirId reference, TyTy::BaseType **result)
{
  Analysis::Mappings *mappings = Analysis::Mappings::get ();
  TypeCheckContext *context = TypeCheckContext::get ();

  if (context->query_in_progress (reference))
    return false;

  if (context->lookup_type (reference, result))
    return true;

  context->insert_query (reference);

  HIR::Item *item = mappings->lookup_hir_item (reference);
  if (item != nullptr)
    {
      rust_debug_loc (item->get_locus (), "resolved item {%u} to", reference);
      *result = TypeCheckItem::Resolve (*item);
      context->query_completed (reference);
      return true;
    }

  HirId parent_impl_id = UNKNOWN_HIRID;
  HIR::ImplItem *impl_item
    = mappings->lookup_hir_implitem (reference, &parent_impl_id);
  if (impl_item != nullptr)
    {
      HIR::ImplBlock *impl_block
	= mappings->lookup_hir_impl_block (parent_impl_id);
      rust_assert (impl_block != nullptr);

      // found an impl item
      rust_debug_loc (impl_item->get_locus (), "resolved impl-item {%u} to",
		      reference);

      *result = TypeCheckItem::ResolveImplItem (*impl_block, *impl_item);
      context->query_completed (reference);
      return true;
    }

  // is it an impl_type?
  HIR::ImplBlock *impl_block_by_type = nullptr;
  bool found_impl_block_type
    = mappings->lookup_impl_block_type (reference, &impl_block_by_type);
  if (found_impl_block_type)
    {
      *result = TypeCheckItem::ResolveImplBlockSelf (*impl_block_by_type);
      context->query_completed (reference);
      return true;
    }

  // is it an extern item?
  HirId parent_extern_block_id = UNKNOWN_HIRID;
  HIR::ExternalItem *extern_item
    = mappings->lookup_hir_extern_item (reference, &parent_extern_block_id);
  if (extern_item != nullptr)
    {
      HIR::ExternBlock *block
	= mappings->lookup_hir_extern_block (parent_extern_block_id);
      rust_assert (block != nullptr);

      *result = TypeCheckTopLevelExternItem::Resolve (extern_item, *block);
      context->query_completed (reference);
      return true;
    }

  // more?
  Location possible_locus = mappings->lookup_location (reference);
  rust_debug_loc (possible_locus, "query system failed to resolve: [%u]",
		  reference);
  context->query_completed (reference);

  return false;
}

} // namespace Resolver
} // namespace Rust
