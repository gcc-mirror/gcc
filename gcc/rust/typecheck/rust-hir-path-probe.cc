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

#include "rust-hir-path-probe.h"
#include "rust-hir-type-check-item.h"

namespace Rust {
namespace Resolver {

void
PathProbeType::process_impl_item_candidate (HirId id, HIR::ImplItem *item,
					    HIR::ImplBlock *impl)
{
  current_impl = impl;
  HirId impl_ty_id = impl->get_type ()->get_mappings ().get_hirid ();
  TyTy::BaseType *impl_block_ty = nullptr;
  if (!query_type (impl_ty_id, &impl_block_ty))
    return;

  if (!receiver->can_eq (impl_block_ty, false))
    {
      if (!impl_block_ty->can_eq (receiver, false))
	return;
    }

  // lets visit the impl_item
  item->accept_vis (*this);
}

} // namespace Resolver
} // namespace Rust
