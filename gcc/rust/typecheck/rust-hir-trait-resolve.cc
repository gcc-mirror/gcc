// Copyright (C) 2021 Free Software Foundation, Inc.

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

#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

void
ResolveTraitItemToRef::visit (HIR::TraitItemType &type)
{
  TyTy::BaseType *ty
    = new TyTy::PlaceholderType (type.get_mappings ().get_hirid ());
  context->insert_type (type.get_mappings (), ty);

  // create trait-item-ref
  Location locus = type.get_locus ();
  bool is_optional = false;
  std::string identifier = type.get_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::TYPE, &type,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemConst &cst)
{
  // create trait-item-ref
  Location locus = cst.get_locus ();
  bool is_optional = cst.has_expr ();
  std::string identifier = cst.get_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::CONST, &cst,
				 self, substitutions, locus);
}

void
ResolveTraitItemToRef::visit (HIR::TraitItemFunc &fn)
{
  // create trait-item-ref
  Location locus = fn.get_locus ();
  bool is_optional = fn.has_block_defined ();
  std::string identifier = fn.get_decl ().get_function_name ();

  resolved = TraitItemReference (identifier, is_optional,
				 TraitItemReference::TraitItemType::FN, &fn,
				 self, substitutions, locus);
}

} // namespace Resolver
} // namespace Rust
