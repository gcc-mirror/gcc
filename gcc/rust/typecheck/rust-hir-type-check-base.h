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

#ifndef RUST_HIR_TYPE_CHECK_BASE
#define RUST_HIR_TYPE_CHECK_BASE

#include "rust-hir-map.h"
#include "rust-hir-type-check.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Resolver {

class TraitReference;
class TypeCheckBase
{
public:
  virtual ~TypeCheckBase () {}

protected:
  TypeCheckBase ();

  TraitReference *resolve_trait_path (HIR::TypePath &);

  TyTy::TypeBoundPredicate
  get_predicate_from_bound (HIR::TypePath &path, HIR::Type *associated_self,
			    BoundPolarity polarity
			    = BoundPolarity::RegularBound);

  bool check_for_unconstrained (
    const std::vector<TyTy::SubstitutionParamMapping> &params_to_constrain,
    const TyTy::SubstitutionArgumentMappings &constraint_a,
    const TyTy::SubstitutionArgumentMappings &constraint_b,
    const TyTy::BaseType *reference);

  TyTy::BaseType *resolve_literal (const Analysis::NodeMapping &mappings,
				   HIR::Literal &literal, location_t locus);

  TyTy::ADTType::ReprOptions parse_repr_options (const AST::AttrVec &attrs,
						 location_t locus);

  void resolve_generic_params (
    const std::vector<std::unique_ptr<HIR::GenericParam> > &generic_params,
    std::vector<TyTy::SubstitutionParamMapping> &substitutions);

  TyTy::TypeBoundPredicate
  get_marker_predicate (Analysis::RustLangItem::ItemType item_type,
			location_t locus);

  Analysis::Mappings *mappings;
  Resolver *resolver;
  TypeCheckContext *context;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_BASE
