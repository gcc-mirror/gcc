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

#ifndef RUST_HIR_TYPE_CHECK_BASE
#define RUST_HIR_TYPE_CHECK_BASE

#include "rust-diagnostics.h"
#include "rust-hir-type-check.h"
#include "rust-name-resolver.h"
#include "rust-hir-visitor.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Resolver {

class TraitReference;
class TypeCheckBase
{
public:
  virtual ~TypeCheckBase () {}

  static TyTy::BaseType *unify_site (HirId id, TyTy::TyWithLocation lhs,
				     TyTy::TyWithLocation rhs,
				     Location unify_locus);

  static TyTy::BaseType *coercion_site (HirId id, TyTy::TyWithLocation lhs,
					TyTy::TyWithLocation rhs,
					Location coercion_locus);

  static TyTy::BaseType *cast_site (HirId id, TyTy::TyWithLocation from,
				    TyTy::TyWithLocation to,
				    Location cast_locus);

protected:
  TypeCheckBase ();

  TraitReference *resolve_trait_path (HIR::TypePath &);

  TyTy::TypeBoundPredicate get_predicate_from_bound (HIR::TypePath &path);

  bool check_for_unconstrained (
    const std::vector<TyTy::SubstitutionParamMapping> &params_to_constrain,
    const TyTy::SubstitutionArgumentMappings &constraint_a,
    const TyTy::SubstitutionArgumentMappings &constraint_b,
    const TyTy::BaseType *reference);

  TyTy::BaseType *resolve_literal (const Analysis::NodeMapping &mappings,
				   HIR::Literal &literal, Location locus);

  TyTy::ADTType::ReprOptions parse_repr_options (const AST::AttrVec &attrs,
						 Location locus);

  void resolve_generic_params (
    const std::vector<std::unique_ptr<HIR::GenericParam>> &generic_params,
    std::vector<TyTy::SubstitutionParamMapping> &substitutions);

  Analysis::Mappings *mappings;
  Resolver *resolver;
  TypeCheckContext *context;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_BASE
