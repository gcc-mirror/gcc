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

#ifndef RUST_HIR_TYPE_CHECK_TYPE
#define RUST_HIR_TYPE_CHECK_TYPE

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-path-probe.h"

namespace Rust {
namespace Resolver {

// FIXME
// This simply fetches the HIR:::GenericArgs from the base class. Check to see
// if we can get rid of this class
class TypeCheckResolveGenericArguments : public TypeCheckBase
{
public:
  static HIR::GenericArgs resolve (HIR::TypePathSegment *segment);

  void visit (HIR::TypePathSegmentGeneric &generic);

private:
  TypeCheckResolveGenericArguments (Location locus)
    : TypeCheckBase (), args (HIR::GenericArgs::create_empty (locus))
  {}

  HIR::GenericArgs args;
};

class TypeCheckType : public TypeCheckBase, public HIR::HIRTypeVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Type *type);

  void visit (HIR::BareFunctionType &fntype) override;
  void visit (HIR::TupleType &tuple) override;
  void visit (HIR::TypePath &path) override;
  void visit (HIR::QualifiedPathInType &path) override;
  void visit (HIR::ArrayType &type) override;
  void visit (HIR::SliceType &type) override;
  void visit (HIR::ReferenceType &type) override;
  void visit (HIR::RawPointerType &type) override;
  void visit (HIR::InferredType &type) override;
  void visit (HIR::NeverType &type) override;
  void visit (HIR::TraitObjectType &type) override;

  void visit (HIR::TypePathSegmentFunction &segment) override
  { /* TODO */
  }
  void visit (HIR::TraitBound &bound) override
  { /* TODO */
  }
  void visit (HIR::ImplTraitType &type) override
  { /* TODO */
  }
  void visit (HIR::ParenthesisedType &type) override
  { /* TODO */
  }
  void visit (HIR::ImplTraitTypeOneBound &type) override
  { /* TODO */
  }

private:
  TypeCheckType (HirId id)
    : TypeCheckBase (), translated (new TyTy::ErrorType (id))
  {}

  TyTy::BaseType *resolve_root_path (HIR::TypePath &path, size_t *offset,
				     NodeId *root_resolved_node_id);

  TyTy::BaseType *resolve_segments (
    NodeId root_resolved_node_id, HirId expr_id,
    std::vector<std::unique_ptr<HIR::TypePathSegment>> &segments, size_t offset,
    TyTy::BaseType *tyseg, const Analysis::NodeMapping &expr_mappings,
    Location expr_locus);

  TyTy::BaseType *translated;
};

class TypeResolveGenericParam : public TypeCheckBase
{
public:
  static TyTy::ParamType *Resolve (HIR::GenericParam *param);

protected:
  void visit (HIR::TypeParam &param);
  void visit (HIR::LifetimeParam &param);
  void visit (HIR::ConstGenericParam &param);

private:
  TypeResolveGenericParam () : TypeCheckBase (), resolved (nullptr) {}

  TyTy::ParamType *resolved;
};

class ResolveWhereClauseItem : public TypeCheckBase
{
public:
  static void Resolve (HIR::WhereClauseItem &item);

protected:
  void visit (HIR::LifetimeWhereClauseItem &item);
  void visit (HIR::TypeBoundWhereClauseItem &item);

private:
  ResolveWhereClauseItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TYPE
