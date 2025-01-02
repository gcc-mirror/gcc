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

#ifndef RUST_HIR_TYPE_CHECK_IMPLITEM_H
#define RUST_HIR_TYPE_CHECK_IMPLITEM_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckTopLevelExternItem : public TypeCheckBase,
				    public HIR::HIRExternalItemVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::ExternalItem *item,
				  const HIR::ExternBlock &parent);

  void visit (HIR::ExternalStaticItem &item) override;
  void visit (HIR::ExternalFunctionItem &function) override;
  void visit (HIR::ExternalTypeItem &type) override;

private:
  TypeCheckTopLevelExternItem (const HIR::ExternBlock &parent);

  const HIR::ExternBlock &parent;
  TyTy::BaseType *resolved;
};

class TypeCheckImplItem : public TypeCheckBase, public HIR::HIRImplVisitor
{
public:
  static TyTy::BaseType *
  Resolve (HIR::ImplBlock *parent, HIR::ImplItem *item, TyTy::BaseType *self,
	   std::vector<TyTy::SubstitutionParamMapping> substitutions);

  void visit (HIR::Function &function) override;
  void visit (HIR::ConstantItem &const_item) override;
  void visit (HIR::TypeAlias &type_alias) override;

protected:
  TypeCheckImplItem (HIR::ImplBlock *parent, TyTy::BaseType *self,
		     std::vector<TyTy::SubstitutionParamMapping> substitutions);

  HIR::ImplBlock *parent;
  TyTy::BaseType *self;
  std::vector<TyTy::SubstitutionParamMapping> substitutions;

  TyTy::BaseType *result = nullptr;
};

class TypeCheckImplItemWithTrait : public TypeCheckBase,
				   public HIR::HIRImplVisitor
{
public:
  static TyTy::TypeBoundPredicateItem
  Resolve (HIR::ImplBlock *parent, HIR::ImplItem *item, TyTy::BaseType *self,
	   TyTy::TypeBoundPredicate &trait_reference,
	   std::vector<TyTy::SubstitutionParamMapping> substitutions);

  void visit (HIR::ConstantItem &constant) override;
  void visit (HIR::TypeAlias &type) override;
  void visit (HIR::Function &function) override;

protected:
  // this allows us to inherit the must_use specified on a trait definition onto
  // its implementation
  void merge_attributes (AST::AttrVec &impl_item_attrs,
			 const HIR::TraitItem &trait_item);

private:
  TypeCheckImplItemWithTrait (
    HIR::ImplBlock *parent, TyTy::BaseType *self,
    TyTy::TypeBoundPredicate &trait_reference,
    std::vector<TyTy::SubstitutionParamMapping> substitutions);

  bool is_trait_impl_block () const;

  TyTy::TypeBoundPredicate &trait_reference;
  TyTy::TypeBoundPredicateItem resolved_trait_item;

  HIR::ImplBlock *parent;
  TyTy::BaseType *self;
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  TyTy::RegionConstraints region_constraints;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_IMPLITEM_H
