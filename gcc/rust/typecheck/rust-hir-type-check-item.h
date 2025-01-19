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

#ifndef RUST_HIR_TYPE_CHECK_ITEM
#define RUST_HIR_TYPE_CHECK_ITEM

#include "rust-hir-type-check-base.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Resolver {

class TypeCheckItem : private TypeCheckBase, private HIR::HIRVisItemVisitor
{
public:
  static TyTy::BaseType *Resolve (HIR::Item &item);

  static TyTy::BaseType *ResolveImplItem (HIR::ImplBlock &impl_block,
					  HIR::ImplItem &item);

  static TyTy::BaseType *ResolveImplBlockSelf (HIR::ImplBlock &impl_block);

  static TyTy::BaseType *ResolveImplBlockSelfWithInference (
    HIR::ImplBlock &impl, location_t locus,
    TyTy::SubstitutionArgumentMappings *infer_arguments);

  void visit (HIR::Module &module) override;
  void visit (HIR::Function &function) override;
  void visit (HIR::TypeAlias &alias) override;
  void visit (HIR::TupleStruct &struct_decl) override;
  void visit (HIR::StructStruct &struct_decl) override;
  void visit (HIR::Enum &enum_decl) override;
  void visit (HIR::Union &union_decl) override;
  void visit (HIR::StaticItem &var) override;
  void visit (HIR::ConstantItem &constant) override;
  void visit (HIR::ImplBlock &impl_block) override;
  void visit (HIR::ExternBlock &extern_block) override;
  void visit (HIR::Trait &trait_block) override;

  // nothing to do
  void visit (HIR::ExternCrate &) override {}
  void visit (HIR::UseDeclaration &) override {}

protected:
  std::pair<std::vector<TyTy::SubstitutionParamMapping>,
	    TyTy::RegionConstraints>
  resolve_impl_block_substitutions (HIR::ImplBlock &impl_block,
				    bool &failure_flag);

  void validate_trait_impl_block (
    HIR::ImplBlock &impl_block, TyTy::BaseType *self,
    std::vector<TyTy::SubstitutionParamMapping> &substitutions);

  TyTy::BaseType *resolve_impl_item (HIR::ImplBlock &impl_block,
				     HIR::ImplItem &item);

  TyTy::BaseType *resolve_impl_block_self (HIR::ImplBlock &impl_block);

private:
  TypeCheckItem ();

  TyTy::BaseType *infered;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_ITEM
