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

#ifndef RUST_AST_LOWER_TYPE
#define RUST_AST_LOWER_TYPE

#include "rust-ast-lower-base.h"
#include "rust-ast-lower-expr.h"

namespace Rust {
namespace HIR {

class ASTLowerTypePath : public ASTLoweringBase
{
protected:
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::TypePath *translate (AST::TypePath &type);

  void visit (AST::TypePathSegmentFunction &segment) override;
  void visit (AST::TypePathSegment &segment) override;
  void visit (AST::TypePathSegmentGeneric &segment) override;
  void visit (AST::TypePath &path) override;

protected:
  HIR::TypePathSegment *translated_segment;

private:
  HIR::TypePath *translated;
};

class ASTLowerQualifiedPathInType : public ASTLowerTypePath
{
  using ASTLowerTypePath::visit;

public:
  static HIR::QualifiedPathInType *translate (AST::QualifiedPathInType &type);

  void visit (AST::QualifiedPathInType &path) override;

private:
  HIR::QualifiedPathInType *translated;
};

class ASTLoweringType : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::Type *translate (AST::Type *type,
			       bool default_to_static_lifetime = false);

  void visit (AST::BareFunctionType &fntype) override;
  void visit (AST::TupleType &tuple) override;
  void visit (AST::TypePath &path) override;
  void visit (AST::QualifiedPathInType &path) override;
  void visit (AST::ArrayType &type) override;
  void visit (AST::ReferenceType &type) override;
  void visit (AST::RawPointerType &type) override;
  void visit (AST::SliceType &type) override;
  void visit (AST::InferredType &type) override;
  void visit (AST::NeverType &type) override;
  void visit (AST::TraitObjectTypeOneBound &type) override;
  void visit (AST::TraitObjectType &type) override;

private:
  ASTLoweringType (bool default_to_static_lifetime)
    : ASTLoweringBase (),
      default_to_static_lifetime (default_to_static_lifetime),
      translated (nullptr)
  {}

  /** Used when compiling const and static items. */
  bool default_to_static_lifetime;

  HIR::Type *translated;
};

class ASTLowerGenericParam : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::GenericParam *translate (AST::GenericParam *param);

  void visit (AST::LifetimeParam &param) override;
  void visit (AST::ConstGenericParam &param) override;
  void visit (AST::TypeParam &param) override;

private:
  ASTLowerGenericParam () : ASTLoweringBase (), translated (nullptr) {}

  HIR::GenericParam *translated;
};

class ASTLoweringTypeBounds : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::TypeParamBound *translate (AST::TypeParamBound *type);

  void visit (AST::TraitBound &bound) override;
  void visit (AST::Lifetime &bound) override;

private:
  ASTLoweringTypeBounds () : ASTLoweringBase (), translated (nullptr) {}

  HIR::TypeParamBound *translated;
};

class ASTLowerWhereClauseItem : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::WhereClauseItem *translate (AST::WhereClauseItem &item);

  void visit (AST::LifetimeWhereClauseItem &item) override;
  void visit (AST::TypeBoundWhereClauseItem &item) override;

private:
  ASTLowerWhereClauseItem () : ASTLoweringBase (), translated (nullptr) {}

  HIR::WhereClauseItem *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_TYPE
