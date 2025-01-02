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

#ifndef RUST_BIR_BUILDER_STRUCT_H
#define RUST_BIR_BUILDER_STRUCT_H

#include "rust-bir-builder-internal.h"
#include "rust-bir-builder-expr-stmt.h"

namespace Rust {
namespace BIR {

// Separated because it needs the struct type as a context.
class StructBuilder : public AbstractBuilder, public HIR::HIRFullVisitor
{
  TyTy::VariantDef *struct_ty;
  /** Output of the builder. */
  std::vector<PlaceId> init_values;

public:
  StructBuilder (BuilderContext &ctx, TyTy::VariantDef *struct_ty)
    : AbstractBuilder (ctx), struct_ty (struct_ty)
  {
    init_values.reserve (struct_ty->get_fields ().size ());
  }

  std::vector<PlaceId> &&build (HIR::StructExprStructFields &struct_expr)
  {
    for (auto &field : struct_expr.get_fields ())
      field->accept_vis (*this);
    return std::move (init_values);
  }

  void visit (HIR::StructExprFieldIdentifier &field) override
  {
    handle_named_field (field, resolve_variable (field));
  }
  void visit (HIR::StructExprFieldIdentifierValue &field) override
  {
    auto value = ExprStmtBuilder (ctx).build (*field.get_value ());
    handle_named_field (field, value);
  }
  void visit (HIR::StructExprFieldIndexValue &field) override
  {
    auto value = ExprStmtBuilder (ctx).build (*field.get_value ());
    coercion_site (value,
		   struct_ty->get_field_at_index (field.get_tuple_index ())
		     ->get_field_type ());
    init_values.push_back (value);
  }

private:
  template <typename T> void handle_named_field (T &field, PlaceId value)
  {
    size_t field_index;
    TyTy::StructFieldType *field_type;
    bool ok = struct_ty->lookup_field (field.get_field_name ().as_string (),
				       &field_type, &field_index);
    rust_assert (ok);
    rust_assert (value != INVALID_PLACE);
    coercion_site (value, field_type->get_field_type ());
    init_values.push_back (value);
  }

protected:
  void visit (HIR::Lifetime &lifetime) override { rust_unreachable (); }
  void visit (HIR::LifetimeParam &lifetime_param) override
  {
    rust_unreachable ();
  }
  void visit (HIR::PathInExpression &path) override { rust_unreachable (); }
  void visit (HIR::TypePathSegment &segment) override { rust_unreachable (); }
  void visit (HIR::TypePathSegmentGeneric &segment) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TypePathSegmentFunction &segment) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TypePath &path) override { rust_unreachable (); }
  void visit (HIR::QualifiedPathInExpression &path) override
  {
    rust_unreachable ();
  }
  void visit (HIR::QualifiedPathInType &path) override { rust_unreachable (); }
  void visit (HIR::LiteralExpr &expr) override { rust_unreachable (); }
  void visit (HIR::BorrowExpr &expr) override { rust_unreachable (); }
  void visit (HIR::DereferenceExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ErrorPropagationExpr &expr) override { rust_unreachable (); }
  void visit (HIR::NegationExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override
  {
    rust_unreachable ();
  }
  void visit (HIR::ComparisonExpr &expr) override { rust_unreachable (); }
  void visit (HIR::LazyBooleanExpr &expr) override { rust_unreachable (); }
  void visit (HIR::TypeCastExpr &expr) override { rust_unreachable (); }
  void visit (HIR::AssignmentExpr &expr) override { rust_unreachable (); }
  void visit (HIR::CompoundAssignmentExpr &expr) override
  {
    rust_unreachable ();
  }
  void visit (HIR::GroupedExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ArrayElemsValues &elems) override { rust_unreachable (); }
  void visit (HIR::ArrayElemsCopied &elems) override { rust_unreachable (); }
  void visit (HIR::ArrayExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ArrayIndexExpr &expr) override { rust_unreachable (); }
  void visit (HIR::TupleExpr &expr) override { rust_unreachable (); }
  void visit (HIR::TupleIndexExpr &expr) override { rust_unreachable (); }
  void visit (HIR::StructExprStruct &expr) override { rust_unreachable (); }
  void visit (HIR::StructExprStructFields &expr) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructExprStructBase &expr) override { rust_unreachable (); }
  void visit (HIR::CallExpr &expr) override { rust_unreachable (); }
  void visit (HIR::MethodCallExpr &expr) override { rust_unreachable (); }
  void visit (HIR::FieldAccessExpr &expr) override { rust_unreachable (); }
  void visit (HIR::BlockExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ClosureExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ContinueExpr &expr) override { rust_unreachable (); }
  void visit (HIR::BreakExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFromToExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFromExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeToExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFullExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeFromToInclExpr &expr) override { rust_unreachable (); }
  void visit (HIR::RangeToInclExpr &expr) override { rust_unreachable (); }
  void visit (HIR::ReturnExpr &expr) override { rust_unreachable (); }
  void visit (HIR::UnsafeBlockExpr &expr) override { rust_unreachable (); }
  void visit (HIR::LoopExpr &expr) override { rust_unreachable (); }
  void visit (HIR::WhileLoopExpr &expr) override { rust_unreachable (); }
  void visit (HIR::WhileLetLoopExpr &expr) override { rust_unreachable (); }
  void visit (HIR::IfExpr &expr) override { rust_unreachable (); }
  void visit (HIR::IfExprConseqElse &expr) override { rust_unreachable (); }
  void visit (HIR::IfLetExpr &expr) override { rust_unreachable (); }
  void visit (HIR::IfLetExprConseqElse &expr) override { rust_unreachable (); }
  void visit (HIR::MatchExpr &expr) override { rust_unreachable (); }
  void visit (HIR::AwaitExpr &expr) override { rust_unreachable (); }
  void visit (HIR::AsyncBlockExpr &expr) override { rust_unreachable (); }
  void visit (HIR::TypeParam &param) override { rust_unreachable (); }
  void visit (HIR::ConstGenericParam &param) override { rust_unreachable (); }
  void visit (HIR::LifetimeWhereClauseItem &item) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TypeBoundWhereClauseItem &item) override
  {
    rust_unreachable ();
  }
  void visit (HIR::Module &module) override { rust_unreachable (); }
  void visit (HIR::ExternCrate &crate) override { rust_unreachable (); }
  void visit (HIR::UseTreeGlob &use_tree) override { rust_unreachable (); }
  void visit (HIR::UseTreeList &use_tree) override { rust_unreachable (); }
  void visit (HIR::UseTreeRebind &use_tree) override { rust_unreachable (); }
  void visit (HIR::UseDeclaration &use_decl) override { rust_unreachable (); }
  void visit (HIR::Function &function) override { rust_unreachable (); }
  void visit (HIR::TypeAlias &type_alias) override { rust_unreachable (); }
  void visit (HIR::StructStruct &struct_item) override { rust_unreachable (); }
  void visit (HIR::TupleStruct &tuple_struct) override { rust_unreachable (); }
  void visit (HIR::EnumItem &item) override { rust_unreachable (); }
  void visit (HIR::EnumItemTuple &item) override { rust_unreachable (); }
  void visit (HIR::EnumItemStruct &item) override { rust_unreachable (); }
  void visit (HIR::EnumItemDiscriminant &item) override { rust_unreachable (); }
  void visit (HIR::Enum &enum_item) override { rust_unreachable (); }
  void visit (HIR::Union &union_item) override { rust_unreachable (); }
  void visit (HIR::ConstantItem &const_item) override { rust_unreachable (); }
  void visit (HIR::StaticItem &static_item) override { rust_unreachable (); }
  void visit (HIR::TraitItemFunc &item) override { rust_unreachable (); }
  void visit (HIR::TraitItemConst &item) override { rust_unreachable (); }
  void visit (HIR::TraitItemType &item) override { rust_unreachable (); }
  void visit (HIR::Trait &trait) override { rust_unreachable (); }
  void visit (HIR::ImplBlock &impl) override { rust_unreachable (); }
  void visit (HIR::ExternalStaticItem &item) override { rust_unreachable (); }
  void visit (HIR::ExternalFunctionItem &item) override { rust_unreachable (); }
  void visit (HIR::ExternalTypeItem &item) override { rust_unreachable (); }
  void visit (HIR::ExternBlock &block) override { rust_unreachable (); }
  void visit (HIR::LiteralPattern &pattern) override { rust_unreachable (); }
  void visit (HIR::IdentifierPattern &pattern) override { rust_unreachable (); }
  void visit (HIR::WildcardPattern &pattern) override { rust_unreachable (); }
  void visit (HIR::RangePatternBoundLiteral &bound) override
  {
    rust_unreachable ();
  }
  void visit (HIR::RangePatternBoundPath &bound) override
  {
    rust_unreachable ();
  }
  void visit (HIR::RangePatternBoundQualPath &bound) override
  {
    rust_unreachable ();
  }
  void visit (HIR::RangePattern &pattern) override { rust_unreachable (); }
  void visit (HIR::ReferencePattern &pattern) override { rust_unreachable (); }
  void visit (HIR::StructPatternFieldTuplePat &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructPatternFieldIdentPat &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructPatternFieldIdent &field) override
  {
    rust_unreachable ();
  }
  void visit (HIR::StructPattern &pattern) override { rust_unreachable (); }
  void visit (HIR::TupleStructItemsNoRange &tuple_items) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TupleStructItemsRange &tuple_items) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TupleStructPattern &pattern) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TuplePatternItemsMultiple &tuple_items) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TuplePatternItemsRanged &tuple_items) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TuplePattern &pattern) override { rust_unreachable (); }
  void visit (HIR::SlicePattern &pattern) override { rust_unreachable (); }
  void visit (HIR::AltPattern &pattern) override { rust_unreachable (); }
  void visit (HIR::EmptyStmt &stmt) override { rust_unreachable (); }
  void visit (HIR::LetStmt &stmt) override { rust_unreachable (); }
  void visit (HIR::ExprStmt &stmt) override { rust_unreachable (); }
  void visit (HIR::TraitBound &bound) override { rust_unreachable (); }
  void visit (HIR::ImplTraitType &type) override { rust_unreachable (); }
  void visit (HIR::TraitObjectType &type) override { rust_unreachable (); }
  void visit (HIR::ParenthesisedType &type) override { rust_unreachable (); }
  void visit (HIR::ImplTraitTypeOneBound &type) override
  {
    rust_unreachable ();
  }
  void visit (HIR::TupleType &type) override { rust_unreachable (); }
  void visit (HIR::NeverType &type) override { rust_unreachable (); }
  void visit (HIR::RawPointerType &type) override { rust_unreachable (); }
  void visit (HIR::ReferenceType &type) override { rust_unreachable (); }
  void visit (HIR::ArrayType &type) override { rust_unreachable (); }
  void visit (HIR::SliceType &type) override { rust_unreachable (); }
  void visit (HIR::InferredType &type) override { rust_unreachable (); }
  void visit (HIR::BareFunctionType &type) override { rust_unreachable (); }
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_STRUCT_H
