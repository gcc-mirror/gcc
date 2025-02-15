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

#ifndef RUST_HIR_FUNCTION_COLLECTOR_H
#define RUST_HIR_FUNCTION_COLLECTOR_H

#include "rust-hir-item.h"
#include "rust-hir-visitor.h"
#include "rust-hir.h"
#include "rust-system.h"

namespace Rust {

// Collects all HIR items eligible for borrow checking.
class FunctionCollector : public HIR::HIRFullVisitor
{
  std::vector<HIR::Function *> functions;
  std::vector<HIR::ClosureExpr *> closures;

public:
  void go (HIR::Crate &crate) { visit_all (crate.get_items ()); }

  WARN_UNUSED_RESULT const std::vector<HIR::Function *> &get_functions () const
  {
    return functions;
  }

  WARN_UNUSED_RESULT const std::vector<HIR::ClosureExpr *> &
  get_closures () const
  {
    return closures;
  }

protected:
  template <typename T> void visit_all (std::vector<std::unique_ptr<T>> &items)
  {
    for (std::unique_ptr<T> &item : items)
      item->accept_vis (*this);
  }

  void visit (HIR::Function &function) override
  {
    functions.push_back (&function);
    function.get_definition ().accept_vis (*this);
  }

  void visit (HIR::ClosureExpr &closure) override
  {
    closures.push_back (&closure);
    closure.get_expr ().accept_vis (*this);
  }

  // TODO: recurse for nested closures and functions.
public:
  void visit (HIR::Lifetime &lifetime) override {}
  void visit (HIR::LifetimeParam &lifetime_param) override {}
  void visit (HIR::PathInExpression &path) override {}
  void visit (HIR::TypePathSegment &segment) override {}
  void visit (HIR::TypePathSegmentGeneric &segment) override {}
  void visit (HIR::TypePathSegmentFunction &segment) override {}
  void visit (HIR::TypePath &path) override {}
  void visit (HIR::QualifiedPathInExpression &path) override {}
  void visit (HIR::QualifiedPathInType &path) override {}
  void visit (HIR::LiteralExpr &expr) override {}
  void visit (HIR::BorrowExpr &expr) override {}
  void visit (HIR::DereferenceExpr &expr) override {}
  void visit (HIR::ErrorPropagationExpr &expr) override {}
  void visit (HIR::NegationExpr &expr) override {}
  void visit (HIR::ArithmeticOrLogicalExpr &expr) override {}
  void visit (HIR::ComparisonExpr &expr) override {}
  void visit (HIR::LazyBooleanExpr &expr) override {}
  void visit (HIR::TypeCastExpr &expr) override {}
  void visit (HIR::AssignmentExpr &expr) override {}
  void visit (HIR::CompoundAssignmentExpr &expr) override {}
  void visit (HIR::GroupedExpr &expr) override {}
  void visit (HIR::ArrayElemsValues &elems) override {}
  void visit (HIR::ArrayElemsCopied &elems) override {}
  void visit (HIR::ArrayExpr &expr) override {}
  void visit (HIR::ArrayIndexExpr &expr) override {}
  void visit (HIR::TupleExpr &expr) override {}
  void visit (HIR::TupleIndexExpr &expr) override {}
  void visit (HIR::StructExprStruct &expr) override {}
  void visit (HIR::StructExprFieldIdentifier &field) override {}
  void visit (HIR::StructExprFieldIdentifierValue &field) override {}
  void visit (HIR::StructExprFieldIndexValue &field) override {}
  void visit (HIR::StructExprStructFields &expr) override {}
  void visit (HIR::StructExprStructBase &expr) override {}
  void visit (HIR::CallExpr &expr) override {}
  void visit (HIR::MethodCallExpr &expr) override {}
  void visit (HIR::FieldAccessExpr &expr) override {}
  void visit (HIR::BlockExpr &expr) override {}
  void visit (HIR::ContinueExpr &expr) override {}
  void visit (HIR::BreakExpr &expr) override {}
  void visit (HIR::RangeFromToExpr &expr) override {}
  void visit (HIR::RangeFromExpr &expr) override {}
  void visit (HIR::RangeToExpr &expr) override {}
  void visit (HIR::RangeFullExpr &expr) override {}
  void visit (HIR::RangeFromToInclExpr &expr) override {}
  void visit (HIR::RangeToInclExpr &expr) override {}
  void visit (HIR::ReturnExpr &expr) override {}
  void visit (HIR::UnsafeBlockExpr &expr) override {}
  void visit (HIR::LoopExpr &expr) override {}
  void visit (HIR::WhileLoopExpr &expr) override {}
  void visit (HIR::WhileLetLoopExpr &expr) override {}
  void visit (HIR::IfExpr &expr) override {}
  void visit (HIR::IfExprConseqElse &expr) override {}
  void visit (HIR::MatchExpr &expr) override {}
  void visit (HIR::AwaitExpr &expr) override {}
  void visit (HIR::AsyncBlockExpr &expr) override {}
  void visit (HIR::InlineAsm &expr) override {}
  void visit (HIR::TypeParam &param) override {}
  void visit (HIR::ConstGenericParam &param) override {}
  void visit (HIR::LifetimeWhereClauseItem &item) override {}
  void visit (HIR::TypeBoundWhereClauseItem &item) override {}
  void visit (HIR::Module &module) override {}
  void visit (HIR::ExternCrate &crate) override {}
  void visit (HIR::UseTreeGlob &use_tree) override {}
  void visit (HIR::UseTreeList &use_tree) override {}
  void visit (HIR::UseTreeRebind &use_tree) override {}
  void visit (HIR::UseDeclaration &use_decl) override {}
  void visit (HIR::TypeAlias &type_alias) override {}
  void visit (HIR::StructStruct &struct_item) override {}
  void visit (HIR::TupleStruct &tuple_struct) override {}
  void visit (HIR::EnumItem &item) override {}
  void visit (HIR::EnumItemTuple &item) override {}
  void visit (HIR::EnumItemStruct &item) override {}
  void visit (HIR::EnumItemDiscriminant &item) override {}
  void visit (HIR::Enum &enum_item) override {}
  void visit (HIR::Union &union_item) override {}
  void visit (HIR::ConstantItem &const_item) override {}
  void visit (HIR::StaticItem &static_item) override {}
  void visit (HIR::TraitItemFunc &item) override {}
  void visit (HIR::TraitItemConst &item) override {}
  void visit (HIR::TraitItemType &item) override {}
  void visit (HIR::Trait &trait) override {}
  void visit (HIR::ImplBlock &impl) override {}
  void visit (HIR::ExternalStaticItem &item) override {}
  void visit (HIR::ExternalFunctionItem &item) override {}
  void visit (HIR::ExternalTypeItem &item) override {}
  void visit (HIR::ExternBlock &block) override {}
  void visit (HIR::LiteralPattern &pattern) override {}
  void visit (HIR::IdentifierPattern &pattern) override {}
  void visit (HIR::WildcardPattern &pattern) override {}
  void visit (HIR::RangePatternBoundLiteral &bound) override {}
  void visit (HIR::RangePatternBoundPath &bound) override {}
  void visit (HIR::RangePatternBoundQualPath &bound) override {}
  void visit (HIR::RangePattern &pattern) override {}
  void visit (HIR::ReferencePattern &pattern) override {}
  void visit (HIR::StructPatternFieldTuplePat &field) override {}
  void visit (HIR::StructPatternFieldIdentPat &field) override {}
  void visit (HIR::StructPatternFieldIdent &field) override {}
  void visit (HIR::StructPattern &pattern) override {}
  void visit (HIR::TupleStructItemsNoRange &tuple_items) override {}
  void visit (HIR::TupleStructItemsRange &tuple_items) override {}
  void visit (HIR::TupleStructPattern &pattern) override {}
  void visit (HIR::TuplePatternItemsMultiple &tuple_items) override {}
  void visit (HIR::TuplePatternItemsRanged &tuple_items) override {}
  void visit (HIR::TuplePattern &pattern) override {}
  void visit (HIR::SlicePattern &pattern) override {}
  void visit (HIR::AltPattern &pattern) override {}
  void visit (HIR::EmptyStmt &stmt) override {}
  void visit (HIR::LetStmt &stmt) override {}
  void visit (HIR::ExprStmt &stmt) override {}
  void visit (HIR::TraitBound &bound) override {}
  void visit (HIR::ImplTraitType &type) override {}
  void visit (HIR::TraitObjectType &type) override {}
  void visit (HIR::ParenthesisedType &type) override {}
  void visit (HIR::TupleType &type) override {}
  void visit (HIR::NeverType &type) override {}
  void visit (HIR::RawPointerType &type) override {}
  void visit (HIR::ReferenceType &type) override {}
  void visit (HIR::ArrayType &type) override {}
  void visit (HIR::SliceType &type) override {}
  void visit (HIR::InferredType &type) override {}
  void visit (HIR::BareFunctionType &type) override {}
};

} // namespace Rust

#endif // RUST_HIR_FUNCTION_COLLECTOR_H
