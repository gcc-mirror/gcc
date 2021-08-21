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

#ifndef RUST_HIR_LIVENESS_BASE
#define RUST_HIR_LIVENESS_BASE

#include "rust-diagnostics.h"
#include "rust-lint-marklive.h"
#include "rust-lint-marklive-base.h"
#include "rust-hir-visitor.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Analysis {

class MarkLiveBase : public HIR::HIRVisitor
{
public:
  virtual ~MarkLiveBase () {}
  virtual void visit (HIR::IdentifierExpr &) override {}
  virtual void visit (HIR::Lifetime &) override {}
  virtual void visit (HIR::LifetimeParam &) override {}
  virtual void visit (HIR::PathInExpression &) override {}
  virtual void visit (HIR::TypePathSegment &) override {}
  virtual void visit (HIR::TypePathSegmentGeneric &) override {}
  virtual void visit (HIR::TypePathSegmentFunction &) override {}
  virtual void visit (HIR::TypePath &) override {}
  virtual void visit (HIR::QualifiedPathInExpression &) override {}
  virtual void visit (HIR::QualifiedPathInType &) override {}

  virtual void visit (HIR::LiteralExpr &) override {}
  virtual void visit (HIR::BorrowExpr &) override {}
  virtual void visit (HIR::DereferenceExpr &) override {}
  virtual void visit (HIR::ErrorPropagationExpr &) override {}
  virtual void visit (HIR::NegationExpr &) override {}
  virtual void visit (HIR::ArithmeticOrLogicalExpr &) override {}
  virtual void visit (HIR::ComparisonExpr &) override {}
  virtual void visit (HIR::LazyBooleanExpr &) override {}
  virtual void visit (HIR::TypeCastExpr &) override {}
  virtual void visit (HIR::AssignmentExpr &) override {}

  virtual void visit (HIR::GroupedExpr &) override {}

  virtual void visit (HIR::ArrayElemsValues &) override {}
  virtual void visit (HIR::ArrayElemsCopied &) override {}
  virtual void visit (HIR::ArrayExpr &) override {}
  virtual void visit (HIR::ArrayIndexExpr &) override {}
  virtual void visit (HIR::TupleExpr &) override {}
  virtual void visit (HIR::TupleIndexExpr &) override {}
  virtual void visit (HIR::StructExprStruct &) override {}

  virtual void visit (HIR::StructExprFieldIdentifier &) override {}
  virtual void visit (HIR::StructExprFieldIdentifierValue &) override {}

  virtual void visit (HIR::StructExprFieldIndexValue &) override {}
  virtual void visit (HIR::StructExprStructFields &) override {}
  virtual void visit (HIR::StructExprStructBase &) override {}
  virtual void visit (HIR::StructExprTuple &) override {}
  virtual void visit (HIR::StructExprUnit &) override {}

  virtual void visit (HIR::EnumExprFieldIdentifier &) override {}
  virtual void visit (HIR::EnumExprFieldIdentifierValue &) override {}

  virtual void visit (HIR::EnumExprFieldIndexValue &) override {}
  virtual void visit (HIR::EnumExprStruct &) override {}
  virtual void visit (HIR::EnumExprTuple &) override {}
  virtual void visit (HIR::EnumExprFieldless &) override {}
  virtual void visit (HIR::CallExpr &) override {}
  virtual void visit (HIR::MethodCallExpr &) override {}
  virtual void visit (HIR::FieldAccessExpr &) override {}
  virtual void visit (HIR::ClosureExprInner &) override {}
  virtual void visit (HIR::BlockExpr &) override {}
  virtual void visit (HIR::ClosureExprInnerTyped &) override {}
  virtual void visit (HIR::ContinueExpr &) override {}
  virtual void visit (HIR::BreakExpr &) override {}
  virtual void visit (HIR::RangeFromToExpr &) override {}
  virtual void visit (HIR::RangeFromExpr &) override {}
  virtual void visit (HIR::RangeToExpr &) override {}
  virtual void visit (HIR::RangeFullExpr &) override {}
  virtual void visit (HIR::RangeFromToInclExpr &) override {}
  virtual void visit (HIR::RangeToInclExpr &) override {}
  virtual void visit (HIR::ReturnExpr &) override {}
  virtual void visit (HIR::UnsafeBlockExpr &) override {}
  virtual void visit (HIR::LoopExpr &) override {}
  virtual void visit (HIR::WhileLoopExpr &) override {}
  virtual void visit (HIR::WhileLetLoopExpr &) override {}
  virtual void visit (HIR::ForLoopExpr &) override {}
  virtual void visit (HIR::IfExpr &) override {}
  virtual void visit (HIR::IfExprConseqElse &) override {}
  virtual void visit (HIR::IfExprConseqIf &) override {}
  virtual void visit (HIR::IfExprConseqIfLet &) override {}
  virtual void visit (HIR::IfLetExpr &) override {}
  virtual void visit (HIR::IfLetExprConseqElse &) override {}
  virtual void visit (HIR::IfLetExprConseqIf &) override {}
  virtual void visit (HIR::IfLetExprConseqIfLet &) override {}

  virtual void visit (HIR::MatchExpr &) override {}
  virtual void visit (HIR::AwaitExpr &) override {}
  virtual void visit (HIR::AsyncBlockExpr &) override {}

  virtual void visit (HIR::TypeParam &) override {}

  virtual void visit (HIR::LifetimeWhereClauseItem &) override {}
  virtual void visit (HIR::TypeBoundWhereClauseItem &) override {}
  virtual void visit (HIR::Module &) override {}
  virtual void visit (HIR::ExternCrate &) override {}

  virtual void visit (HIR::UseTreeGlob &) override {}
  virtual void visit (HIR::UseTreeList &) override {}
  virtual void visit (HIR::UseTreeRebind &) override {}
  virtual void visit (HIR::UseDeclaration &) override {}
  virtual void visit (HIR::Function &) override {}
  virtual void visit (HIR::TypeAlias &) override {}
  virtual void visit (HIR::StructStruct &) override {}
  virtual void visit (HIR::TupleStruct &) override {}
  virtual void visit (HIR::EnumItem &) override {}
  virtual void visit (HIR::EnumItemTuple &) override {}
  virtual void visit (HIR::EnumItemStruct &) override {}
  virtual void visit (HIR::EnumItemDiscriminant &) override {}
  virtual void visit (HIR::Enum &) override {}
  virtual void visit (HIR::Union &) override {}
  virtual void visit (HIR::ConstantItem &) override {}
  virtual void visit (HIR::StaticItem &) override {}
  virtual void visit (HIR::TraitItemFunc &) override {}
  virtual void visit (HIR::TraitItemConst &) override {}
  virtual void visit (HIR::TraitItemType &) override {}
  virtual void visit (HIR::Trait &) override {}
  virtual void visit (HIR::ImplBlock &) override {}

  virtual void visit (HIR::ExternalStaticItem &) override {}
  virtual void visit (HIR::ExternalFunctionItem &) override {}
  virtual void visit (HIR::ExternBlock &) override {}

  virtual void visit (HIR::LiteralPattern &) override {}
  virtual void visit (HIR::IdentifierPattern &) override {}
  virtual void visit (HIR::WildcardPattern &) override {}

  virtual void visit (HIR::RangePatternBoundLiteral &) override {}
  virtual void visit (HIR::RangePatternBoundPath &) override {}
  virtual void visit (HIR::RangePatternBoundQualPath &) override {}
  virtual void visit (HIR::RangePattern &) override {}
  virtual void visit (HIR::ReferencePattern &) override {}

  virtual void visit (HIR::StructPatternFieldTuplePat &) override {}
  virtual void visit (HIR::StructPatternFieldIdentPat &) override {}
  virtual void visit (HIR::StructPatternFieldIdent &) override {}
  virtual void visit (HIR::StructPattern &) override {}

  virtual void visit (HIR::TupleStructItemsNoRange &) override {}
  virtual void visit (HIR::TupleStructItemsRange &) override {}
  virtual void visit (HIR::TupleStructPattern &) override {}

  virtual void visit (HIR::TuplePatternItemsMultiple &) override {}
  virtual void visit (HIR::TuplePatternItemsRanged &) override {}
  virtual void visit (HIR::TuplePattern &) override {}
  virtual void visit (HIR::GroupedPattern &) override {}
  virtual void visit (HIR::SlicePattern &) override {}

  virtual void visit (HIR::EmptyStmt &) override {}
  virtual void visit (HIR::LetStmt &) override {}
  virtual void visit (HIR::ExprStmtWithoutBlock &) override {}
  virtual void visit (HIR::ExprStmtWithBlock &) override {}

  virtual void visit (HIR::TraitBound &) override {}
  virtual void visit (HIR::ImplTraitType &) override {}
  virtual void visit (HIR::TraitObjectType &) override {}
  virtual void visit (HIR::ParenthesisedType &) override {}
  virtual void visit (HIR::ImplTraitTypeOneBound &) override {}
  virtual void visit (HIR::TraitObjectTypeOneBound &) override {}
  virtual void visit (HIR::TupleType &) override {}
  virtual void visit (HIR::NeverType &) override {}
  virtual void visit (HIR::RawPointerType &) override {}
  virtual void visit (HIR::ReferenceType &) override {}
  virtual void visit (HIR::ArrayType &) override {}
  virtual void visit (HIR::SliceType &) override {}
  virtual void visit (HIR::InferredType &) override {}
  virtual void visit (HIR::BareFunctionType &) override {}

protected:
  MarkLiveBase () : mappings (Analysis::Mappings::get ()) {}

  Analysis::Mappings *mappings;
};

} // namespace Analysis
} // namespace Rust

#endif
