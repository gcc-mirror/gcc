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

#ifndef RUST_HIR_DUMP_H
#define RUST_HIR_DUMP_H

#include "rust-hir-visitor.h"
#include "rust-hir.h"
#include "rust-hir-full.h"

namespace Rust {
namespace HIR {

class Dump : public HIRFullVisitor
{
public:
  Dump (std::ostream &stream);
  void go (HIR::Crate &crate);

private:
  std::ostream &stream;
  std::size_t indent; // current indentation level
  char indent_char = '\t';

  virtual void visit (Lifetime &) override;
  virtual void visit (LifetimeParam &) override;
  virtual void visit (PathInExpression &) override;
  virtual void visit (TypePathSegment &) override;
  virtual void visit (TypePathSegmentGeneric &) override;
  virtual void visit (TypePathSegmentFunction &) override;
  virtual void visit (TypePath &) override;
  virtual void visit (QualifiedPathInExpression &) override;
  virtual void visit (QualifiedPathInType &) override;

  virtual void visit (LiteralExpr &) override;
  virtual void visit (BorrowExpr &) override;
  virtual void visit (DereferenceExpr &) override;
  virtual void visit (ErrorPropagationExpr &) override;
  virtual void visit (NegationExpr &) override;
  virtual void visit (ArithmeticOrLogicalExpr &) override;
  virtual void visit (ComparisonExpr &) override;
  virtual void visit (LazyBooleanExpr &) override;
  virtual void visit (TypeCastExpr &) override;
  virtual void visit (AssignmentExpr &) override;
  virtual void visit (CompoundAssignmentExpr &) override;
  virtual void visit (GroupedExpr &) override;

  virtual void visit (ArrayElemsValues &) override;
  virtual void visit (ArrayElemsCopied &) override;
  virtual void visit (ArrayExpr &) override;
  virtual void visit (ArrayIndexExpr &) override;
  virtual void visit (TupleExpr &) override;
  virtual void visit (TupleIndexExpr &) override;
  virtual void visit (StructExprStruct &) override;

  virtual void visit (StructExprFieldIdentifier &) override;
  virtual void visit (StructExprFieldIdentifierValue &) override;

  virtual void visit (StructExprFieldIndexValue &) override;
  virtual void visit (StructExprStructFields &) override;
  virtual void visit (StructExprStructBase &) override;

  virtual void visit (CallExpr &) override;
  virtual void visit (MethodCallExpr &) override;
  virtual void visit (FieldAccessExpr &) override;
  virtual void visit (ClosureExpr &) override;
  virtual void visit (BlockExpr &) override;
  virtual void visit (ContinueExpr &) override;
  virtual void visit (BreakExpr &) override;
  virtual void visit (RangeFromToExpr &) override;
  virtual void visit (RangeFromExpr &) override;
  virtual void visit (RangeToExpr &) override;
  virtual void visit (RangeFullExpr &) override;
  virtual void visit (RangeFromToInclExpr &) override;
  virtual void visit (RangeToInclExpr &) override;
  virtual void visit (ReturnExpr &) override;
  virtual void visit (UnsafeBlockExpr &) override;
  virtual void visit (LoopExpr &) override;
  virtual void visit (WhileLoopExpr &) override;
  virtual void visit (WhileLetLoopExpr &) override;
  virtual void visit (ForLoopExpr &) override;
  virtual void visit (IfExpr &) override;
  virtual void visit (IfExprConseqElse &) override;
  virtual void visit (IfExprConseqIf &) override;
  virtual void visit (IfExprConseqIfLet &) override;
  virtual void visit (IfLetExpr &) override;
  virtual void visit (IfLetExprConseqElse &) override;
  virtual void visit (IfLetExprConseqIf &) override;
  virtual void visit (IfLetExprConseqIfLet &) override;

  virtual void visit (MatchExpr &) override;
  virtual void visit (AwaitExpr &) override;
  virtual void visit (AsyncBlockExpr &) override;

  virtual void visit (TypeParam &) override;
  virtual void visit (ConstGenericParam &) override;

  virtual void visit (LifetimeWhereClauseItem &) override;
  virtual void visit (TypeBoundWhereClauseItem &) override;
  virtual void visit (Module &) override;
  virtual void visit (ExternCrate &) override;

  virtual void visit (UseTreeGlob &) override;
  virtual void visit (UseTreeList &) override;
  virtual void visit (UseTreeRebind &) override;
  virtual void visit (UseDeclaration &) override;
  virtual void visit (Function &) override;
  virtual void visit (TypeAlias &) override;
  virtual void visit (StructStruct &) override;
  virtual void visit (TupleStruct &) override;
  virtual void visit (EnumItem &) override;
  virtual void visit (EnumItemTuple &) override;
  virtual void visit (EnumItemStruct &) override;
  virtual void visit (EnumItemDiscriminant &) override;
  virtual void visit (Enum &) override;
  virtual void visit (Union &) override;
  virtual void visit (ConstantItem &) override;
  virtual void visit (StaticItem &) override;
  virtual void visit (TraitItemFunc &) override;
  virtual void visit (TraitItemConst &) override;
  virtual void visit (TraitItemType &) override;
  virtual void visit (Trait &) override;
  virtual void visit (ImplBlock &) override;

  virtual void visit (ExternalStaticItem &) override;
  virtual void visit (ExternalFunctionItem &) override;
  virtual void visit (ExternBlock &) override;

  virtual void visit (LiteralPattern &) override;
  virtual void visit (IdentifierPattern &) override;
  virtual void visit (WildcardPattern &) override;

  virtual void visit (RangePatternBoundLiteral &) override;
  virtual void visit (RangePatternBoundPath &) override;
  virtual void visit (RangePatternBoundQualPath &) override;
  virtual void visit (RangePattern &) override;
  virtual void visit (ReferencePattern &) override;

  virtual void visit (StructPatternFieldTuplePat &) override;
  virtual void visit (StructPatternFieldIdentPat &) override;
  virtual void visit (StructPatternFieldIdent &) override;
  virtual void visit (StructPattern &) override;

  virtual void visit (TupleStructItemsNoRange &) override;
  virtual void visit (TupleStructItemsRange &) override;
  virtual void visit (TupleStructPattern &) override;

  virtual void visit (TuplePatternItemsMultiple &) override;
  virtual void visit (TuplePatternItemsRanged &) override;
  virtual void visit (TuplePattern &) override;
  virtual void visit (SlicePattern &) override;

  virtual void visit (EmptyStmt &) override;
  virtual void visit (LetStmt &) override;
  virtual void visit (ExprStmtWithoutBlock &) override;
  virtual void visit (ExprStmtWithBlock &) override;

  virtual void visit (TraitBound &) override;
  virtual void visit (ImplTraitType &) override;
  virtual void visit (TraitObjectType &) override;
  virtual void visit (ParenthesisedType &) override;
  virtual void visit (ImplTraitTypeOneBound &) override;
  virtual void visit (TupleType &) override;
  virtual void visit (NeverType &) override;
  virtual void visit (RawPointerType &) override;
  virtual void visit (ReferenceType &) override;
  virtual void visit (ArrayType &) override;
  virtual void visit (SliceType &) override;
  virtual void visit (InferredType &) override;
  virtual void visit (BareFunctionType &) override;
};

} // namespace HIR
} // namespace Rust

#endif // !RUST_HIR_DUMP_H
