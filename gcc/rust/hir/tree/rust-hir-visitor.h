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

#ifndef RUST_HIR_VISITOR_H
#define RUST_HIR_VISITOR_H

#include "rust-hir-full-decls.h"

namespace Rust {
namespace HIR {

class HIRFullVisitor
{
public:
  virtual void visit (Lifetime &lifetime) = 0;
  virtual void visit (LifetimeParam &lifetime_param) = 0;
  virtual void visit (PathInExpression &path) = 0;
  virtual void visit (TypePathSegment &segment) = 0;
  virtual void visit (TypePathSegmentGeneric &segment) = 0;
  virtual void visit (TypePathSegmentFunction &segment) = 0;
  virtual void visit (TypePath &path) = 0;
  virtual void visit (QualifiedPathInExpression &path) = 0;
  virtual void visit (QualifiedPathInType &path) = 0;
  virtual void visit (LiteralExpr &expr) = 0;
  virtual void visit (BorrowExpr &expr) = 0;
  virtual void visit (DereferenceExpr &expr) = 0;
  virtual void visit (ErrorPropagationExpr &expr) = 0;
  virtual void visit (NegationExpr &expr) = 0;
  virtual void visit (ArithmeticOrLogicalExpr &expr) = 0;
  virtual void visit (ComparisonExpr &expr) = 0;
  virtual void visit (LazyBooleanExpr &expr) = 0;
  virtual void visit (TypeCastExpr &expr) = 0;
  virtual void visit (AssignmentExpr &expr) = 0;
  virtual void visit (CompoundAssignmentExpr &expr) = 0;
  virtual void visit (GroupedExpr &expr) = 0;
  virtual void visit (ArrayElemsValues &elems) = 0;
  virtual void visit (ArrayElemsCopied &elems) = 0;
  virtual void visit (ArrayExpr &expr) = 0;
  virtual void visit (ArrayIndexExpr &expr) = 0;
  virtual void visit (TupleExpr &expr) = 0;
  virtual void visit (TupleIndexExpr &expr) = 0;
  virtual void visit (StructExprStruct &expr) = 0;
  virtual void visit (StructExprFieldIdentifier &field) = 0;
  virtual void visit (StructExprFieldIdentifierValue &field) = 0;
  virtual void visit (StructExprFieldIndexValue &field) = 0;
  virtual void visit (StructExprStructFields &expr) = 0;
  virtual void visit (StructExprStructBase &expr) = 0;
  virtual void visit (CallExpr &expr) = 0;
  virtual void visit (MethodCallExpr &expr) = 0;
  virtual void visit (FieldAccessExpr &expr) = 0;
  virtual void visit (BlockExpr &expr) = 0;
  virtual void visit (ClosureExpr &expr) = 0;
  virtual void visit (ContinueExpr &expr) = 0;
  virtual void visit (BreakExpr &expr) = 0;
  virtual void visit (RangeFromToExpr &expr) = 0;
  virtual void visit (RangeFromExpr &expr) = 0;
  virtual void visit (RangeToExpr &expr) = 0;
  virtual void visit (RangeFullExpr &expr) = 0;
  virtual void visit (RangeFromToInclExpr &expr) = 0;
  virtual void visit (RangeToInclExpr &expr) = 0;
  virtual void visit (ReturnExpr &expr) = 0;
  virtual void visit (UnsafeBlockExpr &expr) = 0;
  virtual void visit (LoopExpr &expr) = 0;
  virtual void visit (WhileLoopExpr &expr) = 0;
  virtual void visit (WhileLetLoopExpr &expr) = 0;
  virtual void visit (ForLoopExpr &expr) = 0;
  virtual void visit (IfExpr &expr) = 0;
  virtual void visit (IfExprConseqElse &expr) = 0;
  virtual void visit (IfExprConseqIf &expr) = 0;
  virtual void visit (IfExprConseqIfLet &expr) = 0;
  virtual void visit (IfLetExpr &expr) = 0;
  virtual void visit (IfLetExprConseqElse &expr) = 0;
  virtual void visit (IfLetExprConseqIf &expr) = 0;
  virtual void visit (IfLetExprConseqIfLet &expr) = 0;
  virtual void visit (MatchExpr &expr) = 0;
  virtual void visit (AwaitExpr &expr) = 0;
  virtual void visit (AsyncBlockExpr &expr) = 0;
  virtual void visit (TypeParam &param) = 0;
  virtual void visit (ConstGenericParam &param) = 0;
  virtual void visit (LifetimeWhereClauseItem &item) = 0;
  virtual void visit (TypeBoundWhereClauseItem &item) = 0;
  virtual void visit (Module &module) = 0;
  virtual void visit (ExternCrate &crate) = 0;
  virtual void visit (UseTreeGlob &use_tree) = 0;
  virtual void visit (UseTreeList &use_tree) = 0;
  virtual void visit (UseTreeRebind &use_tree) = 0;
  virtual void visit (UseDeclaration &use_decl) = 0;
  virtual void visit (Function &function) = 0;
  virtual void visit (TypeAlias &type_alias) = 0;
  virtual void visit (StructStruct &struct_item) = 0;
  virtual void visit (TupleStruct &tuple_struct) = 0;
  virtual void visit (EnumItem &item) = 0;
  virtual void visit (EnumItemTuple &item) = 0;
  virtual void visit (EnumItemStruct &item) = 0;
  virtual void visit (EnumItemDiscriminant &item) = 0;
  virtual void visit (Enum &enum_item) = 0;
  virtual void visit (Union &union_item) = 0;
  virtual void visit (ConstantItem &const_item) = 0;
  virtual void visit (StaticItem &static_item) = 0;
  virtual void visit (TraitItemFunc &item) = 0;
  virtual void visit (TraitItemConst &item) = 0;
  virtual void visit (TraitItemType &item) = 0;
  virtual void visit (Trait &trait) = 0;
  virtual void visit (ImplBlock &impl) = 0;
  virtual void visit (ExternalStaticItem &item) = 0;
  virtual void visit (ExternalFunctionItem &item) = 0;
  virtual void visit (ExternBlock &block) = 0;
  virtual void visit (LiteralPattern &pattern) = 0;
  virtual void visit (IdentifierPattern &pattern) = 0;
  virtual void visit (WildcardPattern &pattern) = 0;
  virtual void visit (RangePatternBoundLiteral &bound) = 0;
  virtual void visit (RangePatternBoundPath &bound) = 0;
  virtual void visit (RangePatternBoundQualPath &bound) = 0;
  virtual void visit (RangePattern &pattern) = 0;
  virtual void visit (ReferencePattern &pattern) = 0;
  virtual void visit (StructPatternFieldTuplePat &field) = 0;
  virtual void visit (StructPatternFieldIdentPat &field) = 0;
  virtual void visit (StructPatternFieldIdent &field) = 0;
  virtual void visit (StructPattern &pattern) = 0;
  virtual void visit (TupleStructItemsNoRange &tuple_items) = 0;
  virtual void visit (TupleStructItemsRange &tuple_items) = 0;
  virtual void visit (TupleStructPattern &pattern) = 0;
  virtual void visit (TuplePatternItemsMultiple &tuple_items) = 0;
  virtual void visit (TuplePatternItemsRanged &tuple_items) = 0;
  virtual void visit (TuplePattern &pattern) = 0;
  virtual void visit (SlicePattern &pattern) = 0;
  virtual void visit (EmptyStmt &stmt) = 0;
  virtual void visit (LetStmt &stmt) = 0;
  virtual void visit (ExprStmtWithoutBlock &stmt) = 0;
  virtual void visit (ExprStmtWithBlock &stmt) = 0;
  virtual void visit (TraitBound &bound) = 0;
  virtual void visit (ImplTraitType &type) = 0;
  virtual void visit (TraitObjectType &type) = 0;
  virtual void visit (ParenthesisedType &type) = 0;
  virtual void visit (ImplTraitTypeOneBound &type) = 0;
  virtual void visit (TupleType &type) = 0;
  virtual void visit (NeverType &type) = 0;
  virtual void visit (RawPointerType &type) = 0;
  virtual void visit (ReferenceType &type) = 0;
  virtual void visit (ArrayType &type) = 0;
  virtual void visit (SliceType &type) = 0;
  virtual void visit (InferredType &type) = 0;
  virtual void visit (BareFunctionType &type) = 0;
};

class HIRFullVisitorBase : public HIRFullVisitor
{
public:
  virtual ~HIRFullVisitorBase () {}

  virtual void visit (Lifetime &) override {}
  virtual void visit (LifetimeParam &) override {}
  virtual void visit (PathInExpression &) override {}
  virtual void visit (TypePathSegment &) override {}
  virtual void visit (TypePathSegmentGeneric &) override {}
  virtual void visit (TypePathSegmentFunction &) override {}
  virtual void visit (TypePath &) override {}
  virtual void visit (QualifiedPathInExpression &) override {}
  virtual void visit (QualifiedPathInType &) override {}

  virtual void visit (LiteralExpr &) override {}
  virtual void visit (BorrowExpr &) override {}
  virtual void visit (DereferenceExpr &) override {}
  virtual void visit (ErrorPropagationExpr &) override {}
  virtual void visit (NegationExpr &) override {}
  virtual void visit (ArithmeticOrLogicalExpr &) override {}
  virtual void visit (ComparisonExpr &) override {}
  virtual void visit (LazyBooleanExpr &) override {}
  virtual void visit (TypeCastExpr &) override {}
  virtual void visit (AssignmentExpr &) override {}
  virtual void visit (CompoundAssignmentExpr &) override {}
  virtual void visit (GroupedExpr &) override {}

  virtual void visit (ArrayElemsValues &) override {}
  virtual void visit (ArrayElemsCopied &) override {}
  virtual void visit (ArrayExpr &) override {}
  virtual void visit (ArrayIndexExpr &) override {}
  virtual void visit (TupleExpr &) override {}
  virtual void visit (TupleIndexExpr &) override {}
  virtual void visit (StructExprStruct &) override {}

  virtual void visit (StructExprFieldIdentifier &) override {}
  virtual void visit (StructExprFieldIdentifierValue &) override {}

  virtual void visit (StructExprFieldIndexValue &) override {}
  virtual void visit (StructExprStructFields &) override {}
  virtual void visit (StructExprStructBase &) override {}

  virtual void visit (CallExpr &) override {}
  virtual void visit (MethodCallExpr &) override {}
  virtual void visit (FieldAccessExpr &) override {}
  virtual void visit (ClosureExpr &) override {}
  virtual void visit (BlockExpr &) override {}
  virtual void visit (ContinueExpr &) override {}
  virtual void visit (BreakExpr &) override {}
  virtual void visit (RangeFromToExpr &) override {}
  virtual void visit (RangeFromExpr &) override {}
  virtual void visit (RangeToExpr &) override {}
  virtual void visit (RangeFullExpr &) override {}
  virtual void visit (RangeFromToInclExpr &) override {}
  virtual void visit (RangeToInclExpr &) override {}
  virtual void visit (ReturnExpr &) override {}
  virtual void visit (UnsafeBlockExpr &) override {}
  virtual void visit (LoopExpr &) override {}
  virtual void visit (WhileLoopExpr &) override {}
  virtual void visit (WhileLetLoopExpr &) override {}
  virtual void visit (ForLoopExpr &) override {}
  virtual void visit (IfExpr &) override {}
  virtual void visit (IfExprConseqElse &) override {}
  virtual void visit (IfExprConseqIf &) override {}
  virtual void visit (IfExprConseqIfLet &) override {}
  virtual void visit (IfLetExpr &) override {}
  virtual void visit (IfLetExprConseqElse &) override {}
  virtual void visit (IfLetExprConseqIf &) override {}
  virtual void visit (IfLetExprConseqIfLet &) override {}

  virtual void visit (MatchExpr &) override {}
  virtual void visit (AwaitExpr &) override {}
  virtual void visit (AsyncBlockExpr &) override {}

  virtual void visit (TypeParam &) override {}
  virtual void visit (ConstGenericParam &) override {}

  virtual void visit (LifetimeWhereClauseItem &) override {}
  virtual void visit (TypeBoundWhereClauseItem &) override {}
  virtual void visit (Module &) override {}
  virtual void visit (ExternCrate &) override {}

  virtual void visit (UseTreeGlob &) override {}
  virtual void visit (UseTreeList &) override {}
  virtual void visit (UseTreeRebind &) override {}
  virtual void visit (UseDeclaration &) override {}
  virtual void visit (Function &) override {}
  virtual void visit (TypeAlias &) override {}
  virtual void visit (StructStruct &) override {}
  virtual void visit (TupleStruct &) override {}
  virtual void visit (EnumItem &) override {}
  virtual void visit (EnumItemTuple &) override {}
  virtual void visit (EnumItemStruct &) override {}
  virtual void visit (EnumItemDiscriminant &) override {}
  virtual void visit (Enum &) override {}
  virtual void visit (Union &) override {}
  virtual void visit (ConstantItem &) override {}
  virtual void visit (StaticItem &) override {}
  virtual void visit (TraitItemFunc &) override {}
  virtual void visit (TraitItemConst &) override {}
  virtual void visit (TraitItemType &) override {}
  virtual void visit (Trait &) override {}
  virtual void visit (ImplBlock &) override {}

  virtual void visit (ExternalStaticItem &) override {}
  virtual void visit (ExternalFunctionItem &) override {}
  virtual void visit (ExternBlock &) override {}

  virtual void visit (LiteralPattern &) override {}
  virtual void visit (IdentifierPattern &) override {}
  virtual void visit (WildcardPattern &) override {}

  virtual void visit (RangePatternBoundLiteral &) override {}
  virtual void visit (RangePatternBoundPath &) override {}
  virtual void visit (RangePatternBoundQualPath &) override {}
  virtual void visit (RangePattern &) override {}
  virtual void visit (ReferencePattern &) override {}

  virtual void visit (StructPatternFieldTuplePat &) override {}
  virtual void visit (StructPatternFieldIdentPat &) override {}
  virtual void visit (StructPatternFieldIdent &) override {}
  virtual void visit (StructPattern &) override {}

  virtual void visit (TupleStructItemsNoRange &) override {}
  virtual void visit (TupleStructItemsRange &) override {}
  virtual void visit (TupleStructPattern &) override {}

  virtual void visit (TuplePatternItemsMultiple &) override {}
  virtual void visit (TuplePatternItemsRanged &) override {}
  virtual void visit (TuplePattern &) override {}
  virtual void visit (SlicePattern &) override {}

  virtual void visit (EmptyStmt &) override {}
  virtual void visit (LetStmt &) override {}
  virtual void visit (ExprStmtWithoutBlock &) override {}
  virtual void visit (ExprStmtWithBlock &) override {}

  virtual void visit (TraitBound &) override {}
  virtual void visit (ImplTraitType &) override {}
  virtual void visit (TraitObjectType &) override {}
  virtual void visit (ParenthesisedType &) override {}
  virtual void visit (ImplTraitTypeOneBound &) override {}
  virtual void visit (TupleType &) override {}
  virtual void visit (NeverType &) override {}
  virtual void visit (RawPointerType &) override {}
  virtual void visit (ReferenceType &) override {}
  virtual void visit (ArrayType &) override {}
  virtual void visit (SliceType &) override {}
  virtual void visit (InferredType &) override {}
  virtual void visit (BareFunctionType &) override {}
};

class HIRExternalItemVisitor
{
public:
  virtual void visit (ExternalStaticItem &item) = 0;
  virtual void visit (ExternalFunctionItem &item) = 0;
};

class HIRTraitItemVisitor
{
public:
  virtual void visit (TraitItemFunc &item) = 0;
  virtual void visit (TraitItemConst &item) = 0;
  virtual void visit (TraitItemType &item) = 0;
};

class HIRVisItemVisitor
{
public:
  virtual void visit (Module &module) = 0;
  virtual void visit (ExternCrate &crate) = 0;
  virtual void visit (UseDeclaration &use_decl) = 0;
  virtual void visit (Function &function) = 0;
  virtual void visit (TypeAlias &type_alias) = 0;
  virtual void visit (StructStruct &struct_item) = 0;
  virtual void visit (TupleStruct &tuple_struct) = 0;
  virtual void visit (Enum &enum_item) = 0;
  virtual void visit (Union &union_item) = 0;
  virtual void visit (ConstantItem &const_item) = 0;
  virtual void visit (StaticItem &static_item) = 0;
  virtual void visit (Trait &trait) = 0;
  virtual void visit (ImplBlock &impl) = 0;
  virtual void visit (ExternBlock &block) = 0;
};

class HIRImplVisitor
{
public:
  virtual void visit (Function &function) = 0;
  virtual void visit (ConstantItem &const_item) = 0;
  virtual void visit (TypeAlias &type_alias) = 0;
};

class HIRTypeVisitor
{
public:
  virtual void visit (TypePathSegmentFunction &segment) = 0;
  virtual void visit (TypePath &path) = 0;
  virtual void visit (QualifiedPathInType &path) = 0;
  virtual void visit (TraitBound &bound) = 0;
  virtual void visit (ImplTraitType &type) = 0;
  virtual void visit (TraitObjectType &type) = 0;
  virtual void visit (ParenthesisedType &type) = 0;
  virtual void visit (ImplTraitTypeOneBound &type) = 0;
  virtual void visit (TupleType &type) = 0;
  virtual void visit (NeverType &type) = 0;
  virtual void visit (RawPointerType &type) = 0;
  virtual void visit (ReferenceType &type) = 0;
  virtual void visit (ArrayType &type) = 0;
  virtual void visit (SliceType &type) = 0;
  virtual void visit (InferredType &type) = 0;
  virtual void visit (BareFunctionType &type) = 0;
};

class HIRStmtVisitor
{
public:
  virtual void visit (EnumItemTuple &) = 0;
  virtual void visit (EnumItemStruct &) = 0;
  virtual void visit (EnumItem &item) = 0;
  virtual void visit (TupleStruct &tuple_struct) = 0;
  virtual void visit (EnumItemDiscriminant &) = 0;
  virtual void visit (TypePathSegmentFunction &segment) = 0;
  virtual void visit (TypePath &path) = 0;
  virtual void visit (QualifiedPathInType &path) = 0;
  virtual void visit (Module &module) = 0;
  virtual void visit (ExternCrate &crate) = 0;
  virtual void visit (UseDeclaration &use_decl) = 0;
  virtual void visit (Function &function) = 0;
  virtual void visit (TypeAlias &type_alias) = 0;
  virtual void visit (StructStruct &struct_item) = 0;
  virtual void visit (Enum &enum_item) = 0;
  virtual void visit (Union &union_item) = 0;
  virtual void visit (ConstantItem &const_item) = 0;
  virtual void visit (StaticItem &static_item) = 0;
  virtual void visit (Trait &trait) = 0;
  virtual void visit (ImplBlock &impl) = 0;
  virtual void visit (ExternBlock &block) = 0;
  virtual void visit (EmptyStmt &stmt) = 0;
  virtual void visit (LetStmt &stmt) = 0;
  virtual void visit (ExprStmtWithoutBlock &stmt) = 0;
  virtual void visit (ExprStmtWithBlock &stmt) = 0;
};

class HIRExpressionVisitor
{
public:
  // These are StructExprField
  // Added because of CompileStructExprField
  virtual void visit (StructExprFieldIdentifier &field) = 0;
  virtual void visit (StructExprFieldIdentifierValue &field) = 0;
  virtual void visit (StructExprFieldIndexValue &field) = 0;

  virtual void visit (HIR::QualifiedPathInExpression &expr) = 0;
  virtual void visit (HIR::PathInExpression &expr) = 0;
  virtual void visit (ClosureExpr &) = 0;
  virtual void visit (StructExprStructFields &) = 0;
  virtual void visit (StructExprStruct &) = 0;
  virtual void visit (LiteralExpr &expr) = 0;
  virtual void visit (BorrowExpr &expr) = 0;
  virtual void visit (DereferenceExpr &expr) = 0;
  virtual void visit (ErrorPropagationExpr &expr) = 0;
  virtual void visit (NegationExpr &expr) = 0;
  virtual void visit (ArithmeticOrLogicalExpr &expr) = 0;
  virtual void visit (ComparisonExpr &expr) = 0;
  virtual void visit (LazyBooleanExpr &expr) = 0;
  virtual void visit (TypeCastExpr &expr) = 0;
  virtual void visit (AssignmentExpr &expr) = 0;
  virtual void visit (CompoundAssignmentExpr &expr) = 0;
  virtual void visit (GroupedExpr &expr) = 0;
  virtual void visit (ArrayExpr &expr) = 0;
  virtual void visit (ArrayIndexExpr &expr) = 0;
  virtual void visit (TupleExpr &expr) = 0;
  virtual void visit (TupleIndexExpr &expr) = 0;
  virtual void visit (CallExpr &expr) = 0;
  virtual void visit (MethodCallExpr &expr) = 0;
  virtual void visit (FieldAccessExpr &expr) = 0;
  virtual void visit (BlockExpr &expr) = 0;
  virtual void visit (ContinueExpr &expr) = 0;
  virtual void visit (BreakExpr &expr) = 0;
  virtual void visit (RangeFromToExpr &expr) = 0;
  virtual void visit (RangeFromExpr &expr) = 0;
  virtual void visit (RangeToExpr &expr) = 0;
  virtual void visit (RangeFullExpr &expr) = 0;
  virtual void visit (RangeFromToInclExpr &expr) = 0;
  virtual void visit (RangeToInclExpr &expr) = 0;
  virtual void visit (ReturnExpr &expr) = 0;
  virtual void visit (UnsafeBlockExpr &expr) = 0;
  virtual void visit (LoopExpr &expr) = 0;
  virtual void visit (WhileLoopExpr &expr) = 0;
  virtual void visit (WhileLetLoopExpr &expr) = 0;
  virtual void visit (ForLoopExpr &expr) = 0;
  virtual void visit (IfExpr &expr) = 0;
  virtual void visit (IfExprConseqElse &expr) = 0;
  virtual void visit (IfExprConseqIf &expr) = 0;
  virtual void visit (IfExprConseqIfLet &expr) = 0;
  virtual void visit (IfLetExpr &expr) = 0;
  virtual void visit (IfLetExprConseqElse &expr) = 0;
  virtual void visit (IfLetExprConseqIf &expr) = 0;
  virtual void visit (IfLetExprConseqIfLet &expr) = 0;
  virtual void visit (MatchExpr &expr) = 0;
  virtual void visit (AwaitExpr &expr) = 0;
  virtual void visit (AsyncBlockExpr &expr) = 0;
};

class HIRPatternVisitor
{
public:
  virtual void visit (IdentifierPattern &) = 0;
  virtual void visit (LiteralPattern &) = 0;
  virtual void visit (PathInExpression &) = 0;
  virtual void visit (QualifiedPathInExpression &) = 0;
  virtual void visit (RangePattern &) = 0;
  virtual void visit (ReferencePattern &) = 0;
  virtual void visit (SlicePattern &) = 0;
  virtual void visit (StructPattern &) = 0;
  virtual void visit (TuplePattern &) = 0;
  virtual void visit (TupleStructPattern &) = 0;
  virtual void visit (WildcardPattern &) = 0;
};

} // namespace HIR
} // namespace Rust

#endif
