// Copyright (C) 2020 Free Software Foundation, Inc.

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

class HIRVisitor
{
public:
  virtual void visit (IdentifierExpr &ident_expr) = 0;
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
  virtual void visit (StructExprTuple &expr) = 0;
  virtual void visit (StructExprUnit &expr) = 0;
  virtual void visit (EnumExprFieldIdentifier &field) = 0;
  virtual void visit (EnumExprFieldIdentifierValue &field) = 0;
  virtual void visit (EnumExprFieldIndexValue &field) = 0;
  virtual void visit (EnumExprStruct &expr) = 0;
  virtual void visit (EnumExprTuple &expr) = 0;
  virtual void visit (EnumExprFieldless &expr) = 0;
  virtual void visit (CallExpr &expr) = 0;
  virtual void visit (MethodCallExpr &expr) = 0;
  virtual void visit (FieldAccessExpr &expr) = 0;
  virtual void visit (ClosureExprInner &expr) = 0;
  virtual void visit (BlockExpr &expr) = 0;
  virtual void visit (ClosureExprInnerTyped &expr) = 0;
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
  virtual void visit (LifetimeWhereClauseItem &item) = 0;
  virtual void visit (TypeBoundWhereClauseItem &item) = 0;
  virtual void visit (Method &method) = 0;
  virtual void visit (ModuleBodied &module) = 0;
  virtual void visit (ModuleNoBody &module) = 0;
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
  virtual void visit (TraitItemMethod &item) = 0;
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
  virtual void visit (GroupedPattern &pattern) = 0;
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
  virtual void visit (TraitObjectTypeOneBound &type) = 0;
  virtual void visit (TupleType &type) = 0;
  virtual void visit (NeverType &type) = 0;
  virtual void visit (RawPointerType &type) = 0;
  virtual void visit (ReferenceType &type) = 0;
  virtual void visit (ArrayType &type) = 0;
  virtual void visit (SliceType &type) = 0;
  virtual void visit (InferredType &type) = 0;
  virtual void visit (BareFunctionType &type) = 0;
};

} // namespace HIR
} // namespace Rust

#endif
