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

#ifndef RUST_HIR_VISITOR_H
#define RUST_HIR_VISITOR_H

#include "rust-hir-full-decls.h"
#include "rust-ast.h"

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
  virtual void visit (AnonConst &expr) = 0;
  virtual void visit (ConstBlock &expr) = 0;
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
  virtual void visit (IfExpr &expr) = 0;
  virtual void visit (IfExprConseqElse &expr) = 0;
  virtual void visit (MatchExpr &expr) = 0;
  virtual void visit (AwaitExpr &expr) = 0;
  virtual void visit (AsyncBlockExpr &expr) = 0;
  virtual void visit (InlineAsm &expr) = 0;
  virtual void visit (LlvmInlineAsm &expr) = 0;
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
  virtual void visit (ExternalTypeItem &item) = 0;
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
  virtual void visit (AltPattern &pattern) = 0;
  virtual void visit (EmptyStmt &stmt) = 0;
  virtual void visit (LetStmt &stmt) = 0;
  virtual void visit (ExprStmt &stmt) = 0;
  virtual void visit (TraitBound &bound) = 0;
  virtual void visit (ImplTraitType &type) = 0;
  virtual void visit (TraitObjectType &type) = 0;
  virtual void visit (ParenthesisedType &type) = 0;
  virtual void visit (TupleType &type) = 0;
  virtual void visit (NeverType &type) = 0;
  virtual void visit (RawPointerType &type) = 0;
  virtual void visit (ReferenceType &type) = 0;
  virtual void visit (ArrayType &type) = 0;
  virtual void visit (SliceType &type) = 0;
  virtual void visit (InferredType &type) = 0;
  virtual void visit (BareFunctionType &type) = 0;
};

class DefaultHIRVisitor : public HIRFullVisitor
{
public:
  virtual void visit_where_clause (WhereClause &);
  virtual void visit_where_clause (const WhereClause &);
  virtual void visit_named_function_param (NamedFunctionParam &param);
  virtual void visit_function_param (FunctionParam &param);
  virtual void visit_self_param (SelfParam &param);
  virtual void visit_match_arm (MatchArm &arm);
  virtual void visit_match_case (MatchCase &);
  virtual void visit_struct_field (StructField &field);
  virtual void visit_generic_args (GenericArgs &args);
  virtual void visit_qualified_path_type (QualifiedPathType &);
  virtual void visit_path_expr_segment (PathExprSegment &segment);
  virtual void visit_closure_param (ClosureParam &param);
  virtual void visit_loop_label (LoopLabel &);

  virtual void visit_attribute (AST::Attribute &attr)
  {
    visit_attribute (static_cast<const AST::Attribute &> (attr));
  }
  virtual void visit_attribute (const AST::Attribute &attr) {}
  template <typename T> void visit_outer_attrs (T &node)
  {
    for (auto &attr : node.get_outer_attrs ())
      visit_attribute (attr);
  }
  template <typename T> void visit_inner_attrs (T &node)
  {
    for (auto &attr : node.get_inner_attrs ())
      visit_attribute (attr);
  }

  virtual void visit (WhereClauseItem &node) { walk (node); }

  virtual void visit (Lifetime &node) override { walk (node); }
  virtual void visit (LifetimeParam &node) override { walk (node); }
  virtual void visit (PathInExpression &node) override { walk (node); }
  virtual void visit (TypePathSegment &node) override { walk (node); }
  virtual void visit (TypePathSegmentGeneric &node) override { walk (node); }
  virtual void visit (TypePathSegmentFunction &node) override { walk (node); }
  virtual void visit (TypePath &node) override { walk (node); }
  virtual void visit (QualifiedPathInExpression &node) override { walk (node); }
  virtual void visit (QualifiedPathInType &node) override { walk (node); }
  virtual void visit (LiteralExpr &node) override { walk (node); }
  virtual void visit (BorrowExpr &node) override { walk (node); }
  virtual void visit (DereferenceExpr &node) override { walk (node); }
  virtual void visit (ErrorPropagationExpr &node) override { walk (node); }
  virtual void visit (NegationExpr &node) override { walk (node); }
  virtual void visit (ArithmeticOrLogicalExpr &node) override { walk (node); }
  virtual void visit (ComparisonExpr &node) override { walk (node); }
  virtual void visit (LazyBooleanExpr &node) override { walk (node); }
  virtual void visit (TypeCastExpr &node) override { walk (node); }
  virtual void visit (AssignmentExpr &node) override { walk (node); }
  virtual void visit (CompoundAssignmentExpr &node) override { walk (node); }
  virtual void visit (GroupedExpr &node) override { walk (node); }
  virtual void visit (ArrayElemsValues &node) override { walk (node); }
  virtual void visit (ArrayElemsCopied &node) override { walk (node); }
  virtual void visit (ArrayExpr &node) override { walk (node); }
  virtual void visit (ArrayIndexExpr &node) override { walk (node); }
  virtual void visit (TupleExpr &node) override { walk (node); }
  virtual void visit (TupleIndexExpr &node) override { walk (node); }
  virtual void visit (StructExprStruct &node) override { walk (node); }
  virtual void visit (StructExprFieldIdentifier &node) override { walk (node); }
  virtual void visit (StructExprFieldIdentifierValue &node) override
  {
    walk (node);
  }
  virtual void visit (StructExprFieldIndexValue &node) override { walk (node); }
  virtual void visit (StructExprStructFields &node) override { walk (node); }
  virtual void visit (StructExprStructBase &node) override { walk (node); }
  virtual void visit (CallExpr &node) override { walk (node); }
  virtual void visit (MethodCallExpr &node) override { walk (node); }
  virtual void visit (FieldAccessExpr &node) override { walk (node); }
  virtual void visit (ClosureExpr &node) override { walk (node); }
  virtual void visit (BlockExpr &node) override { walk (node); }
  virtual void visit (AnonConst &node) override { walk (node); }
  virtual void visit (ConstBlock &node) override { walk (node); }
  virtual void visit (ContinueExpr &node) override { walk (node); }
  virtual void visit (BreakExpr &node) override { walk (node); }
  virtual void visit (RangeFromToExpr &node) override { walk (node); }
  virtual void visit (RangeFromExpr &node) override { walk (node); }
  virtual void visit (RangeToExpr &node) override { walk (node); }
  virtual void visit (RangeFullExpr &node) override { walk (node); }
  virtual void visit (RangeFromToInclExpr &node) override { walk (node); }
  virtual void visit (RangeToInclExpr &node) override { walk (node); }
  virtual void visit (ReturnExpr &node) override { walk (node); }
  virtual void visit (UnsafeBlockExpr &node) override { walk (node); }
  virtual void visit (LoopExpr &node) override { walk (node); }
  virtual void visit (WhileLoopExpr &node) override { walk (node); }
  virtual void visit (WhileLetLoopExpr &node) override { walk (node); }
  virtual void visit (IfExpr &node) override { walk (node); }
  virtual void visit (IfExprConseqElse &node) override { walk (node); }
  virtual void visit (MatchExpr &node) override { walk (node); }
  virtual void visit (AwaitExpr &node) override { walk (node); }
  virtual void visit (AsyncBlockExpr &node) override { walk (node); }
  virtual void visit (InlineAsm &node) override { walk (node); }
  virtual void visit (LlvmInlineAsm &node) override { walk (node); }
  virtual void visit (TypeParam &node) override { walk (node); }
  virtual void visit (ConstGenericParam &node) override { walk (node); }
  virtual void visit (LifetimeWhereClauseItem &node) override { walk (node); }
  virtual void visit (TypeBoundWhereClauseItem &node) override { walk (node); }
  virtual void visit (Module &node) override { walk (node); }
  virtual void visit (ExternCrate &node) override { walk (node); }
  virtual void visit (UseTreeGlob &node) override { walk (node); }
  virtual void visit (UseTreeList &node) override { walk (node); }
  virtual void visit (UseTreeRebind &node) override { walk (node); }
  virtual void visit (UseDeclaration &node) override { walk (node); }
  virtual void visit (Function &node) override { walk (node); }
  virtual void visit (TypeAlias &node) override { walk (node); }
  virtual void visit (StructStruct &node) override { walk (node); }
  virtual void visit (TupleStruct &node) override { walk (node); }
  virtual void visit (EnumItem &node) override { walk (node); }
  virtual void visit (EnumItemTuple &node) override { walk (node); }
  virtual void visit (EnumItemStruct &node) override { walk (node); }
  virtual void visit (EnumItemDiscriminant &node) override { walk (node); }
  virtual void visit (Enum &node) override { walk (node); }
  virtual void visit (Union &node) override { walk (node); }
  virtual void visit (ConstantItem &node) override { walk (node); }
  virtual void visit (StaticItem &node) override { walk (node); }
  virtual void visit (TraitItemFunc &node) override { walk (node); }
  virtual void visit (TraitItemConst &node) override { walk (node); }
  virtual void visit (TraitItemType &node) override { walk (node); }
  virtual void visit (Trait &node) override { walk (node); }
  virtual void visit (ImplBlock &node) override { walk (node); }
  virtual void visit (ExternalStaticItem &node) override { walk (node); }
  virtual void visit (ExternalFunctionItem &node) override { walk (node); }
  virtual void visit (ExternalTypeItem &node) override { walk (node); }
  virtual void visit (ExternBlock &node) override { walk (node); }
  virtual void visit (LiteralPattern &node) override { walk (node); }
  virtual void visit (IdentifierPattern &node) override { walk (node); }
  virtual void visit (WildcardPattern &node) override { walk (node); }
  virtual void visit (RangePatternBoundLiteral &node) override { walk (node); }
  virtual void visit (RangePatternBoundPath &node) override { walk (node); }
  virtual void visit (RangePatternBoundQualPath &node) override { walk (node); }
  virtual void visit (RangePattern &node) override { walk (node); }
  virtual void visit (ReferencePattern &node) override { walk (node); }
  virtual void visit (StructPatternFieldTuplePat &node) override
  {
    walk (node);
  }
  virtual void visit (StructPatternFieldIdentPat &node) override
  {
    walk (node);
  }
  virtual void visit (StructPatternFieldIdent &node) override { walk (node); }
  virtual void visit (StructPattern &node) override { walk (node); }
  virtual void visit (TupleStructItemsNoRange &node) override { walk (node); }
  virtual void visit (TupleStructItemsRange &node) override { walk (node); }
  virtual void visit (TupleStructPattern &node) override { walk (node); }
  virtual void visit (TuplePatternItemsMultiple &node) override { walk (node); }
  virtual void visit (TuplePatternItemsRanged &node) override { walk (node); }
  virtual void visit (TuplePattern &node) override { walk (node); }
  virtual void visit (SlicePattern &node) override { walk (node); }
  virtual void visit (AltPattern &node) override { walk (node); }
  virtual void visit (EmptyStmt &node) override { walk (node); }
  virtual void visit (LetStmt &node) override { walk (node); }
  virtual void visit (ExprStmt &node) override { walk (node); }
  virtual void visit (TraitBound &node) override { walk (node); }
  virtual void visit (ImplTraitType &node) override { walk (node); }
  virtual void visit (TraitObjectType &node) override { walk (node); }
  virtual void visit (ParenthesisedType &node) override { walk (node); }
  virtual void visit (TupleType &node) override { walk (node); }
  virtual void visit (NeverType &node) override { walk (node); }
  virtual void visit (RawPointerType &node) override { walk (node); }
  virtual void visit (ReferenceType &node) override { walk (node); }
  virtual void visit (ArrayType &node) override { walk (node); }
  virtual void visit (SliceType &node) override { walk (node); }
  virtual void visit (InferredType &node) override { walk (node); }
  virtual void visit (BareFunctionType &node) override { walk (node); }

protected:
  virtual void walk (WhereClauseItem &) final;

  virtual void walk (Lifetime &) final;
  virtual void walk (LifetimeParam &) final;
  virtual void walk (PathInExpression &) final;
  virtual void walk (TypePathSegment &) final;
  virtual void walk (TypePathSegmentGeneric &) final;
  virtual void walk (TypePathSegmentFunction &) final;
  virtual void walk (TypePath &) final;
  virtual void walk (QualifiedPathInExpression &) final;
  virtual void walk (QualifiedPathInType &) final;

  virtual void walk (LiteralExpr &) final;
  virtual void walk (BorrowExpr &) final;
  virtual void walk (DereferenceExpr &) final;
  virtual void walk (ErrorPropagationExpr &) final;
  virtual void walk (NegationExpr &) final;
  virtual void walk (ArithmeticOrLogicalExpr &) final;
  virtual void walk (ComparisonExpr &) final;
  virtual void walk (LazyBooleanExpr &) final;
  virtual void walk (TypeCastExpr &) final;
  virtual void walk (AssignmentExpr &) final;
  virtual void walk (CompoundAssignmentExpr &) final;
  virtual void walk (GroupedExpr &) final;

  virtual void walk (ArrayElemsValues &) final;
  virtual void walk (ArrayElemsCopied &) final;
  virtual void walk (ArrayExpr &) final;
  virtual void walk (ArrayIndexExpr &) final;
  virtual void walk (TupleExpr &) final;
  virtual void walk (TupleIndexExpr &) final;
  virtual void walk (StructExprStruct &) final;
  virtual void walk (StructExprFieldIdentifier &) final;
  virtual void walk (StructExprFieldIdentifierValue &) final;
  virtual void walk (StructExprFieldIndexValue &) final;
  virtual void walk (StructExprStructFields &) final;
  virtual void walk (StructExprStructBase &) final;
  virtual void walk (CallExpr &) final;
  virtual void walk (MethodCallExpr &) final;
  virtual void walk (FieldAccessExpr &) final;
  virtual void walk (ClosureExpr &) final;
  virtual void walk (BlockExpr &) final;
  virtual void walk (AnonConst &) final;
  virtual void walk (ConstBlock &) final;
  virtual void walk (ContinueExpr &) final;
  virtual void walk (BreakExpr &) final;
  virtual void walk (RangeFromToExpr &) final;
  virtual void walk (RangeFromExpr &) final;
  virtual void walk (RangeToExpr &) final;
  virtual void walk (RangeFullExpr &) final;
  virtual void walk (RangeFromToInclExpr &) final;
  virtual void walk (RangeToInclExpr &) final;
  virtual void walk (ReturnExpr &) final;
  virtual void walk (UnsafeBlockExpr &) final;
  virtual void walk (LoopExpr &) final;
  virtual void walk (WhileLoopExpr &) final;
  virtual void walk (WhileLetLoopExpr &) final;
  virtual void walk (IfExpr &) final;
  virtual void walk (IfExprConseqElse &) final;
  virtual void walk (MatchExpr &) final;
  virtual void walk (AwaitExpr &) final;
  virtual void walk (AsyncBlockExpr &) final;
  virtual void walk (InlineAsm &) final;
  virtual void walk (LlvmInlineAsm &) final;
  virtual void walk (TypeParam &) final;
  virtual void walk (ConstGenericParam &) final;
  virtual void walk (LifetimeWhereClauseItem &) final;
  virtual void walk (TypeBoundWhereClauseItem &) final;
  virtual void walk (Module &) final;
  virtual void walk (ExternCrate &) final;
  virtual void walk (UseTreeGlob &) final;
  virtual void walk (UseTreeList &) final;
  virtual void walk (UseTreeRebind &) final;
  virtual void walk (UseDeclaration &) final;
  virtual void walk (Function &) final;
  virtual void walk (TypeAlias &) final;
  virtual void walk (StructStruct &) final;
  virtual void walk (TupleStruct &) final;
  virtual void walk (EnumItem &) final;
  virtual void walk (EnumItemTuple &) final;
  virtual void walk (EnumItemStruct &) final;
  virtual void walk (EnumItemDiscriminant &) final;
  virtual void walk (Enum &) final;
  virtual void walk (Union &) final;
  virtual void walk (ConstantItem &) final;
  virtual void walk (StaticItem &) final;
  virtual void walk (TraitItemFunc &) final;
  virtual void walk (TraitItemConst &) final;
  virtual void walk (TraitItemType &) final;
  virtual void walk (Trait &) final;
  virtual void walk (ImplBlock &) final;
  virtual void walk (ExternalStaticItem &) final;
  virtual void walk (ExternalFunctionItem &) final;
  virtual void walk (ExternalTypeItem &) final;
  virtual void walk (ExternBlock &) final;
  virtual void walk (LiteralPattern &) final;
  virtual void walk (IdentifierPattern &) final;
  virtual void walk (WildcardPattern &) final;
  virtual void walk (RangePatternBoundLiteral &) final;
  virtual void walk (RangePatternBoundPath &) final;
  virtual void walk (RangePatternBoundQualPath &) final;
  virtual void walk (RangePattern &) final;
  virtual void walk (ReferencePattern &) final;
  virtual void walk (StructPatternFieldTuplePat &) final;
  virtual void walk (StructPatternFieldIdentPat &) final;
  virtual void walk (StructPatternFieldIdent &) final;
  virtual void walk (StructPattern &) final;
  virtual void walk (TupleStructItemsNoRange &) final;
  virtual void walk (TupleStructItemsRange &) final;
  virtual void walk (TupleStructPattern &) final;
  virtual void walk (TuplePatternItemsMultiple &) final;
  virtual void walk (TuplePatternItemsRanged &) final;
  virtual void walk (TuplePattern &) final;
  virtual void walk (SlicePattern &) final;
  virtual void walk (AltPattern &) final;
  virtual void walk (EmptyStmt &) final;
  virtual void walk (LetStmt &) final;
  virtual void walk (ExprStmt &) final;
  virtual void walk (TraitBound &) final;
  virtual void walk (ImplTraitType &) final;
  virtual void walk (TraitObjectType &) final;
  virtual void walk (ParenthesisedType &) final;
  virtual void walk (TupleType &) final;
  virtual void walk (NeverType &) final;
  virtual void walk (RawPointerType &) final;
  virtual void walk (ReferenceType &) final;
  virtual void walk (ArrayType &) final;
  virtual void walk (SliceType &) final;
  virtual void walk (InferredType &) final;
  virtual void walk (BareFunctionType &) final;
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
  virtual void visit (AnonConst &) override {}
  virtual void visit (ConstBlock &) override {}
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
  virtual void visit (IfExpr &) override {}
  virtual void visit (IfExprConseqElse &) override {}

  virtual void visit (MatchExpr &) override {}
  virtual void visit (AwaitExpr &) override {}
  virtual void visit (AsyncBlockExpr &) override {}
  virtual void visit (InlineAsm &) override {}
  virtual void visit (LlvmInlineAsm &) override {}

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
  virtual void visit (ExternalTypeItem &) override {}
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
  virtual void visit (AltPattern &) override {}

  virtual void visit (EmptyStmt &) override {}
  virtual void visit (LetStmt &) override {}
  virtual void visit (ExprStmt &) override {}

  virtual void visit (TraitBound &) override {}
  virtual void visit (ImplTraitType &) override {}
  virtual void visit (TraitObjectType &) override {}
  virtual void visit (ParenthesisedType &) override {}
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
  virtual void visit (ExternalTypeItem &item) = 0;
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
  virtual void visit (ExprStmt &stmt) = 0;
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
  virtual void visit (AnonConst &expr) = 0;
  virtual void visit (ConstBlock &expr) = 0;
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
  virtual void visit (IfExpr &expr) = 0;
  virtual void visit (IfExprConseqElse &expr) = 0;
  virtual void visit (InlineAsm &expr) = 0;
  virtual void visit (LlvmInlineAsm &expr) = 0;
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
  virtual void visit (AltPattern &) = 0;
  virtual void visit (StructPattern &) = 0;
  virtual void visit (TuplePattern &) = 0;
  virtual void visit (TupleStructPattern &) = 0;
  virtual void visit (WildcardPattern &) = 0;
};

} // namespace HIR
} // namespace Rust

#endif
