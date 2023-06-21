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

#ifndef RUST_CONST_CHECKER_H
#define RUST_CONST_CHECKER_H

#include "rust-hir-visitor.h"
#include "rust-hir-type-check.h"
#include "rust-stacked-contexts.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace HIR {
class ConstChecker : public HIRFullVisitor
{
public:
  ConstChecker ();

  void go (HIR::Crate &crate);

  /**
   * Check if an item is a const extern item or not
   * TODO: Move this to a const compilation context class or an attribute
   * checking class
   */
  static bool is_const_extern_fn (HIR::ExternalFunctionItem &fn);

private:
  /**
   * Check that only const functions are called in const contexts
   */
  void check_function_call (HirId fn_id, Location locus);

  /* All possible const contexts */
  enum class ConstGenericCtx
  {
    Function,
    TypeAlias,
    Struct,
    Enum,
    Union,
    Trait,
    Impl
  };

  /* Get the string representation of a const context */
  const char *ctx_to_str (ConstGenericCtx ctx);

  /* Check if a const context allows default values */
  bool ctx_allows_default (ConstGenericCtx ctx);

  /**
   * Check that const generic parameters only contains defaults in allowed
   * contexts
   */
  void check_default_const_generics (
    std::vector<std::unique_ptr<GenericParam>> &param, ConstGenericCtx context);

  StackedContexts<HirId> const_context;
  Resolver::Resolver &resolver;
  Analysis::Mappings &mappings;

  virtual void visit (Lifetime &lifetime) override;
  virtual void visit (LifetimeParam &lifetime_param) override;
  virtual void visit (PathInExpression &path) override;
  virtual void visit (TypePathSegment &segment) override;
  virtual void visit (TypePathSegmentGeneric &segment) override;
  virtual void visit (TypePathSegmentFunction &segment) override;
  virtual void visit (TypePath &path) override;
  virtual void visit (QualifiedPathInExpression &path) override;
  virtual void visit (QualifiedPathInType &path) override;
  virtual void visit (LiteralExpr &expr) override;
  virtual void visit (BorrowExpr &expr) override;
  virtual void visit (DereferenceExpr &expr) override;
  virtual void visit (ErrorPropagationExpr &expr) override;
  virtual void visit (NegationExpr &expr) override;
  virtual void visit (ArithmeticOrLogicalExpr &expr) override;
  virtual void visit (ComparisonExpr &expr) override;
  virtual void visit (LazyBooleanExpr &expr) override;
  virtual void visit (TypeCastExpr &expr) override;
  virtual void visit (AssignmentExpr &expr) override;
  virtual void visit (CompoundAssignmentExpr &expr) override;
  virtual void visit (GroupedExpr &expr) override;
  virtual void visit (ArrayElemsValues &elems) override;
  virtual void visit (ArrayElemsCopied &elems) override;
  virtual void visit (ArrayExpr &expr) override;
  virtual void visit (ArrayIndexExpr &expr) override;
  virtual void visit (TupleExpr &expr) override;
  virtual void visit (TupleIndexExpr &expr) override;
  virtual void visit (StructExprStruct &expr) override;
  virtual void visit (StructExprFieldIdentifier &field) override;
  virtual void visit (StructExprFieldIdentifierValue &field) override;
  virtual void visit (StructExprFieldIndexValue &field) override;
  virtual void visit (StructExprStructFields &expr) override;
  virtual void visit (StructExprStructBase &expr) override;
  virtual void visit (CallExpr &expr) override;
  virtual void visit (MethodCallExpr &expr) override;
  virtual void visit (FieldAccessExpr &expr) override;
  virtual void visit (ClosureExpr &expr) override;
  virtual void visit (BlockExpr &expr) override;
  virtual void visit (ContinueExpr &expr) override;
  virtual void visit (BreakExpr &expr) override;
  virtual void visit (RangeFromToExpr &expr) override;
  virtual void visit (RangeFromExpr &expr) override;
  virtual void visit (RangeToExpr &expr) override;
  virtual void visit (RangeFullExpr &expr) override;
  virtual void visit (RangeFromToInclExpr &expr) override;
  virtual void visit (RangeToInclExpr &expr) override;
  virtual void visit (ReturnExpr &expr) override;
  virtual void visit (UnsafeBlockExpr &expr) override;
  virtual void visit (LoopExpr &expr) override;
  virtual void visit (WhileLoopExpr &expr) override;
  virtual void visit (WhileLetLoopExpr &expr) override;
  virtual void visit (ForLoopExpr &expr) override;
  virtual void visit (IfExpr &expr) override;
  virtual void visit (IfExprConseqElse &expr) override;
  virtual void visit (IfExprConseqIf &expr) override;
  virtual void visit (IfExprConseqIfLet &expr) override;
  virtual void visit (IfLetExpr &expr) override;
  virtual void visit (IfLetExprConseqElse &expr) override;
  virtual void visit (IfLetExprConseqIf &expr) override;
  virtual void visit (IfLetExprConseqIfLet &expr) override;
  virtual void visit (MatchExpr &expr) override;
  virtual void visit (AwaitExpr &expr) override;
  virtual void visit (AsyncBlockExpr &expr) override;
  virtual void visit (TypeParam &param) override;
  virtual void visit (ConstGenericParam &param) override;
  virtual void visit (LifetimeWhereClauseItem &item) override;
  virtual void visit (TypeBoundWhereClauseItem &item) override;
  virtual void visit (Module &module) override;
  virtual void visit (ExternCrate &crate) override;
  virtual void visit (UseTreeGlob &use_tree) override;
  virtual void visit (UseTreeList &use_tree) override;
  virtual void visit (UseTreeRebind &use_tree) override;
  virtual void visit (UseDeclaration &use_decl) override;
  virtual void visit (Function &function) override;
  virtual void visit (TypeAlias &type_alias) override;
  virtual void visit (StructStruct &struct_item) override;
  virtual void visit (TupleStruct &tuple_struct) override;
  virtual void visit (EnumItem &item) override;
  virtual void visit (EnumItemTuple &item) override;
  virtual void visit (EnumItemStruct &item) override;
  virtual void visit (EnumItemDiscriminant &item) override;
  virtual void visit (Enum &enum_item) override;
  virtual void visit (Union &union_item) override;
  virtual void visit (ConstantItem &const_item) override;
  virtual void visit (StaticItem &static_item) override;
  virtual void visit (TraitItemFunc &item) override;
  virtual void visit (TraitItemConst &item) override;
  virtual void visit (TraitItemType &item) override;
  virtual void visit (Trait &trait) override;
  virtual void visit (ImplBlock &impl) override;
  virtual void visit (ExternalStaticItem &item) override;
  virtual void visit (ExternalFunctionItem &item) override;
  virtual void visit (ExternBlock &block) override;
  virtual void visit (LiteralPattern &pattern) override;
  virtual void visit (IdentifierPattern &pattern) override;
  virtual void visit (WildcardPattern &pattern) override;
  virtual void visit (RangePatternBoundLiteral &bound) override;
  virtual void visit (RangePatternBoundPath &bound) override;
  virtual void visit (RangePatternBoundQualPath &bound) override;
  virtual void visit (RangePattern &pattern) override;
  virtual void visit (ReferencePattern &pattern) override;
  virtual void visit (StructPatternFieldTuplePat &field) override;
  virtual void visit (StructPatternFieldIdentPat &field) override;
  virtual void visit (StructPatternFieldIdent &field) override;
  virtual void visit (StructPattern &pattern) override;
  virtual void visit (TupleStructItemsNoRange &tuple_items) override;
  virtual void visit (TupleStructItemsRange &tuple_items) override;
  virtual void visit (TupleStructPattern &pattern) override;
  virtual void visit (TuplePatternItemsMultiple &tuple_items) override;
  virtual void visit (TuplePatternItemsRanged &tuple_items) override;
  virtual void visit (TuplePattern &pattern) override;
  virtual void visit (SlicePattern &pattern) override;
  virtual void visit (EmptyStmt &stmt) override;
  virtual void visit (LetStmt &stmt) override;
  virtual void visit (ExprStmtWithoutBlock &stmt) override;
  virtual void visit (ExprStmtWithBlock &stmt) override;
  virtual void visit (TraitBound &bound) override;
  virtual void visit (ImplTraitType &type) override;
  virtual void visit (TraitObjectType &type) override;
  virtual void visit (ParenthesisedType &type) override;
  virtual void visit (ImplTraitTypeOneBound &type) override;
  virtual void visit (TupleType &type) override;
  virtual void visit (NeverType &type) override;
  virtual void visit (RawPointerType &type) override;
  virtual void visit (ReferenceType &type) override;
  virtual void visit (ArrayType &type) override;
  virtual void visit (SliceType &type) override;
  virtual void visit (InferredType &type) override;
  virtual void visit (BareFunctionType &type) override;
};

} // namespace HIR
} // namespace Rust

#endif /* !RUST_CONST_CHECKER_H */
