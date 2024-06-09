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

#ifndef DERIVE_VISITOR_H
#define DERIVE_VISITOR_H

#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-ast-builder.h"
#include "rust-macro-builtins.h"

namespace Rust {
namespace AST {

/**
 * The goal of this class is to accumulate and create the required items from a
 * builtin `#[derive]` macro applied on a struct, enum or union.
 */
class DeriveVisitor : public AST::ASTVisitor
{
public:
  static std::unique_ptr<Item> derive (Item &item, const Attribute &derive,
				       BuiltinMacro to_derive);

protected:
  DeriveVisitor (location_t loc);

  location_t loc;
  AstBuilder builder;

private:
  // the 4 "allowed" visitors, which a derive-visitor can specify and override
  virtual void visit_struct (StructStruct &struct_item) = 0;
  virtual void visit_tuple (TupleStruct &tuple_item) = 0;
  virtual void visit_enum (Enum &enum_item) = 0;
  virtual void visit_union (Union &enum_item) = 0;

  // all visitors are final, so no deriving class can implement `derive` for
  // anything other than structs, tuples, enums and unions

  virtual void visit (StructStruct &struct_item) override final
  {
    visit_struct (struct_item);
  }

  virtual void visit (TupleStruct &tuple_struct) override final
  {
    visit_tuple (tuple_struct);
  }

  virtual void visit (Enum &enum_item) override final
  {
    visit_enum (enum_item);
  }

  virtual void visit (Union &union_item) override final
  {
    visit_union (union_item);
  }

  virtual void visit (Token &tok) override final{};
  virtual void visit (DelimTokenTree &delim_tok_tree) override final{};
  virtual void visit (AttrInputMetaItemContainer &input) override final{};
  virtual void visit (AttrInputMacro &expr) override final{};
  virtual void visit (IdentifierExpr &ident_expr) override final{};
  virtual void visit (Lifetime &lifetime) override final{};
  virtual void visit (LifetimeParam &lifetime_param) override final{};
  virtual void visit (ConstGenericParam &const_param) override final{};
  virtual void visit (PathInExpression &path) override final{};
  virtual void visit (TypePathSegment &segment) override final{};
  virtual void visit (TypePathSegmentGeneric &segment) override final{};
  virtual void visit (TypePathSegmentFunction &segment) override final{};
  virtual void visit (TypePath &path) override final{};
  virtual void visit (QualifiedPathInExpression &path) override final{};
  virtual void visit (QualifiedPathInType &path) override final{};
  virtual void visit (LiteralExpr &expr) override final{};
  virtual void visit (AttrInputLiteral &attr_input) override final{};
  virtual void visit (MetaItemLitExpr &meta_item) override final{};
  virtual void visit (MetaItemPathLit &meta_item) override final{};
  virtual void visit (BorrowExpr &expr) override final{};
  virtual void visit (DereferenceExpr &expr) override final{};
  virtual void visit (ErrorPropagationExpr &expr) override final{};
  virtual void visit (NegationExpr &expr) override final{};
  virtual void visit (ArithmeticOrLogicalExpr &expr) override final{};
  virtual void visit (ComparisonExpr &expr) override final{};
  virtual void visit (LazyBooleanExpr &expr) override final{};
  virtual void visit (TypeCastExpr &expr) override final{};
  virtual void visit (AssignmentExpr &expr) override final{};
  virtual void visit (CompoundAssignmentExpr &expr) override final{};
  virtual void visit (GroupedExpr &expr) override final{};
  virtual void visit (ArrayElemsValues &elems) override final{};
  virtual void visit (ArrayElemsCopied &elems) override final{};
  virtual void visit (ArrayExpr &expr) override final{};
  virtual void visit (ArrayIndexExpr &expr) override final{};
  virtual void visit (TupleExpr &expr) override final{};
  virtual void visit (TupleIndexExpr &expr) override final{};
  virtual void visit (StructExprStruct &expr) override final{};
  virtual void visit (StructExprFieldIdentifier &field) override final{};
  virtual void visit (StructExprFieldIdentifierValue &field) override final{};
  virtual void visit (StructExprFieldIndexValue &field) override final{};
  virtual void visit (StructExprStructFields &expr) override final{};
  virtual void visit (StructExprStructBase &expr) override final{};
  virtual void visit (CallExpr &expr) override final{};
  virtual void visit (MethodCallExpr &expr) override final{};
  virtual void visit (FieldAccessExpr &expr) override final{};
  virtual void visit (ClosureExprInner &expr) override final{};
  virtual void visit (BlockExpr &expr) override final{};
  virtual void visit (ClosureExprInnerTyped &expr) override final{};
  virtual void visit (ContinueExpr &expr) override final{};
  virtual void visit (BreakExpr &expr) override final{};
  virtual void visit (RangeFromToExpr &expr) override final{};
  virtual void visit (RangeFromExpr &expr) override final{};
  virtual void visit (RangeToExpr &expr) override final{};
  virtual void visit (RangeFullExpr &expr) override final{};
  virtual void visit (RangeFromToInclExpr &expr) override final{};
  virtual void visit (RangeToInclExpr &expr) override final{};
  virtual void visit (ReturnExpr &expr) override final{};
  virtual void visit (UnsafeBlockExpr &expr) override final{};
  virtual void visit (LoopExpr &expr) override final{};
  virtual void visit (WhileLoopExpr &expr) override final{};
  virtual void visit (WhileLetLoopExpr &expr) override final{};
  virtual void visit (ForLoopExpr &expr) override final{};
  virtual void visit (IfExpr &expr) override final{};
  virtual void visit (IfExprConseqElse &expr) override final{};
  virtual void visit (IfLetExpr &expr) override final{};
  virtual void visit (IfLetExprConseqElse &expr) override final{};
  virtual void visit (MatchExpr &expr) override final{};
  virtual void visit (AwaitExpr &expr) override final{};
  virtual void visit (AsyncBlockExpr &expr) override final{};
  virtual void visit (TypeParam &param) override final{};
  virtual void visit (LifetimeWhereClauseItem &item) override final{};
  virtual void visit (TypeBoundWhereClauseItem &item) override final{};
  virtual void visit (Module &module) override final{};
  virtual void visit (ExternCrate &crate) override final{};
  virtual void visit (UseTreeGlob &use_tree) override final{};
  virtual void visit (UseTreeList &use_tree) override final{};
  virtual void visit (UseTreeRebind &use_tree) override final{};
  virtual void visit (UseDeclaration &use_decl) override final{};
  virtual void visit (Function &function) override final{};
  virtual void visit (TypeAlias &type_alias) override final{};
  virtual void visit (EnumItem &item) override final{};
  virtual void visit (EnumItemTuple &item) override final{};
  virtual void visit (EnumItemStruct &item) override final{};
  virtual void visit (EnumItemDiscriminant &item) override final{};
  virtual void visit (ConstantItem &const_item) override final{};
  virtual void visit (StaticItem &static_item) override final{};
  virtual void visit (TraitItemConst &item) override final{};
  virtual void visit (TraitItemType &item) override final{};
  virtual void visit (Trait &trait) override final{};
  virtual void visit (InherentImpl &impl) override final{};
  virtual void visit (TraitImpl &impl) override final{};
  virtual void visit (ExternalTypeItem &type) override final{};
  virtual void visit (ExternalStaticItem &item) override final{};
  virtual void visit (ExternalFunctionItem &item) override final{};
  virtual void visit (ExternBlock &block) override final{};
  virtual void visit (MacroMatchFragment &match) override final{};
  virtual void visit (MacroMatchRepetition &match) override final{};
  virtual void visit (MacroMatcher &matcher) override final{};
  virtual void visit (MacroRulesDefinition &rules_def) override final{};
  virtual void visit (MacroInvocation &macro_invoc) override final{};
  virtual void visit (MetaItemPath &meta_item) override final{};
  virtual void visit (MetaItemSeq &meta_item) override final{};
  virtual void visit (MetaWord &meta_item) override final{};
  virtual void visit (MetaNameValueStr &meta_item) override final{};
  virtual void visit (MetaListPaths &meta_item) override final{};
  virtual void visit (MetaListNameValueStr &meta_item) override final{};
  virtual void visit (LiteralPattern &pattern) override final{};
  virtual void visit (IdentifierPattern &pattern) override final{};
  virtual void visit (WildcardPattern &pattern) override final{};
  virtual void visit (RestPattern &pattern) override final{};
  virtual void visit (RangePatternBoundLiteral &bound) override final{};
  virtual void visit (RangePatternBoundPath &bound) override final{};
  virtual void visit (RangePatternBoundQualPath &bound) override final{};
  virtual void visit (RangePattern &pattern) override final{};
  virtual void visit (ReferencePattern &pattern) override final{};
  virtual void visit (StructPatternFieldTuplePat &field) override final{};
  virtual void visit (StructPatternFieldIdentPat &field) override final{};
  virtual void visit (StructPatternFieldIdent &field) override final{};
  virtual void visit (StructPattern &pattern) override final{};
  virtual void visit (TupleStructItemsNoRange &tuple_items) override final{};
  virtual void visit (TupleStructItemsRange &tuple_items) override final{};
  virtual void visit (TupleStructPattern &pattern) override final{};
  virtual void visit (TuplePatternItemsMultiple &tuple_items) override final{};
  virtual void visit (TuplePatternItemsRanged &tuple_items) override final{};
  virtual void visit (TuplePattern &pattern) override final{};
  virtual void visit (GroupedPattern &pattern) override final{};
  virtual void visit (SlicePattern &pattern) override final{};
  virtual void visit (AltPattern &pattern) override final{};
  virtual void visit (EmptyStmt &stmt) override final{};
  virtual void visit (LetStmt &stmt) override final{};
  virtual void visit (ExprStmt &stmt) override final{};
  virtual void visit (TraitBound &bound) override final{};
  virtual void visit (ImplTraitType &type) override final{};
  virtual void visit (TraitObjectType &type) override final{};
  virtual void visit (ParenthesisedType &type) override final{};
  virtual void visit (ImplTraitTypeOneBound &type) override final{};
  virtual void visit (TraitObjectTypeOneBound &type) override final{};
  virtual void visit (TupleType &type) override final{};
  virtual void visit (NeverType &type) override final{};
  virtual void visit (RawPointerType &type) override final{};
  virtual void visit (ReferenceType &type) override final{};
  virtual void visit (ArrayType &type) override final{};
  virtual void visit (SliceType &type) override final{};
  virtual void visit (InferredType &type) override final{};
  virtual void visit (BareFunctionType &type) override final{};
  virtual void visit (SelfParam &param) override final{};
  virtual void visit (FunctionParam &param) override final{};
  virtual void visit (VariadicParam &param) override final{};
};

} // namespace AST
} // namespace Rust

#endif // DERIVE_VISITOR_H
