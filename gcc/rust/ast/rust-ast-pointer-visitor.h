// Copyright (C) 2025 Free Software Foundation, Inc.

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

#ifndef RUST_AST_POINTER_VISITOR_H
#define RUST_AST_POINTER_VISITOR_H

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-item.h"

namespace Rust {
namespace AST {

/**
 * Regular AST visitor which may reseat pointers when necessary.
 */
class PointerVisitor : public DefaultASTVisitor
{
public:
  using DefaultASTVisitor::visit;

  virtual void reseat (std::unique_ptr<AST::Expr> &ptr) { visit (ptr); }
  virtual void reseat (std::unique_ptr<AST::BlockExpr> &ptr) { visit (ptr); }

  virtual void reseat (std::unique_ptr<AST::Stmt> &ptr) { visit (ptr); }

  virtual void reseat (std::unique_ptr<AST::Item> &ptr) { visit (ptr); }

  virtual void reseat (std::unique_ptr<AST::AssociatedItem> &ptr)
  {
    visit (ptr);
  }

  virtual void reseat (std::unique_ptr<AST::ExternalItem> &ptr) { visit (ptr); }

  virtual void reseat (std::unique_ptr<AST::Type> &ptr) { visit (ptr); }

  virtual void reseat (std::unique_ptr<AST::TypeNoBounds> &ptr) { visit (ptr); }

  virtual void reseat (std::unique_ptr<AST::Pattern> &ptr) { visit (ptr); }

  void visit (AST::Crate &crate) override;
  void visit (AST::AttrInputMetaItemContainer &input) override;
  void visit (AST::IdentifierExpr &ident_expr) override;
  void visit (AST::LifetimeParam &lifetime_param) override;
  void visit (AST::ConstGenericParam &const_param) override;
  void visit (AST::PathInExpression &path) override;
  void visit (GenericArgsBinding &binding) override;
  void visit (AST::TypePathSegmentGeneric &segment) override;
  void visit (AST::TypePathFunction &tpf) override;
  void visit (AST::TypePathSegmentFunction &segment) override;
  void visit (AST::GenericArgs &args) override;
  void visit (AST::PathExprSegment &segment) override;
  void visit (AST::TypePath &path) override;
  void visit (AST::QualifiedPathInExpression &path) override;
  void visit (AST::QualifiedPathType &path) override;
  void visit (AST::QualifiedPathInType &path) override;
  void visit (AST::LiteralExpr &expr) override;
  void visit (AST::AttrInputLiteral &attr_input) override;
  void visit (AST::AttrInputMacro &attr_input) override;
  void visit (AST::MetaItemLitExpr &meta_item) override;
  void visit (AST::SimplePath &path) override;
  void visit (AST::MetaItemPathExpr &meta_item) override;
  void visit (AST::BorrowExpr &expr) override;
  void visit (AST::DereferenceExpr &expr) override;
  void visit (AST::ErrorPropagationExpr &expr) override;
  void visit (AST::NegationExpr &expr) override;
  void visit (AST::ArithmeticOrLogicalExpr &expr) override;
  void visit (AST::ComparisonExpr &expr) override;
  void visit (AST::LazyBooleanExpr &expr) override;
  void visit (AST::TypeCastExpr &expr) override;
  void visit (AST::AssignmentExpr &expr) override;
  void visit (AST::CompoundAssignmentExpr &expr) override;
  void visit (AST::GroupedExpr &expr) override;
  void visit (AST::ArrayElemsValues &elems) override;
  void visit (AST::ArrayElemsCopied &elems) override;
  void visit (AST::ArrayExpr &expr) override;
  void visit (AST::ArrayIndexExpr &expr) override;
  void visit (AST::TupleExpr &expr) override;
  void visit (AST::TupleIndexExpr &expr) override;
  void visit (AST::StructExprStruct &expr) override;
  void visit (AST::StructExprFieldIdentifier &field) override;
  void visit (AST::StructExprFieldIdentifierValue &field) override;
  void visit (AST::StructExprFieldIndexValue &field) override;
  void visit (AST::StructBase &base) override;
  void visit (AST::StructExprStructFields &expr) override;
  void visit (AST::StructExprStructBase &expr) override;
  void visit (AST::CallExpr &expr) override;
  void visit (AST::MethodCallExpr &expr) override;
  void visit (AST::FieldAccessExpr &expr) override;
  void visit (AST::ClosureExprInner &expr) override;
  void visit (AST::BlockExpr &expr) override;
  void visit (AST::ConstBlock &expr) override;
  void visit (AST::AnonConst &expr) override;
  void visit (AST::ClosureExprInnerTyped &expr) override;
  void visit (AST::ClosureParam &param) override;
  void visit (AST::ContinueExpr &expr) override;
  void visit (AST::BreakExpr &expr) override;
  void visit (AST::RangeFromToExpr &expr) override;
  void visit (AST::RangeFromExpr &expr) override;
  void visit (AST::RangeToExpr &expr) override;
  void visit (AST::RangeFullExpr &expr) override;
  void visit (AST::RangeFromToInclExpr &expr) override;
  void visit (AST::RangeToInclExpr &expr) override;
  void visit (AST::ReturnExpr &expr) override;
  void visit (AST::TryExpr &expr) override;
  void visit (AST::BoxExpr &expr) override;
  void visit (AST::UnsafeBlockExpr &expr) override;
  void visit (AST::LoopLabel &label) override;
  void visit (AST::LoopExpr &expr) override;
  void visit (AST::WhileLoopExpr &expr) override;
  void visit (AST::WhileLetLoopExpr &expr) override;
  void visit (AST::ForLoopExpr &expr) override;
  void visit (AST::IfExpr &expr) override;
  void visit (AST::IfExprConseqElse &expr) override;
  void visit (AST::IfLetExpr &expr) override;
  void visit (AST::IfLetExprConseqElse &expr) override;
  void visit (AST::MatchArm &arm) override;
  void visit (AST::MatchCase &arm) override;
  void visit (AST::MatchExpr &expr) override;
  void visit (AST::AwaitExpr &expr) override;
  void visit (AST::AsyncBlockExpr &expr) override;
  void visit (AST::InlineAsm &expr) override;
  void visit (AST::LlvmInlineAsm &expr) override;
  void visit (AST::TypeParam &param) override;
  void visit (AST::LifetimeWhereClauseItem &item) override;
  void visit (AST::TypeBoundWhereClauseItem &item) override;
  void visit (AST::Visibility &vis) override;
  void visit (AST::WhereClause &where) override;
  void visit (AST::FunctionParam &param) override;
  void visit (AST::SelfParam &param) override;
  void visit (AST::Module &module) override;
  void visit (AST::ExternCrate &crate) override;
  void visit (AST::UseTreeGlob &use_tree) override;
  void visit (AST::UseTreeList &use_tree) override;
  void visit (AST::UseTreeRebind &use_tree) override;
  void visit (AST::UseDeclaration &use_decl) override;
  void visit_function_params (AST::Function &function) override;
  void visit (AST::Function &function) override;
  void visit (AST::TypeAlias &type_alias) override;
  void visit (AST::StructField &field) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TupleField &field) override;
  void visit (AST::TupleStruct &tuple_struct) override;
  void visit (AST::EnumItem &item) override;
  void visit (AST::EnumItemTuple &item) override;
  void visit (AST::EnumItemStruct &item) override;
  void visit (AST::EnumItemDiscriminant &item) override;
  void visit (AST::Enum &enum_item) override;
  void visit (AST::Union &union_item) override;
  void visit (AST::ConstantItem &const_item) override;
  void visit (AST::StaticItem &static_item) override;
  void visit (AST::TraitItemType &item) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::InherentImpl &impl) override;
  void visit (AST::TraitImpl &impl) override;
  void visit (AST::ExternalTypeItem &item) override;
  void visit (AST::ExternalStaticItem &item) override;
  void visit (AST::ExternBlock &block) override;
  void visit (AST::MacroMatchFragment &match) override;
  void visit (AST::MacroMatchRepetition &match) override;
  void visit (AST::MacroMatcher &matcher) override;
  void visit (AST::MacroTranscriber &transcriber) override;
  void visit (AST::MacroRule &rule) override;
  void visit (AST::MacroRulesDefinition &rules_def) override;
  void visit (AST::MacroInvocData &data) override;
  void visit (AST::MacroInvocation &macro_invoc) override;
  void visit (AST::MetaItemPath &meta_item) override;
  void visit (AST::MetaItemSeq &meta_item) override;
  void visit (AST::MetaListPaths &meta_item) override;
  void visit (AST::MetaListNameValueStr &meta_item) override;
  void visit (AST::IdentifierPattern &pattern) override;
  void visit (AST::RangePatternBoundPath &bound) override;
  void visit (AST::RangePatternBoundQualPath &bound) override;
  void visit (AST::RangePattern &pattern) override;
  void visit (AST::ReferencePattern &pattern) override;
  void visit (AST::StructPatternFieldTuplePat &field) override;
  void visit (AST::StructPatternFieldIdentPat &field) override;
  void visit (AST::StructPatternFieldIdent &field) override;
  void visit (AST::StructPatternElements &spe) override;
  void visit (AST::StructPattern &pattern) override;
  void visit (AST::TupleStructItemsNoRest &tuple_items) override;
  void visit (AST::TupleStructItemsHasRest &tuple_items) override;
  void visit (AST::TupleStructPattern &pattern) override;
  void visit (AST::TuplePatternItemsNoRest &tuple_items) override;
  void visit (AST::TuplePatternItemsHasRest &tuple_items) override;
  void visit (AST::TuplePattern &pattern) override;
  void visit (AST::GroupedPattern &pattern) override;
  void visit (AST::SlicePatternItemsNoRest &items) override;
  void visit (AST::SlicePatternItemsHasRest &items) override;
  void visit (AST::SlicePattern &pattern) override;
  void visit (AST::AltPattern &pattern) override;
  void visit (AST::EmptyStmt &stmt) override;
  void visit (AST::LetStmt &stmt) override;
  void visit (AST::ExprStmt &stmt) override;
  void visit (AST::TraitBound &bound) override;
  void visit (AST::ImplTraitType &type) override;
  void visit (AST::TraitObjectType &type) override;
  void visit (AST::ParenthesisedType &type) override;
  void visit (AST::ImplTraitTypeOneBound &type) override;
  void visit (AST::TraitObjectTypeOneBound &type) override;
  void visit (AST::TupleType &type) override;
  void visit (AST::NeverType &type) override;
  void visit (AST::RawPointerType &type) override;
  void visit (AST::ReferenceType &type) override;
  void visit (AST::ArrayType &type) override;
  void visit (AST::SliceType &type) override;
  void visit (AST::InferredType &type) override;
  void visit (AST::MaybeNamedParam &param) override;
  void visit (AST::BareFunctionType &type) override;
  void visit (AST::FormatArgs &) override;
  void visit (AST::OffsetOf &offset_of) override;
  void visit (AST::VariadicParam &param) override;
};

} // namespace AST
} // namespace Rust

#endif
