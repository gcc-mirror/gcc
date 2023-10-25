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

#ifndef RUST_AST_VISITOR_H
#define RUST_AST_VISITOR_H
// Visitor base for AST

// full include not required - only forward decls
#include "rust-ast-full-decls.h"
#include "rust-ast.h"
#include "rust-system.h"

namespace Rust {
namespace AST {
/* Pure abstract class that provides an interface for accessing different
 * classes of the AST. */
class ASTVisitor
{
public:
  // only concrete class overloads are required

  // rust-ast.h
  // virtual void visit(AttrInput& attr_input) = 0;
  // virtual void visit(TokenTree& token_tree) = 0;
  // virtual void visit(MacroMatch& macro_match) = 0;
  virtual void visit (Token &tok) = 0;
  virtual void visit (DelimTokenTree &delim_tok_tree) = 0;
  virtual void visit (AttrInputMetaItemContainer &input) = 0;
  // virtual void visit(MetaItem& meta_item) = 0;
  // virtual void visit(Stmt& stmt) = 0;
  // virtual void visit(Expr& expr) = 0;
  virtual void visit (IdentifierExpr &ident_expr) = 0;
  // virtual void visit(Pattern& pattern) = 0;
  // virtual void visit(Type& type) = 0;
  // virtual void visit(TypeParamBound& type_param_bound) = 0;
  virtual void visit (Lifetime &lifetime) = 0;
  // virtual void visit(GenericParam& generic_param) = 0;
  virtual void visit (LifetimeParam &lifetime_param) = 0;
  virtual void visit (ConstGenericParam &const_param) = 0;
  // virtual void visit(TraitItem& trait_item) = 0;
  // virtual void visit(InherentImplItem& inherent_impl_item) = 0;
  // virtual void visit(TraitImplItem& trait_impl_item) = 0;

  // rust-path.h
  virtual void visit (PathInExpression &path) = 0;
  virtual void visit (TypePathSegment &segment) = 0;
  virtual void visit (TypePathSegmentGeneric &segment) = 0;
  virtual void visit (TypePathSegmentFunction &segment) = 0;
  virtual void visit (TypePath &path) = 0;
  virtual void visit (QualifiedPathInExpression &path) = 0;
  virtual void visit (QualifiedPathInType &path) = 0;

  // rust-expr.h
  virtual void visit (LiteralExpr &expr) = 0;
  virtual void visit (AttrInputLiteral &attr_input) = 0;
  virtual void visit (AttrInputMacro &attr_input) = 0;
  virtual void visit (MetaItemLitExpr &meta_item) = 0;
  virtual void visit (MetaItemPathLit &meta_item) = 0;
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
  // virtual void visit(ArrayElems& elems) = 0;
  virtual void visit (ArrayElemsValues &elems) = 0;
  virtual void visit (ArrayElemsCopied &elems) = 0;
  virtual void visit (ArrayExpr &expr) = 0;
  virtual void visit (ArrayIndexExpr &expr) = 0;
  virtual void visit (TupleExpr &expr) = 0;
  virtual void visit (TupleIndexExpr &expr) = 0;
  virtual void visit (StructExprStruct &expr) = 0;
  // virtual void visit(StructExprField& field) = 0;
  virtual void visit (StructExprFieldIdentifier &field) = 0;
  virtual void visit (StructExprFieldIdentifierValue &field) = 0;
  virtual void visit (StructExprFieldIndexValue &field) = 0;
  virtual void visit (StructExprStructFields &expr) = 0;
  virtual void visit (StructExprStructBase &expr) = 0;
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
  virtual void visit (IfLetExpr &expr) = 0;
  virtual void visit (IfLetExprConseqElse &expr) = 0;
  // virtual void visit(MatchCase& match_case) = 0;
  // virtual void visit (MatchCaseBlockExpr &match_case) = 0;
  // virtual void visit (MatchCaseExpr &match_case) = 0;
  virtual void visit (MatchExpr &expr) = 0;
  virtual void visit (AwaitExpr &expr) = 0;
  virtual void visit (AsyncBlockExpr &expr) = 0;

  // rust-item.h
  virtual void visit (TypeParam &param) = 0;
  // virtual void visit(WhereClauseItem& item) = 0;
  virtual void visit (LifetimeWhereClauseItem &item) = 0;
  virtual void visit (TypeBoundWhereClauseItem &item) = 0;
  virtual void visit (Method &method) = 0;
  virtual void visit (Module &module) = 0;
  virtual void visit (ExternCrate &crate) = 0;
  // virtual void visit(UseTree& use_tree) = 0;
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
  virtual void visit (InherentImpl &impl) = 0;
  virtual void visit (TraitImpl &impl) = 0;
  // virtual void visit(ExternalItem& item) = 0;
  virtual void visit (ExternalTypeItem &type) = 0;
  virtual void visit (ExternalStaticItem &item) = 0;
  virtual void visit (ExternalFunctionItem &item) = 0;
  virtual void visit (ExternBlock &block) = 0;

  // rust-macro.h
  virtual void visit (MacroMatchFragment &match) = 0;
  virtual void visit (MacroMatchRepetition &match) = 0;
  virtual void visit (MacroMatcher &matcher) = 0;
  virtual void visit (MacroRulesDefinition &rules_def) = 0;
  virtual void visit (MacroInvocation &macro_invoc) = 0;
  virtual void visit (MetaItemPath &meta_item) = 0;
  virtual void visit (MetaItemSeq &meta_item) = 0;
  virtual void visit (MetaWord &meta_item) = 0;
  virtual void visit (MetaNameValueStr &meta_item) = 0;
  virtual void visit (MetaListPaths &meta_item) = 0;
  virtual void visit (MetaListNameValueStr &meta_item) = 0;

  // rust-pattern.h
  virtual void visit (LiteralPattern &pattern) = 0;
  virtual void visit (IdentifierPattern &pattern) = 0;
  virtual void visit (WildcardPattern &pattern) = 0;
  virtual void visit (RestPattern &pattern) = 0;
  // virtual void visit(RangePatternBound& bound) = 0;
  virtual void visit (RangePatternBoundLiteral &bound) = 0;
  virtual void visit (RangePatternBoundPath &bound) = 0;
  virtual void visit (RangePatternBoundQualPath &bound) = 0;
  virtual void visit (RangePattern &pattern) = 0;
  virtual void visit (ReferencePattern &pattern) = 0;
  // virtual void visit(StructPatternField& field) = 0;
  virtual void visit (StructPatternFieldTuplePat &field) = 0;
  virtual void visit (StructPatternFieldIdentPat &field) = 0;
  virtual void visit (StructPatternFieldIdent &field) = 0;
  virtual void visit (StructPattern &pattern) = 0;
  // virtual void visit(TupleStructItems& tuple_items) = 0;
  virtual void visit (TupleStructItemsNoRange &tuple_items) = 0;
  virtual void visit (TupleStructItemsRange &tuple_items) = 0;
  virtual void visit (TupleStructPattern &pattern) = 0;
  // virtual void visit(TuplePatternItems& tuple_items) = 0;
  virtual void visit (TuplePatternItemsMultiple &tuple_items) = 0;
  virtual void visit (TuplePatternItemsRanged &tuple_items) = 0;
  virtual void visit (TuplePattern &pattern) = 0;
  virtual void visit (GroupedPattern &pattern) = 0;
  virtual void visit (SlicePattern &pattern) = 0;
  virtual void visit (AltPattern &pattern) = 0;

  // rust-stmt.h
  virtual void visit (EmptyStmt &stmt) = 0;
  virtual void visit (LetStmt &stmt) = 0;
  virtual void visit (ExprStmt &stmt) = 0;

  // rust-type.h
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

  // TODO: rust-cond-compilation.h visiting? not currently used
};

class DefaultASTVisitor : public ASTVisitor
{
  void visit (AST::Crate &crate);

  void visit (AST::Token &tok) override;
  void visit (AST::DelimTokenTree &delim_tok_tree) override;
  void visit (AST::AttrInputMetaItemContainer &input) override;
  void visit (AST::IdentifierExpr &ident_expr) override;
  void visit (AST::Lifetime &lifetime) override;
  void visit (AST::LifetimeParam &lifetime_param) override;
  void visit (AST::ConstGenericParam &const_param) override;
  void visit (AST::PathInExpression &path) override;
  void visit (AST::TypePathSegment &segment) override;
  void visit (AST::TypePathSegmentGeneric &segment) override;
  void visit (AST::TypePathSegmentFunction &segment) override;
  void visit (AST::TypePath &path) override;
  void visit (AST::QualifiedPathInExpression &path) override;
  void visit (AST::QualifiedPathInType &path) override;
  void visit (AST::LiteralExpr &expr) override;
  void visit (AST::AttrInputLiteral &attr_input) override;
  void visit (AST::AttrInputMacro &attr_input) override;
  void visit (AST::MetaItemLitExpr &meta_item) override;
  void visit (AST::MetaItemPathLit &meta_item) override;
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
  void visit (AST::StructExprStructFields &expr) override;
  void visit (AST::StructExprStructBase &expr) override;
  void visit (AST::CallExpr &expr) override;
  void visit (AST::MethodCallExpr &expr) override;
  void visit (AST::FieldAccessExpr &expr) override;
  void visit (AST::ClosureExprInner &expr) override;
  void visit (AST::BlockExpr &expr) override;
  void visit (AST::ClosureExprInnerTyped &expr) override;
  void visit (AST::ContinueExpr &expr) override;
  void visit (AST::BreakExpr &expr) override;
  void visit (AST::RangeFromToExpr &expr) override;
  void visit (AST::RangeFromExpr &expr) override;
  void visit (AST::RangeToExpr &expr) override;
  void visit (AST::RangeFullExpr &expr) override;
  void visit (AST::RangeFromToInclExpr &expr) override;
  void visit (AST::RangeToInclExpr &expr) override;
  void visit (AST::ReturnExpr &expr) override;
  void visit (AST::UnsafeBlockExpr &expr) override;
  void visit (AST::LoopExpr &expr) override;
  void visit (AST::WhileLoopExpr &expr) override;
  void visit (AST::WhileLetLoopExpr &expr) override;
  void visit (AST::ForLoopExpr &expr) override;
  void visit (AST::IfExpr &expr) override;
  void visit (AST::IfExprConseqElse &expr) override;
  void visit (AST::IfLetExpr &expr) override;
  void visit (AST::IfLetExprConseqElse &expr) override;
  void visit (AST::MatchExpr &expr) override;
  void visit (AST::AwaitExpr &expr) override;
  void visit (AST::AsyncBlockExpr &expr) override;
  void visit (AST::TypeParam &param) override;
  void visit (AST::LifetimeWhereClauseItem &item) override;
  void visit (AST::TypeBoundWhereClauseItem &item) override;
  void visit (AST::Method &method) override;
  void visit (AST::Module &module) override;
  void visit (AST::ExternCrate &crate) override;
  void visit (AST::UseTreeGlob &use_tree) override;
  void visit (AST::UseTreeList &use_tree) override;
  void visit (AST::UseTreeRebind &use_tree) override;
  void visit (AST::UseDeclaration &use_decl) override;
  void visit (AST::Function &function) override;
  void visit (AST::TypeAlias &type_alias) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TupleStruct &tuple_struct) override;
  void visit (AST::EnumItem &item) override;
  void visit (AST::EnumItemTuple &item) override;
  void visit (AST::EnumItemStruct &item) override;
  void visit (AST::EnumItemDiscriminant &item) override;
  void visit (AST::Enum &enum_item) override;
  void visit (AST::Union &union_item) override;
  void visit (AST::ConstantItem &const_item) override;
  void visit (AST::StaticItem &static_item) override;
  void visit (AST::TraitItemFunc &item) override;
  void visit (AST::TraitItemMethod &item) override;
  void visit (AST::TraitItemConst &item) override;
  void visit (AST::TraitItemType &item) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::InherentImpl &impl) override;
  void visit (AST::TraitImpl &impl) override;
  void visit (AST::ExternalTypeItem &item) override;
  void visit (AST::ExternalStaticItem &item) override;
  void visit (AST::ExternalFunctionItem &item) override;
  void visit (AST::ExternBlock &block) override;
  void visit (AST::MacroMatchFragment &match) override;
  void visit (AST::MacroMatchRepetition &match) override;
  void visit (AST::MacroMatcher &matcher) override;
  void visit (AST::MacroRulesDefinition &rules_def) override;
  void visit (AST::MacroInvocation &macro_invoc) override;
  void visit (AST::MetaItemPath &meta_item) override;
  void visit (AST::MetaItemSeq &meta_item) override;
  void visit (AST::MetaWord &meta_item) override;
  void visit (AST::MetaNameValueStr &meta_item) override;
  void visit (AST::MetaListPaths &meta_item) override;
  void visit (AST::MetaListNameValueStr &meta_item) override;
  void visit (AST::LiteralPattern &pattern) override;
  void visit (AST::IdentifierPattern &pattern) override;
  void visit (AST::WildcardPattern &pattern) override;
  void visit (AST::RestPattern &pattern) override;
  void visit (AST::RangePatternBoundLiteral &bound) override;
  void visit (AST::RangePatternBoundPath &bound) override;
  void visit (AST::RangePatternBoundQualPath &bound) override;
  void visit (AST::RangePattern &pattern) override;
  void visit (AST::ReferencePattern &pattern) override;
  void visit (AST::StructPatternFieldTuplePat &field) override;
  void visit (AST::StructPatternFieldIdentPat &field) override;
  void visit (AST::StructPatternFieldIdent &field) override;
  void visit (AST::StructPattern &pattern) override;
  void visit (AST::TupleStructItemsNoRange &tuple_items) override;
  void visit (AST::TupleStructItemsRange &tuple_items) override;
  void visit (AST::TupleStructPattern &pattern) override;
  void visit (AST::TuplePatternItemsMultiple &tuple_items) override;
  void visit (AST::TuplePatternItemsRanged &tuple_items) override;
  void visit (AST::TuplePattern &pattern) override;
  void visit (AST::GroupedPattern &pattern) override;
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
  void visit (AST::BareFunctionType &type) override;

  template <typename T> void visit (T &node);

  template <typename T> void visit (std::unique_ptr<T> &node);

private:
  void visit (AST::GenericArgsBinding &binding);
  void visit (AST::PathExprSegment &segment);
  void visit (AST::GenericArgs &args);
  void visit (AST::QualifiedPathType &path);
  void visit (AST::TypePathFunction &tpf);
  void visit (AST::PathIdentSegment &segment);
  void visit (AST::SimplePath &path);
  void visit (AST::SimplePathSegment &segment);
  void visit (AST::StructBase &base);
  void visit (AST::ClosureParam &param);
  void visit (AST::LoopLabel &label);
  void visit (AST::MatchCase &arm);
  void visit (AST::MatchArm &arm);
  void visit (AST::Visibility &vis);
  void visit (AST::FunctionQualifiers &qualifiers);
  void visit (AST::SelfParam &self);
  void visit (AST::WhereClause &where);
  void visit (AST::FunctionParam &param);
  void visit (AST::StructField &field);
  void visit (AST::TupleField &field);
  void visit (AST::TraitFunctionDecl &decl);
  void visit (AST::TraitMethodDecl &decl);
  void visit (AST::NamedFunctionParam &param);
  void visit (AST::MacroRule &rule);
  void visit (AST::MacroInvocData &data);
  void visit (AST::MacroTranscriber &transcriber);
  void visit (AST::StructPatternElements &spe);
  void visit (AST::MaybeNamedParam &param);

  template <typename T> void visit_outer_attrs (T &node)
  {
    for (auto &attr : node.get_outer_attrs ())
      visit (attr);
  }

  template <typename T> void visit_inner_attrs (T &node)
  {
    for (auto &attr : node.get_inner_attrs ())
      visit (attr);
  }

  void visit (AST::Attribute &attribute) {}
};

} // namespace AST
} // namespace Rust

#endif
