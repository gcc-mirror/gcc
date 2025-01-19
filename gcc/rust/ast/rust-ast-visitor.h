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

#ifndef RUST_AST_VISITOR_H
#define RUST_AST_VISITOR_H
// Visitor base for AST

// full include not required - only forward decls
#include "rust-ast-full-decls.h"
#include "rust-ast.h"
#include "rust-item.h"
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
  virtual void visit (SelfParam &param) = 0;
  virtual void visit (FunctionParam &param) = 0;
  virtual void visit (VariadicParam &param) = 0;

  // virtual void visit(WhereClauseItem& item) = 0;
  virtual void visit (LifetimeWhereClauseItem &item) = 0;
  virtual void visit (TypeBoundWhereClauseItem &item) = 0;
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
  virtual void visit (TraitItemConst &item) = 0;
  virtual void visit (TraitItemType &item) = 0;
  virtual void visit (Trait &trait) = 0;
  virtual void visit (InherentImpl &impl) = 0;
  virtual void visit (TraitImpl &impl) = 0;
  // virtual void visit(ExternalItem& item) = 0;
  virtual void visit (ExternalTypeItem &type) = 0;
  virtual void visit (ExternalStaticItem &item) = 0;
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

  // special AST nodes for certain builtin macros such as `asm!()`
  virtual void visit (FormatArgs &fmt) = 0;

  // TODO: rust-cond-compilation.h visiting? not currently used
};

class DefaultASTVisitor : public ASTVisitor
{
public:
  virtual void visit (AST::Crate &crate);

protected:
  virtual void visit (AST::Token &tok) override;
  virtual void visit (AST::DelimTokenTree &delim_tok_tree) override;
  virtual void visit (AST::AttrInputMetaItemContainer &input) override;
  virtual void visit (AST::IdentifierExpr &ident_expr) override;
  virtual void visit (AST::Lifetime &lifetime) override;
  virtual void visit (AST::LifetimeParam &lifetime_param) override;
  virtual void visit (AST::ConstGenericParam &const_param) override;
  virtual void visit (AST::PathInExpression &path) override;
  virtual void visit (AST::TypePathSegment &segment) override;
  virtual void visit (AST::TypePathSegmentGeneric &segment) override;
  virtual void visit (AST::TypePathSegmentFunction &segment) override;
  virtual void visit (AST::TypePath &path) override;
  virtual void visit (AST::QualifiedPathInExpression &path) override;
  virtual void visit (AST::QualifiedPathInType &path) override;
  virtual void visit (AST::LiteralExpr &expr) override;
  virtual void visit (AST::AttrInputLiteral &attr_input) override;
  virtual void visit (AST::AttrInputMacro &attr_input) override;
  virtual void visit (AST::MetaItemLitExpr &meta_item) override;
  virtual void visit (AST::MetaItemPathLit &meta_item) override;
  virtual void visit (AST::BorrowExpr &expr) override;
  virtual void visit (AST::DereferenceExpr &expr) override;
  virtual void visit (AST::ErrorPropagationExpr &expr) override;
  virtual void visit (AST::NegationExpr &expr) override;
  virtual void visit (AST::ArithmeticOrLogicalExpr &expr) override;
  virtual void visit (AST::ComparisonExpr &expr) override;
  virtual void visit (AST::LazyBooleanExpr &expr) override;
  virtual void visit (AST::TypeCastExpr &expr) override;
  virtual void visit (AST::AssignmentExpr &expr) override;
  virtual void visit (AST::CompoundAssignmentExpr &expr) override;
  virtual void visit (AST::GroupedExpr &expr) override;
  virtual void visit (AST::ArrayElemsValues &elems) override;
  virtual void visit (AST::ArrayElemsCopied &elems) override;
  virtual void visit (AST::ArrayExpr &expr) override;
  virtual void visit (AST::ArrayIndexExpr &expr) override;
  virtual void visit (AST::TupleExpr &expr) override;
  virtual void visit (AST::TupleIndexExpr &expr) override;
  virtual void visit (AST::StructExprStruct &expr) override;
  virtual void visit (AST::StructExprFieldIdentifier &field) override;
  virtual void visit (AST::StructExprFieldIdentifierValue &field) override;
  virtual void visit (AST::StructExprFieldIndexValue &field) override;
  virtual void visit (AST::StructExprStructFields &expr) override;
  virtual void visit (AST::StructExprStructBase &expr) override;
  virtual void visit (AST::CallExpr &expr) override;
  virtual void visit (AST::MethodCallExpr &expr) override;
  virtual void visit (AST::FieldAccessExpr &expr) override;
  virtual void visit (AST::ClosureExprInner &expr) override;
  virtual void visit (AST::BlockExpr &expr) override;
  virtual void visit (AST::ClosureExprInnerTyped &expr) override;
  virtual void visit (AST::ContinueExpr &expr) override;
  virtual void visit (AST::BreakExpr &expr) override;
  virtual void visit (AST::RangeFromToExpr &expr) override;
  virtual void visit (AST::RangeFromExpr &expr) override;
  virtual void visit (AST::RangeToExpr &expr) override;
  virtual void visit (AST::RangeFullExpr &expr) override;
  virtual void visit (AST::RangeFromToInclExpr &expr) override;
  virtual void visit (AST::RangeToInclExpr &expr) override;
  virtual void visit (AST::ReturnExpr &expr) override;
  virtual void visit (AST::UnsafeBlockExpr &expr) override;
  virtual void visit (AST::LoopExpr &expr) override;
  virtual void visit (AST::WhileLoopExpr &expr) override;
  virtual void visit (AST::WhileLetLoopExpr &expr) override;
  virtual void visit (AST::ForLoopExpr &expr) override;
  virtual void visit (AST::IfExpr &expr) override;
  virtual void visit (AST::IfExprConseqElse &expr) override;
  virtual void visit (AST::IfLetExpr &expr) override;
  virtual void visit (AST::IfLetExprConseqElse &expr) override;
  virtual void visit (AST::MatchExpr &expr) override;
  virtual void visit (AST::AwaitExpr &expr) override;
  virtual void visit (AST::AsyncBlockExpr &expr) override;
  virtual void visit (AST::TypeParam &param) override;
  virtual void visit (AST::LifetimeWhereClauseItem &item) override;
  virtual void visit (AST::TypeBoundWhereClauseItem &item) override;
  virtual void visit (AST::Module &module) override;
  virtual void visit (AST::ExternCrate &crate) override;
  virtual void visit (AST::UseTreeGlob &use_tree) override;
  virtual void visit (AST::UseTreeList &use_tree) override;
  virtual void visit (AST::UseTreeRebind &use_tree) override;
  virtual void visit (AST::UseDeclaration &use_decl) override;
  virtual void visit (AST::Function &function) override;
  virtual void visit (AST::TypeAlias &type_alias) override;
  virtual void visit (AST::StructStruct &struct_item) override;
  virtual void visit (AST::TupleStruct &tuple_struct) override;
  virtual void visit (AST::EnumItem &item) override;
  virtual void visit (AST::EnumItemTuple &item) override;
  virtual void visit (AST::EnumItemStruct &item) override;
  virtual void visit (AST::EnumItemDiscriminant &item) override;
  virtual void visit (AST::Enum &enum_item) override;
  virtual void visit (AST::Union &union_item) override;
  virtual void visit (AST::ConstantItem &const_item) override;
  virtual void visit (AST::StaticItem &static_item) override;
  virtual void visit (AST::TraitItemConst &item) override;
  virtual void visit (AST::TraitItemType &item) override;
  virtual void visit (AST::Trait &trait) override;
  virtual void visit (AST::InherentImpl &impl) override;
  virtual void visit (AST::TraitImpl &impl) override;
  virtual void visit (AST::ExternalTypeItem &item) override;
  virtual void visit (AST::ExternalStaticItem &item) override;
  virtual void visit (AST::ExternBlock &block) override;
  virtual void visit (AST::MacroMatchFragment &match) override;
  virtual void visit (AST::MacroMatchRepetition &match) override;
  virtual void visit (AST::MacroMatcher &matcher) override;
  virtual void visit (AST::MacroRulesDefinition &rules_def) override;
  virtual void visit (AST::MacroInvocation &macro_invoc) override;
  virtual void visit (AST::MetaItemPath &meta_item) override;
  virtual void visit (AST::MetaItemSeq &meta_item) override;
  virtual void visit (AST::MetaWord &meta_item) override;
  virtual void visit (AST::MetaNameValueStr &meta_item) override;
  virtual void visit (AST::MetaListPaths &meta_item) override;
  virtual void visit (AST::MetaListNameValueStr &meta_item) override;
  virtual void visit (AST::LiteralPattern &pattern) override;
  virtual void visit (AST::IdentifierPattern &pattern) override;
  virtual void visit (AST::WildcardPattern &pattern) override;
  virtual void visit (AST::RestPattern &pattern) override;
  virtual void visit (AST::RangePatternBoundLiteral &bound) override;
  virtual void visit (AST::RangePatternBoundPath &bound) override;
  virtual void visit (AST::RangePatternBoundQualPath &bound) override;
  virtual void visit (AST::RangePattern &pattern) override;
  virtual void visit (AST::ReferencePattern &pattern) override;
  virtual void visit (AST::StructPatternFieldTuplePat &field) override;
  virtual void visit (AST::StructPatternFieldIdentPat &field) override;
  virtual void visit (AST::StructPatternFieldIdent &field) override;
  virtual void visit (AST::StructPattern &pattern) override;
  virtual void visit (AST::TupleStructItemsNoRange &tuple_items) override;
  virtual void visit (AST::TupleStructItemsRange &tuple_items) override;
  virtual void visit (AST::TupleStructPattern &pattern) override;
  virtual void visit (AST::TuplePatternItemsMultiple &tuple_items) override;
  virtual void visit (AST::TuplePatternItemsRanged &tuple_items) override;
  virtual void visit (AST::TuplePattern &pattern) override;
  virtual void visit (AST::GroupedPattern &pattern) override;
  virtual void visit (AST::SlicePattern &pattern) override;
  virtual void visit (AST::AltPattern &pattern) override;
  virtual void visit (AST::EmptyStmt &stmt) override;
  virtual void visit (AST::LetStmt &stmt) override;
  virtual void visit (AST::ExprStmt &stmt) override;
  virtual void visit (AST::TraitBound &bound) override;
  virtual void visit (AST::ImplTraitType &type) override;
  virtual void visit (AST::TraitObjectType &type) override;
  virtual void visit (AST::ParenthesisedType &type) override;
  virtual void visit (AST::ImplTraitTypeOneBound &type) override;
  virtual void visit (AST::TraitObjectTypeOneBound &type) override;
  virtual void visit (AST::TupleType &type) override;
  virtual void visit (AST::NeverType &type) override;
  virtual void visit (AST::RawPointerType &type) override;
  virtual void visit (AST::ReferenceType &type) override;
  virtual void visit (AST::ArrayType &type) override;
  virtual void visit (AST::SliceType &type) override;
  virtual void visit (AST::InferredType &type) override;
  virtual void visit (AST::BareFunctionType &type) override;
  virtual void visit (AST::SelfParam &self) override;
  virtual void visit (AST::FunctionParam &param) override;
  virtual void visit (AST::VariadicParam &param) override;
  virtual void visit (AST::FormatArgs &fmt) override;

  template <typename T> void visit (T &node) { node.accept_vis (*this); }

  template <typename T> void visit (std::unique_ptr<T> &node)
  {
    node->accept_vis (*this);
  }

  virtual void visit (AST::GenericArgsBinding &binding);
  virtual void visit (AST::PathExprSegment &segment);
  virtual void visit (AST::GenericArgs &args);
  virtual void visit (AST::QualifiedPathType &path);
  virtual void visit (AST::TypePathFunction &tpf);
  virtual void visit (AST::PathIdentSegment &segment);
  virtual void visit (AST::SimplePath &path);
  virtual void visit (AST::SimplePathSegment &segment);
  virtual void visit (AST::StructBase &base);
  virtual void visit (AST::ClosureParam &param);
  virtual void visit (AST::LoopLabel &label);
  virtual void visit (AST::MatchCase &arm);
  virtual void visit (AST::MatchArm &arm);
  virtual void visit (AST::Visibility &vis);
  virtual void visit (AST::FunctionQualifiers &qualifiers);
  virtual void visit (AST::WhereClause &where);
  virtual void visit (AST::StructField &field);
  virtual void visit (AST::TupleField &field);
  virtual void visit (AST::NamedFunctionParam &param);
  virtual void visit (AST::MacroRule &rule);
  virtual void visit (AST::MacroInvocData &data);
  virtual void visit (AST::MacroTranscriber &transcriber);
  virtual void visit (AST::StructPatternElements &spe);
  virtual void visit (AST::MaybeNamedParam &param);

  void visit (AST::Attribute &attribute) {}

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
};

class ContextualASTVisitor : public DefaultASTVisitor
{
protected:
  enum class Context
  {
    FUNCTION,
    INHERENT_IMPL,
    TRAIT_IMPL,
    TRAIT,
    MODULE,
    CRATE,
  };
  using DefaultASTVisitor::visit;

  virtual void visit (AST::Crate &crate) override;

  virtual void visit (AST::InherentImpl &impl) override;

  virtual void visit (AST::TraitImpl &impl) override;

  virtual void visit (AST::Trait &trait) override;

  template <typename T> void visit (T &item)
  {
    DefaultASTVisitor::visit (item);
  }

  std::vector<Context> context;

  void push_context (Context ctx) { context.push_back (ctx); }

  void pop_context () { context.pop_back (); }
};

} // namespace AST
} // namespace Rust

#endif
