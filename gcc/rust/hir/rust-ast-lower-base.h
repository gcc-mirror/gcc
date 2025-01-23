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

#ifndef RUST_AST_LOWER_BASE
#define RUST_AST_LOWER_BASE

#include "rust-ast.h"
#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-full.h"
#include "rust-attributes.h"

namespace Rust {
namespace HIR {

// proxy class so we can do attribute checking on items and trait items
class ItemWrapper
{
public:
  ItemWrapper (const HIR::Item &item)
    : mappings (item.get_mappings ()), locus (item.get_locus ()),
      outer_attrs (item.get_outer_attrs ())
  {}

  ItemWrapper (const HIR::TraitItem &item)
    : mappings (item.get_mappings ()), locus (item.get_trait_locus ()),
      outer_attrs (item.get_outer_attrs ())
  {}

  const Analysis::NodeMapping &get_mappings () const { return mappings; }
  location_t get_locus () const { return locus; }
  const AST::AttrVec &get_outer_attrs () const { return outer_attrs; }

private:
  const Analysis::NodeMapping &mappings;
  location_t locus;
  const AST::AttrVec &outer_attrs;
};

// base class to allow derivatives to overload as needed
class ASTLoweringBase : public AST::ASTVisitor
{
public:
  virtual ~ASTLoweringBase () {}

  // Special casing nodes that should never reach the HIR lowering stage
  virtual void visit (AST::MacroInvocation &) override final;
  virtual void visit (AST::ErrorPropagationExpr &) override final;

  // visitor impl
  // rust-ast.h
  //  virtual void visit(AttrInput& attr_input);
  //  virtual void visit(TokenTree& token_tree);
  //  virtual void visit(MacroMatch& macro_match);
  virtual void visit (AST::Token &tok) override;
  virtual void visit (AST::DelimTokenTree &delim_tok_tree) override;
  virtual void visit (AST::AttrInputMetaItemContainer &input) override;
  //  virtual void visit(MetaItem& meta_item) override;
  //  void vsit(Stmt& stmt) override;
  //  virtual void visit(Expr& expr) override;
  virtual void visit (AST::IdentifierExpr &ident_expr) override;
  //  virtual void visit(Pattern& pattern) override;
  //  virtual void visit(Type& type) override;
  //  virtual void visit(TypeParamBound& type_param_bound) override;
  virtual void visit (AST::Lifetime &lifetime) override;
  //  virtual void visit(GenericParam& generic_param) override;
  virtual void visit (AST::LifetimeParam &lifetime_param) override;
  virtual void visit (AST::ConstGenericParam &const_param) override;
  //  virtual void visit(TraitItem& trait_item) override;
  //  virtual void visit(InherentImplItem& inherent_impl_item) override;
  //  virtual void visit(TraitImplItem& trait_impl_item) override;

  // rust-path.h
  virtual void visit (AST::PathInExpression &path) override;
  virtual void visit (AST::TypePathSegment &segment) override;
  virtual void visit (AST::TypePathSegmentGeneric &segment) override;
  virtual void visit (AST::TypePathSegmentFunction &segment) override;
  virtual void visit (AST::TypePath &path) override;
  virtual void visit (AST::QualifiedPathInExpression &path) override;
  virtual void visit (AST::QualifiedPathInType &path) override;

  // rust-expr.h
  virtual void visit (AST::LiteralExpr &expr) override;
  virtual void visit (AST::AttrInputLiteral &attr_input) override;
  virtual void visit (AST::AttrInputMacro &attr_input) override;
  virtual void visit (AST::MetaItemLitExpr &meta_item) override;
  virtual void visit (AST::MetaItemPathLit &meta_item) override;
  virtual void visit (AST::BorrowExpr &expr) override;
  virtual void visit (AST::DereferenceExpr &expr) override;
  virtual void visit (AST::NegationExpr &expr) override;
  virtual void visit (AST::ArithmeticOrLogicalExpr &expr) override;
  virtual void visit (AST::ComparisonExpr &expr) override;
  virtual void visit (AST::LazyBooleanExpr &expr) override;
  virtual void visit (AST::TypeCastExpr &expr) override;
  virtual void visit (AST::AssignmentExpr &expr) override;
  virtual void visit (AST::CompoundAssignmentExpr &expr) override;
  virtual void visit (AST::GroupedExpr &expr) override;
  //  virtual void visit(ArrayElems& elems) override;
  virtual void visit (AST::ArrayElemsValues &elems) override;
  virtual void visit (AST::ArrayElemsCopied &elems) override;
  virtual void visit (AST::ArrayExpr &expr) override;
  virtual void visit (AST::ArrayIndexExpr &expr) override;
  virtual void visit (AST::TupleExpr &expr) override;
  virtual void visit (AST::TupleIndexExpr &expr) override;
  virtual void visit (AST::StructExprStruct &expr) override;
  //  virtual void visit(StructExprField& field) override;
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
  virtual void visit (AST::BoxExpr &expr) override;
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
  virtual void visit (AST::InlineAsm &expr) override;
  //  virtual void visit(MatchCase& match_case) override;
  // virtual void visit (AST::MatchCaseBlockExpr &match_case) override;
  // virtual void visit (AST::MatchCaseExpr &match_case) override;
  virtual void visit (AST::MatchExpr &expr) override;
  virtual void visit (AST::AwaitExpr &expr) override;
  virtual void visit (AST::AsyncBlockExpr &expr) override;

  // rust-item.h
  virtual void visit (AST::TypeParam &param) override;
  //  virtual void visit(WhereClauseItem& item) override;
  virtual void visit (AST::LifetimeWhereClauseItem &item) override;
  virtual void visit (AST::TypeBoundWhereClauseItem &item) override;
  virtual void visit (AST::Module &module) override;
  virtual void visit (AST::ExternCrate &crate) override;
  //  virtual void visit(UseTree& use_tree) override;
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
  //  virtual void visit(ExternalItem& item) override;
  virtual void visit (AST::ExternalTypeItem &item) override;
  virtual void visit (AST::ExternalStaticItem &item) override;
  virtual void visit (AST::ExternBlock &block) override;

  // rust-macro.h
  virtual void visit (AST::MacroMatchFragment &match) override;
  virtual void visit (AST::MacroMatchRepetition &match) override;
  virtual void visit (AST::MacroMatcher &matcher) override;
  virtual void visit (AST::MacroRulesDefinition &rules_def) override;
  virtual void visit (AST::MetaItemPath &meta_item) override;
  virtual void visit (AST::MetaItemSeq &meta_item) override;
  virtual void visit (AST::MetaWord &meta_item) override;
  virtual void visit (AST::MetaNameValueStr &meta_item) override;
  virtual void visit (AST::MetaListPaths &meta_item) override;
  virtual void visit (AST::MetaListNameValueStr &meta_item) override;

  // rust-pattern.h
  virtual void visit (AST::LiteralPattern &pattern) override;
  virtual void visit (AST::IdentifierPattern &pattern) override;
  virtual void visit (AST::WildcardPattern &pattern) override;
  virtual void visit (AST::RestPattern &pattern) override;
  //  virtual void visit(RangePatternBound& bound) override;
  virtual void visit (AST::RangePatternBoundLiteral &bound) override;
  virtual void visit (AST::RangePatternBoundPath &bound) override;
  virtual void visit (AST::RangePatternBoundQualPath &bound) override;
  virtual void visit (AST::RangePattern &pattern) override;
  virtual void visit (AST::ReferencePattern &pattern) override;
  //  virtual void visit(StructPatternField& field) override;
  virtual void visit (AST::StructPatternFieldTuplePat &field) override;
  virtual void visit (AST::StructPatternFieldIdentPat &field) override;
  virtual void visit (AST::StructPatternFieldIdent &field) override;
  virtual void visit (AST::StructPattern &pattern) override;
  //  virtual void visit(TupleStructItems& tuple_items) override;
  virtual void visit (AST::TupleStructItemsNoRange &tuple_items) override;
  virtual void visit (AST::TupleStructItemsRange &tuple_items) override;
  virtual void visit (AST::TupleStructPattern &pattern) override;
  //  virtual void visit(TuplePatternItems& tuple_items) override;
  virtual void visit (AST::TuplePatternItemsMultiple &tuple_items) override;
  virtual void visit (AST::TuplePatternItemsRanged &tuple_items) override;
  virtual void visit (AST::TuplePattern &pattern) override;
  virtual void visit (AST::GroupedPattern &pattern) override;
  virtual void visit (AST::SlicePattern &pattern) override;
  virtual void visit (AST::AltPattern &pattern) override;

  // rust-stmt.h
  virtual void visit (AST::EmptyStmt &stmt) override;
  virtual void visit (AST::LetStmt &stmt) override;
  virtual void visit (AST::ExprStmt &stmt) override;

  // rust-type.h
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
  virtual void visit (AST::FunctionParam &param) override;
  virtual void visit (AST::VariadicParam &param) override;
  virtual void visit (AST::SelfParam &param) override;

  virtual void visit (AST::FormatArgs &fmt) override;

protected:
  ASTLoweringBase ()
    : mappings (Analysis::Mappings::get ()),
      attr_mappings (Analysis::BuiltinAttributeMappings::get ())
  {}

  Analysis::Mappings &mappings;
  Analysis::BuiltinAttributeMappings *attr_mappings;

  HIR::Lifetime lower_lifetime (AST::Lifetime &lifetime,
				bool default_to_static_lifetime = false);

  HIR::LoopLabel lower_loop_label (AST::LoopLabel &loop_label);

  std::vector<std::unique_ptr<HIR::GenericParam> > lower_generic_params (
    std::vector<std::unique_ptr<AST::GenericParam> > &params);

  HIR::PathExprSegment lower_path_expr_seg (AST::PathExprSegment &s);

  HIR::GenericArgs lower_generic_args (AST::GenericArgs &args);

  HIR::GenericArgsBinding lower_binding (AST::GenericArgsBinding &binding);

  HIR::SelfParam lower_self (AST::Param &self);

  HIR::Type *lower_type_no_bounds (AST::TypeNoBounds &type);

  HIR::TypeParamBound *lower_bound (AST::TypeParamBound &bound);

  HIR::QualifiedPathType
  lower_qual_path_type (AST::QualifiedPathType &qual_path_type);

  HIR::FunctionQualifiers
  lower_qualifiers (const AST::FunctionQualifiers &qualifiers);

  void handle_outer_attributes (const ItemWrapper &item);

  void handle_lang_item_attribute (const ItemWrapper &item,
				   const AST::Attribute &attr);

  void handle_doc_item_attribute (const ItemWrapper &item,
				  const AST::Attribute &attr);

  bool is_known_attribute (const std::string &attribute_path) const;

  bool
  attribute_handled_in_another_pass (const std::string &attribute_path) const;

  std::unique_ptr<TuplePatternItems>
  lower_tuple_pattern_multiple (AST::TuplePatternItemsMultiple &pattern);

  std::unique_ptr<TuplePatternItems>
  lower_tuple_pattern_ranged (AST::TuplePatternItemsRanged &pattern);

  std::unique_ptr<HIR::RangePatternBound>
  lower_range_pattern_bound (AST::RangePatternBound &bound);

  HIR::Literal lower_literal (const AST::Literal &literal);

  HIR::ExternBlock *lower_extern_block (AST::ExternBlock &extern_block);

  HIR::ClosureParam lower_closure_param (AST::ClosureParam &param);

  /* Lower a macro definition if it should be exported */
  void lower_macro_definition (AST::MacroRulesDefinition &def);
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_BASE
