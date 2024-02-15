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
#ifndef RUST_ATTRIBUTES_H
#define RUST_ATTRIBUTES_H

#include "rust-ast.h"
#include "rust-system.h"
#include "rust-ast-visitor.h"

namespace Rust {
namespace Analysis {

enum CompilerPass
{
  UNKNOWN,

  EXPANSION,
  NAME_RESOLUTION,
  HIR_LOWERING,
  TYPE_CHECK,
  STATIC_ANALYSIS,
  CODE_GENERATION
};

struct BuiltinAttrDefinition
{
  std::string name;
  CompilerPass handler;

  static BuiltinAttrDefinition get_error ()
  {
    return BuiltinAttrDefinition{"", UNKNOWN};
  }

  static BuiltinAttrDefinition &error_node ()
  {
    static BuiltinAttrDefinition error_node = get_error ();
    return error_node;
  }

  bool is_error () const { return name.empty (); }
};

class BuiltinAttributeMappings
{
public:
  static BuiltinAttributeMappings *get ();

  const BuiltinAttrDefinition &
  lookup_builtin (const std::string &attr_name) const;

private:
  BuiltinAttributeMappings ();

  std::map<std::string, const BuiltinAttrDefinition> mappings;
};

/**
 * Checks the validity of various attributes. The goal of this visitor is to
 * make sure that attributes are applied in allowed contexts, for example to
 * make sure that #[inline] is only applied to functions and closures, as well
 * as checking the "arguments" or input given to these attributes, making sure
 * it is appropriate and valid.
 */
class AttributeChecker : public AST::DefaultASTVisitor
{
public:
  AttributeChecker ();

  /**
   * Check all the attributes of all the items of a crate
   */
  void go (AST::Crate &crate);

private:
  using AST::DefaultASTVisitor::visit;
  /* Check the validity of a given attribute */
  void check_attribute (const AST::Attribute &attribute);

  /* Check the validity of all given attributes */
  void check_attributes (const AST::AttrVec &attributes);

  // rust-ast.h
  void visit (AST::Crate &crate) override;
  void visit (AST::Token &tok) override;
  void visit (AST::DelimTokenTree &delim_tok_tree) override;
  void visit (AST::AttrInputMetaItemContainer &input) override;
  void visit (AST::IdentifierExpr &ident_expr) override;
  void visit (AST::Lifetime &lifetime) override;
  void visit (AST::LifetimeParam &lifetime_param) override;
  void visit (AST::ConstGenericParam &const_param) override;

  // rust-path.h
  void visit (AST::PathInExpression &path) override;
  void visit (AST::TypePathSegment &segment) override;
  void visit (AST::TypePathSegmentGeneric &segment) override;
  void visit (AST::TypePathSegmentFunction &segment) override;
  void visit (AST::TypePath &path) override;
  void visit (AST::QualifiedPathInExpression &path) override;
  void visit (AST::QualifiedPathInType &path) override;

  // rust-expr.h
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

  // rust-item.h
  void visit (AST::TypeParam &param) override;
  void visit (AST::LifetimeWhereClauseItem &item) override;
  void visit (AST::TypeBoundWhereClauseItem &item) override;
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
  void visit (AST::TraitItemConst &item) override;
  void visit (AST::TraitItemType &item) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::InherentImpl &impl) override;
  void visit (AST::TraitImpl &impl) override;
  void visit (AST::ExternalTypeItem &item) override;
  void visit (AST::ExternalStaticItem &item) override;
  void visit (AST::ExternalFunctionItem &item) override;
  void visit (AST::ExternBlock &block) override;

  // rust-macro.h
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

  // rust-pattern.h
  void visit (AST::LiteralPattern &pattern) override;
  void visit (AST::IdentifierPattern &pattern) override;
  void visit (AST::WildcardPattern &pattern) override;
  void visit (AST::RestPattern &pattern) override;
  // void visit(RangePatternBound& bound) override;
  void visit (AST::RangePatternBoundLiteral &bound) override;
  void visit (AST::RangePatternBoundPath &bound) override;
  void visit (AST::RangePatternBoundQualPath &bound) override;
  void visit (AST::RangePattern &pattern) override;
  void visit (AST::ReferencePattern &pattern) override;
  // void visit(StructPatternField& field) override;
  void visit (AST::StructPatternFieldTuplePat &field) override;
  void visit (AST::StructPatternFieldIdentPat &field) override;
  void visit (AST::StructPatternFieldIdent &field) override;
  void visit (AST::StructPattern &pattern) override;
  // void visit(TupleStructItems& tuple_items) override;
  void visit (AST::TupleStructItemsNoRange &tuple_items) override;
  void visit (AST::TupleStructItemsRange &tuple_items) override;
  void visit (AST::TupleStructPattern &pattern) override;
  // void visit(TuplePatternItems& tuple_items) override;
  void visit (AST::TuplePatternItemsMultiple &tuple_items) override;
  void visit (AST::TuplePatternItemsRanged &tuple_items) override;
  void visit (AST::TuplePattern &pattern) override;
  void visit (AST::GroupedPattern &pattern) override;
  void visit (AST::SlicePattern &pattern) override;
  void visit (AST::AltPattern &pattern) override;

  // rust-stmt.h
  void visit (AST::EmptyStmt &stmt) override;
  void visit (AST::LetStmt &stmt) override;
  void visit (AST::ExprStmt &stmt) override;

  // rust-type.h
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
  void visit (AST::FunctionParam &param) override;
  void visit (AST::VariadicParam &param) override;
  void visit (AST::SelfParam &param) override;
};

} // namespace Analysis
} // namespace Rust

#endif /* ! RUST_ATTRIBUTES_H */
