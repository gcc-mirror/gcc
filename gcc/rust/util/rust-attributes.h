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
class AttributeChecker : public AST::ASTVisitor
{
public:
  AttributeChecker ();

  /**
   * Check all the attributes of all the items of a crate
   */
  void go (AST::Crate &crate);

private:
  /* Check the validity of a given attribute */
  void check_attribute (const AST::Attribute &attribute);

  /* Check the validity of all given attributes */
  void check_attributes (const AST::AttrVec &attributes);

  // rust-ast.h
  void visit (AST::Token &tok);
  void visit (AST::DelimTokenTree &delim_tok_tree);
  void visit (AST::AttrInputMetaItemContainer &input);
  void visit (AST::IdentifierExpr &ident_expr);
  void visit (AST::Lifetime &lifetime);
  void visit (AST::LifetimeParam &lifetime_param);
  void visit (AST::ConstGenericParam &const_param);

  // rust-path.h
  void visit (AST::PathInExpression &path);
  void visit (AST::TypePathSegment &segment);
  void visit (AST::TypePathSegmentGeneric &segment);
  void visit (AST::TypePathSegmentFunction &segment);
  void visit (AST::TypePath &path);
  void visit (AST::QualifiedPathInExpression &path);
  void visit (AST::QualifiedPathInType &path);

  // rust-expr.h
  void visit (AST::LiteralExpr &expr);
  void visit (AST::AttrInputLiteral &attr_input);
  void visit (AST::MetaItemLitExpr &meta_item);
  void visit (AST::MetaItemPathLit &meta_item);
  void visit (AST::BorrowExpr &expr);
  void visit (AST::DereferenceExpr &expr);
  void visit (AST::ErrorPropagationExpr &expr);
  void visit (AST::NegationExpr &expr);
  void visit (AST::ArithmeticOrLogicalExpr &expr);
  void visit (AST::ComparisonExpr &expr);
  void visit (AST::LazyBooleanExpr &expr);
  void visit (AST::TypeCastExpr &expr);
  void visit (AST::AssignmentExpr &expr);
  void visit (AST::CompoundAssignmentExpr &expr);
  void visit (AST::GroupedExpr &expr);
  void visit (AST::ArrayElemsValues &elems);
  void visit (AST::ArrayElemsCopied &elems);
  void visit (AST::ArrayExpr &expr);
  void visit (AST::ArrayIndexExpr &expr);
  void visit (AST::TupleExpr &expr);
  void visit (AST::TupleIndexExpr &expr);
  void visit (AST::StructExprStruct &expr);
  void visit (AST::StructExprFieldIdentifier &field);
  void visit (AST::StructExprFieldIdentifierValue &field);
  void visit (AST::StructExprFieldIndexValue &field);
  void visit (AST::StructExprStructFields &expr);
  void visit (AST::StructExprStructBase &expr);
  void visit (AST::CallExpr &expr);
  void visit (AST::MethodCallExpr &expr);
  void visit (AST::FieldAccessExpr &expr);
  void visit (AST::ClosureExprInner &expr);
  void visit (AST::BlockExpr &expr);
  void visit (AST::ClosureExprInnerTyped &expr);
  void visit (AST::ContinueExpr &expr);
  void visit (AST::BreakExpr &expr);
  void visit (AST::RangeFromToExpr &expr);
  void visit (AST::RangeFromExpr &expr);
  void visit (AST::RangeToExpr &expr);
  void visit (AST::RangeFullExpr &expr);
  void visit (AST::RangeFromToInclExpr &expr);
  void visit (AST::RangeToInclExpr &expr);
  void visit (AST::ReturnExpr &expr);
  void visit (AST::UnsafeBlockExpr &expr);
  void visit (AST::LoopExpr &expr);
  void visit (AST::WhileLoopExpr &expr);
  void visit (AST::WhileLetLoopExpr &expr);
  void visit (AST::ForLoopExpr &expr);
  void visit (AST::IfExpr &expr);
  void visit (AST::IfExprConseqElse &expr);
  void visit (AST::IfExprConseqIf &expr);
  void visit (AST::IfExprConseqIfLet &expr);
  void visit (AST::IfLetExpr &expr);
  void visit (AST::IfLetExprConseqElse &expr);
  void visit (AST::IfLetExprConseqIf &expr);
  void visit (AST::IfLetExprConseqIfLet &expr);
  void visit (AST::MatchExpr &expr);
  void visit (AST::AwaitExpr &expr);
  void visit (AST::AsyncBlockExpr &expr);

  // rust-item.h
  void visit (AST::TypeParam &param);
  void visit (AST::LifetimeWhereClauseItem &item);
  void visit (AST::TypeBoundWhereClauseItem &item);
  void visit (AST::Method &method);
  void visit (AST::Module &module);
  void visit (AST::ExternCrate &crate);
  void visit (AST::UseTreeGlob &use_tree);
  void visit (AST::UseTreeList &use_tree);
  void visit (AST::UseTreeRebind &use_tree);
  void visit (AST::UseDeclaration &use_decl);
  void visit (AST::Function &function);
  void visit (AST::TypeAlias &type_alias);
  void visit (AST::StructStruct &struct_item);
  void visit (AST::TupleStruct &tuple_struct);
  void visit (AST::EnumItem &item);
  void visit (AST::EnumItemTuple &item);
  void visit (AST::EnumItemStruct &item);
  void visit (AST::EnumItemDiscriminant &item);
  void visit (AST::Enum &enum_item);
  void visit (AST::Union &union_item);
  void visit (AST::ConstantItem &const_item);
  void visit (AST::StaticItem &static_item);
  void visit (AST::TraitItemFunc &item);
  void visit (AST::TraitItemMethod &item);
  void visit (AST::TraitItemConst &item);
  void visit (AST::TraitItemType &item);
  void visit (AST::Trait &trait);
  void visit (AST::InherentImpl &impl);
  void visit (AST::TraitImpl &impl);
  void visit (AST::ExternalStaticItem &item);
  void visit (AST::ExternalFunctionItem &item);
  void visit (AST::ExternBlock &block);

  // rust-macro.h
  void visit (AST::MacroMatchFragment &match);
  void visit (AST::MacroMatchRepetition &match);
  void visit (AST::MacroMatcher &matcher);
  void visit (AST::MacroRulesDefinition &rules_def);
  void visit (AST::MacroInvocation &macro_invoc);
  void visit (AST::MetaItemPath &meta_item);
  void visit (AST::MetaItemSeq &meta_item);
  void visit (AST::MetaWord &meta_item);
  void visit (AST::MetaNameValueStr &meta_item);
  void visit (AST::MetaListPaths &meta_item);
  void visit (AST::MetaListNameValueStr &meta_item);

  // rust-pattern.h
  void visit (AST::LiteralPattern &pattern);
  void visit (AST::IdentifierPattern &pattern);
  void visit (AST::WildcardPattern &pattern);
  // void visit(RangePatternBound& bound);
  void visit (AST::RangePatternBoundLiteral &bound);
  void visit (AST::RangePatternBoundPath &bound);
  void visit (AST::RangePatternBoundQualPath &bound);
  void visit (AST::RangePattern &pattern);
  void visit (AST::ReferencePattern &pattern);
  // void visit(StructPatternField& field);
  void visit (AST::StructPatternFieldTuplePat &field);
  void visit (AST::StructPatternFieldIdentPat &field);
  void visit (AST::StructPatternFieldIdent &field);
  void visit (AST::StructPattern &pattern);
  // void visit(TupleStructItems& tuple_items);
  void visit (AST::TupleStructItemsNoRange &tuple_items);
  void visit (AST::TupleStructItemsRange &tuple_items);
  void visit (AST::TupleStructPattern &pattern);
  // void visit(TuplePatternItems& tuple_items);
  void visit (AST::TuplePatternItemsMultiple &tuple_items);
  void visit (AST::TuplePatternItemsRanged &tuple_items);
  void visit (AST::TuplePattern &pattern);
  void visit (AST::GroupedPattern &pattern);
  void visit (AST::SlicePattern &pattern);
  void visit (AST::AltPattern &pattern);

  // rust-stmt.h
  void visit (AST::EmptyStmt &stmt);
  void visit (AST::LetStmt &stmt);
  void visit (AST::ExprStmtWithoutBlock &stmt);
  void visit (AST::ExprStmtWithBlock &stmt);

  // rust-type.h
  void visit (AST::TraitBound &bound);
  void visit (AST::ImplTraitType &type);
  void visit (AST::TraitObjectType &type);
  void visit (AST::ParenthesisedType &type);
  void visit (AST::ImplTraitTypeOneBound &type);
  void visit (AST::TraitObjectTypeOneBound &type);
  void visit (AST::TupleType &type);
  void visit (AST::NeverType &type);
  void visit (AST::RawPointerType &type);
  void visit (AST::ReferenceType &type);
  void visit (AST::ArrayType &type);
  void visit (AST::SliceType &type);
  void visit (AST::InferredType &type);
  void visit (AST::BareFunctionType &type);
};

} // namespace Analysis
} // namespace Rust
