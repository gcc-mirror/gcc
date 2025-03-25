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

#ifndef RUST_AST_RESOLVE_BASE_H
#define RUST_AST_RESOLVE_BASE_H

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-name-resolver.h"
#include "rust-diagnostics.h"
#include "rust-location.h"

namespace Rust {
namespace Resolver {
inline void
redefined_error (const rich_location &loc)
{
  rust_error_at (loc, "redefined multiple times");
}

class ResolverBase : public AST::ASTVisitor
{
public:
  virtual ~ResolverBase () {}

  void visit (AST::Token &);
  void visit (AST::DelimTokenTree &);
  void visit (AST::AttrInputMetaItemContainer &);
  void visit (AST::IdentifierExpr &);
  void visit (AST::Lifetime &);
  void visit (AST::LifetimeParam &);
  void visit (AST::ConstGenericParam &);
  void visit (AST::PathInExpression &);
  void visit (AST::TypePathSegment &);
  void visit (AST::TypePathSegmentGeneric &);
  void visit (AST::TypePathSegmentFunction &);
  void visit (AST::TypePath &);
  void visit (AST::QualifiedPathInExpression &);
  void visit (AST::QualifiedPathInType &);
  void visit (AST::LiteralExpr &);
  void visit (AST::AttrInputLiteral &);
  void visit (AST::AttrInputMacro &);
  void visit (AST::MetaItemLitExpr &);
  void visit (AST::MetaItemPathLit &);
  void visit (AST::BorrowExpr &);
  void visit (AST::DereferenceExpr &);
  void visit (AST::ErrorPropagationExpr &);
  void visit (AST::NegationExpr &);
  void visit (AST::ArithmeticOrLogicalExpr &);
  void visit (AST::ComparisonExpr &);
  void visit (AST::LazyBooleanExpr &);
  void visit (AST::TypeCastExpr &);
  void visit (AST::AssignmentExpr &);
  void visit (AST::CompoundAssignmentExpr &);
  void visit (AST::GroupedExpr &);
  void visit (AST::ArrayElemsValues &);
  void visit (AST::ArrayElemsCopied &);
  void visit (AST::ArrayExpr &);
  void visit (AST::ArrayIndexExpr &);
  void visit (AST::TupleExpr &);
  void visit (AST::TupleIndexExpr &);
  void visit (AST::StructExprStruct &);
  void visit (AST::StructExprFieldIdentifier &);
  void visit (AST::StructExprFieldIdentifierValue &);
  void visit (AST::StructExprFieldIndexValue &);
  void visit (AST::StructExprStructFields &);
  void visit (AST::StructExprStructBase &);
  void visit (AST::CallExpr &);
  void visit (AST::MethodCallExpr &);
  void visit (AST::FieldAccessExpr &);
  void visit (AST::ClosureExprInner &);
  void visit (AST::BlockExpr &);
  void visit (AST::ClosureExprInnerTyped &);
  void visit (AST::ContinueExpr &);
  void visit (AST::BreakExpr &);
  void visit (AST::RangeFromToExpr &);
  void visit (AST::RangeFromExpr &);
  void visit (AST::RangeToExpr &);
  void visit (AST::RangeFullExpr &);
  void visit (AST::RangeFromToInclExpr &);
  void visit (AST::RangeToInclExpr &);
  void visit (AST::BoxExpr &);
  void visit (AST::ReturnExpr &);
  void visit (AST::UnsafeBlockExpr &);
  void visit (AST::LoopExpr &);
  void visit (AST::WhileLoopExpr &);
  void visit (AST::WhileLetLoopExpr &);
  void visit (AST::ForLoopExpr &);
  void visit (AST::IfExpr &);
  void visit (AST::IfExprConseqElse &);
  void visit (AST::IfLetExpr &);
  void visit (AST::IfLetExprConseqElse &);

  void visit (AST::MatchExpr &);
  void visit (AST::AwaitExpr &);
  void visit (AST::AsyncBlockExpr &);
  void visit (AST::InlineAsm &);

  void visit (AST::TypeParam &);

  void visit (AST::LifetimeWhereClauseItem &);
  void visit (AST::TypeBoundWhereClauseItem &);
  void visit (AST::Module &);
  void visit (AST::ExternCrate &);

  void visit (AST::UseTreeGlob &);
  void visit (AST::UseTreeList &);
  void visit (AST::UseTreeRebind &);
  void visit (AST::UseDeclaration &);
  void visit (AST::Function &);
  void visit (AST::TypeAlias &);
  void visit (AST::StructStruct &);
  void visit (AST::TupleStruct &);
  void visit (AST::EnumItem &);
  void visit (AST::EnumItemTuple &);
  void visit (AST::EnumItemStruct &);
  void visit (AST::EnumItemDiscriminant &);
  void visit (AST::Enum &);
  void visit (AST::Union &);
  void visit (AST::ConstantItem &);
  void visit (AST::StaticItem &);
  void visit (AST::TraitItemConst &);
  void visit (AST::TraitItemType &);
  void visit (AST::Trait &);
  void visit (AST::InherentImpl &);
  void visit (AST::TraitImpl &);

  void visit (AST::ExternalTypeItem &);
  void visit (AST::ExternalStaticItem &);
  void visit (AST::ExternBlock &);

  void visit (AST::MacroMatchFragment &);
  void visit (AST::MacroMatchRepetition &);
  void visit (AST::MacroMatcher &);
  void visit (AST::MacroRulesDefinition &);
  void visit (AST::MacroInvocation &);
  void visit (AST::MetaItemPath &);
  void visit (AST::MetaItemSeq &);
  void visit (AST::MetaWord &);
  void visit (AST::MetaNameValueStr &);
  void visit (AST::MetaListPaths &);
  void visit (AST::MetaListNameValueStr &);

  void visit (AST::LiteralPattern &);
  void visit (AST::IdentifierPattern &);
  void visit (AST::WildcardPattern &);
  void visit (AST::RestPattern &);

  void visit (AST::RangePatternBoundLiteral &);
  void visit (AST::RangePatternBoundPath &);
  void visit (AST::RangePatternBoundQualPath &);
  void visit (AST::RangePattern &);
  void visit (AST::ReferencePattern &);

  void visit (AST::StructPatternFieldTuplePat &);
  void visit (AST::StructPatternFieldIdentPat &);
  void visit (AST::StructPatternFieldIdent &);
  void visit (AST::StructPattern &);

  void visit (AST::TupleStructItemsNoRange &);
  void visit (AST::TupleStructItemsRange &);
  void visit (AST::TupleStructPattern &);

  void visit (AST::TuplePatternItemsMultiple &);
  void visit (AST::TuplePatternItemsRanged &);
  void visit (AST::TuplePattern &);
  void visit (AST::GroupedPattern &);
  void visit (AST::SlicePattern &);
  void visit (AST::AltPattern &);

  void visit (AST::EmptyStmt &);
  void visit (AST::LetStmt &);
  void visit (AST::ExprStmt &);

  void visit (AST::TraitBound &);
  void visit (AST::ImplTraitType &);
  void visit (AST::TraitObjectType &);
  void visit (AST::ParenthesisedType &);
  void visit (AST::ImplTraitTypeOneBound &);
  void visit (AST::TraitObjectTypeOneBound &);
  void visit (AST::TupleType &);
  void visit (AST::NeverType &);
  void visit (AST::RawPointerType &);
  void visit (AST::ReferenceType &);
  void visit (AST::ArrayType &);
  void visit (AST::SliceType &);
  void visit (AST::InferredType &);
  void visit (AST::BareFunctionType &);
  void visit (AST::FunctionParam &param);
  void visit (AST::VariadicParam &param);
  void visit (AST::SelfParam &param);

  void visit (AST::FormatArgs &fmt);

protected:
  ResolverBase ()
    : resolver (Resolver::get ()), mappings (Analysis::Mappings::get ()),
      resolved_node (UNKNOWN_NODEID)
  {}

  /**
   * Resolve a visibility's path through the name resolver
   */
  bool resolve_visibility (const AST::Visibility &vis);

  Resolver *resolver;
  Analysis::Mappings &mappings;
  NodeId resolved_node;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_BASE_H
