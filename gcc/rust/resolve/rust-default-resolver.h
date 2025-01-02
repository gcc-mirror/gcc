
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

#ifndef RUST_AST_DEFAULT_RESOLVER_H
#define RUST_AST_DEFAULT_RESOLVER_H

#include "rust-ast-visitor.h"
#include "rust-name-resolution-context.h"

namespace Rust {
namespace Resolver2_0 {

/**
 * The `DefaultResolver` is a base visitor for all passes of our name resolution
 * algorithm: `TopLevel`, `Easy` and `Late`. It does not do a lot, apart from
 * visiting each node's subnodes - a block's statements, a function call's
 * arguments...
 */
class DefaultResolver : public AST::DefaultASTVisitor
{
public:
  using AST::DefaultASTVisitor::visit;

  virtual ~DefaultResolver () {}

  // First, our lexical scope expressions - these visit their sub nodes, always
  // these nodes create new scopes and ribs - they are often used to declare new
  // variables, such as a for loop's iterator, or a function's arguments
  void visit (AST::BlockExpr &);
  void visit (AST::Module &);
  void visit (AST::Function &);
  void visit (AST::ForLoopExpr &);
  void visit (AST::Trait &);
  void visit (AST::InherentImpl &);
  void visit (AST::TraitImpl &);

  // type dec nodes, which visit their fields or variants by default
  void visit (AST::StructStruct &);
  void visit (AST::Enum &);

  // Visitors that visit their expression node(s)
  void visit (AST::StructExprFieldIdentifierValue &);
  void visit (AST::StructExprFieldIndexValue &);
  void visit (AST::ClosureExprInner &);
  void visit (AST::ClosureExprInnerTyped &);
  void visit (AST::ContinueExpr &);
  void visit (AST::RangeFromToExpr &);
  void visit (AST::RangeFromExpr &);
  void visit (AST::RangeToExpr &);
  void visit (AST::RangeFromToInclExpr &);
  void visit (AST::RangeToInclExpr &);
  void visit (AST::ReturnExpr &);
  void visit (AST::LoopExpr &);
  void visit (AST::WhileLoopExpr &);
  void visit (AST::WhileLetLoopExpr &);
  void visit (AST::IfExpr &);
  void visit (AST::IfExprConseqElse &);
  void visit (AST::IfLetExpr &);
  void visit (AST::IfLetExprConseqElse &);
  void visit (AST::MatchExpr &);
  void visit (AST::AwaitExpr &);
  void visit (AST::AsyncBlockExpr &);

  // Leaf visitors, which do nothing by default
  void visit (AST::DelimTokenTree &);
  void visit (AST::AttrInputMetaItemContainer &);
  void visit (AST::IdentifierExpr &);
  void visit (AST::LifetimeParam &);
  void visit (AST::ConstGenericParam &);
  void visit (AST::PathInExpression &);
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
  void visit (AST::StructExprStruct &);
  void visit (AST::StructExprStructFields &);
  void visit (AST::StructExprStructBase &);
  void visit (AST::TypeParam &);
  void visit (AST::LifetimeWhereClauseItem &);
  void visit (AST::TypeBoundWhereClauseItem &);
  void visit (AST::ExternCrate &);
  void visit (AST::UseTreeGlob &);
  void visit (AST::UseTreeList &);
  void visit (AST::UseTreeRebind &);
  void visit (AST::UseDeclaration &);
  void visit (AST::TypeAlias &);
  void visit (AST::EnumItem &);
  void visit (AST::EnumItemTuple &);
  void visit (AST::EnumItemStruct &);
  void visit (AST::EnumItemDiscriminant &);
  void visit (AST::ConstantItem &);
  void visit (AST::StaticItem &);
  void visit (AST::TraitItemConst &);
  void visit (AST::TraitItemType &);
  void visit (AST::ExternalTypeItem &);
  void visit (AST::ExternalStaticItem &);
  void visit (AST::MacroMatchRepetition &);
  void visit (AST::MacroMatcher &);
  void visit (AST::MacroRulesDefinition &);
  void visit (AST::MacroInvocation &);
  void visit (AST::MetaItemPath &);
  void visit (AST::MetaItemSeq &);
  void visit (AST::MetaListPaths &);
  void visit (AST::MetaListNameValueStr &);
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
  void visit (AST::TraitBound &);
  void visit (AST::ImplTraitType &);
  void visit (AST::TraitObjectType &);
  void visit (AST::ParenthesisedType &);
  void visit (AST::ImplTraitTypeOneBound &);
  void visit (AST::TraitObjectTypeOneBound &);
  void visit (AST::TupleType &);
  void visit (AST::ReferenceType &);
  void visit (AST::ArrayType &);
  void visit (AST::SliceType &);
  void visit (AST::BareFunctionType &);
  void visit (AST::FunctionParam &);
  void visit (AST::VariadicParam &);
  void visit (AST::SelfParam &);

protected:
  DefaultResolver (NameResolutionContext &ctx) : ctx (ctx) {}

  NameResolutionContext &ctx;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // RUST_AST_DEFAULT_RESOLVER_H
