
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
  void visit (AST::BlockExpr &) override;
  void visit (AST::Module &) override;
  void visit (AST::Function &) override;
  void visit (AST::ForLoopExpr &) override;
  void visit (AST::Trait &) override;
  void visit (AST::InherentImpl &) override;
  void visit (AST::TraitImpl &) override;

  // type dec nodes, which visit their fields or variants by default
  void visit (AST::StructStruct &) override;
  void visit (AST::Enum &) override;

  // Visitors that visit their expression node(s)
  void visit (AST::StructExprFieldIdentifierValue &) override;
  void visit (AST::StructExprFieldIndexValue &) override;
  void visit (AST::ClosureExprInner &) override;
  void visit (AST::ClosureExprInnerTyped &) override;
  void visit (AST::ContinueExpr &) override;
  void visit (AST::RangeFromToExpr &) override;
  void visit (AST::RangeFromExpr &) override;
  void visit (AST::RangeToExpr &) override;
  void visit (AST::RangeFromToInclExpr &) override;
  void visit (AST::RangeToInclExpr &) override;
  void visit (AST::ReturnExpr &) override;
  void visit (AST::CallExpr &) override;
  void visit (AST::MethodCallExpr &) override;
  void visit (AST::LoopExpr &) override;
  void visit (AST::WhileLoopExpr &) override;
  void visit (AST::WhileLetLoopExpr &) override;
  void visit (AST::IfExpr &) override;
  void visit (AST::IfExprConseqElse &) override;
  void visit (AST::IfLetExpr &) override;
  void visit (AST::IfLetExprConseqElse &) override;
  void visit (AST::MatchExpr &) override;
  void visit (AST::AwaitExpr &) override;
  void visit (AST::AsyncBlockExpr &) override;

  // Leaf visitors, which do nothing by default
  void visit (AST::DelimTokenTree &) override;
  void visit (AST::AttrInputMetaItemContainer &) override;
  void visit (AST::IdentifierExpr &) override;
  void visit (AST::LifetimeParam &) override;
  void visit (AST::ConstGenericParam &) override;
  void visit (AST::PathInExpression &) override;
  void visit (AST::TypePathSegmentGeneric &) override;
  void visit (AST::TypePathSegmentFunction &) override;
  void visit (AST::TypePath &) override;
  void visit (AST::QualifiedPathInExpression &) override;
  void visit (AST::QualifiedPathInType &) override;
  void visit (AST::LiteralExpr &) override;
  void visit (AST::AttrInputLiteral &) override;
  void visit (AST::AttrInputMacro &) override;
  void visit (AST::MetaItemLitExpr &) override;
  void visit (AST::MetaItemPathLit &) override;
  void visit (AST::StructExprStruct &) override;
  void visit (AST::StructExprStructFields &) override;
  void visit (AST::StructExprStructBase &) override;
  void visit (AST::TypeParam &) override;
  void visit (AST::LifetimeWhereClauseItem &) override;
  void visit (AST::TypeBoundWhereClauseItem &) override;
  void visit (AST::ExternCrate &) override;
  void visit (AST::UseTreeGlob &) override;
  void visit (AST::UseTreeList &) override;
  void visit (AST::UseTreeRebind &) override;
  void visit (AST::UseDeclaration &) override;
  void visit (AST::TypeAlias &) override;
  void visit (AST::EnumItem &) override;
  void visit (AST::EnumItemTuple &) override;
  void visit (AST::EnumItemStruct &) override;
  void visit (AST::EnumItemDiscriminant &) override;
  void visit (AST::ConstantItem &) override;
  void visit (AST::StaticItem &) override;
  void visit (AST::TraitItemConst &) override;
  void visit (AST::TraitItemType &) override;
  void visit (AST::ExternalTypeItem &) override;
  void visit (AST::ExternalStaticItem &) override;
  void visit (AST::MacroMatchRepetition &) override;
  void visit (AST::MacroMatcher &) override;
  void visit (AST::MacroRulesDefinition &) override;
  void visit (AST::MacroInvocation &) override;
  void visit (AST::MetaItemPath &) override;
  void visit (AST::MetaItemSeq &) override;
  void visit (AST::MetaListPaths &) override;
  void visit (AST::MetaListNameValueStr &) override;
  void visit (AST::AltPattern &) override;
  void visit (AST::EmptyStmt &) override;
  void visit (AST::TraitBound &) override;
  void visit (AST::ImplTraitType &) override;
  void visit (AST::TraitObjectType &) override;
  void visit (AST::ImplTraitTypeOneBound &) override;
  void visit (AST::TraitObjectTypeOneBound &) override;
  void visit (AST::BareFunctionType &) override;
  void visit (AST::FunctionParam &) override;
  void visit (AST::VariadicParam &) override;
  void visit (AST::SelfParam &) override;

protected:
  DefaultResolver (NameResolutionContext &ctx) : ctx (ctx) {}

  NameResolutionContext &ctx;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // RUST_AST_DEFAULT_RESOLVER_H
