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

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-expr.h"
#include "rust-ast-resolve-path.h"
#include "rust-item.h"

namespace Rust {
namespace Resolver {

bool
ResolverBase::resolve_visibility (const AST::Visibility &vis)
{
  if (vis.has_path ())
    {
      auto path = vis.get_path ();
      ResolvePath::go (&path);

      // Do we need to lookup something here?
      // Is it just about resolving the names correctly so we can look them up
      // later?
    }

  return true;
}

// Default visitors implementations

void
ResolverBase::visit (AST::Token &)
{}

void
ResolverBase::visit (AST::DelimTokenTree &)
{}

void
ResolverBase::visit (AST::AttrInputMetaItemContainer &)
{}

void
ResolverBase::visit (AST::IdentifierExpr &)
{}

void
ResolverBase::visit (AST::Lifetime &)
{}

void
ResolverBase::visit (AST::LifetimeParam &)
{}

void
ResolverBase::visit (AST::ConstGenericParam &)
{}

void
ResolverBase::visit (AST::PathInExpression &)
{}

void
ResolverBase::visit (AST::TypePathSegment &)
{}

void
ResolverBase::visit (AST::TypePathSegmentGeneric &)
{}

void
ResolverBase::visit (AST::TypePathSegmentFunction &)
{}

void
ResolverBase::visit (AST::TypePath &)
{}

void
ResolverBase::visit (AST::QualifiedPathInExpression &)
{}

void
ResolverBase::visit (AST::QualifiedPathInType &)
{}

void
ResolverBase::visit (AST::LiteralExpr &)
{}

void
ResolverBase::visit (AST::AttrInputLiteral &)
{}

void
ResolverBase::visit (AST::MetaItemLitExpr &)
{}

void
ResolverBase::visit (AST::MetaItemPathLit &)
{}

void
ResolverBase::visit (AST::BorrowExpr &)
{}

void
ResolverBase::visit (AST::DereferenceExpr &)
{}

void
ResolverBase::visit (AST::ErrorPropagationExpr &)
{}

void
ResolverBase::visit (AST::NegationExpr &)
{}

void
ResolverBase::visit (AST::ArithmeticOrLogicalExpr &)
{}

void
ResolverBase::visit (AST::ComparisonExpr &)
{}

void
ResolverBase::visit (AST::LazyBooleanExpr &)
{}

void
ResolverBase::visit (AST::TypeCastExpr &)
{}

void
ResolverBase::visit (AST::AssignmentExpr &)
{}

void
ResolverBase::visit (AST::CompoundAssignmentExpr &)
{}

void
ResolverBase::visit (AST::GroupedExpr &)
{}

void
ResolverBase::visit (AST::ArrayElemsValues &)
{}

void
ResolverBase::visit (AST::ArrayElemsCopied &)
{}

void
ResolverBase::visit (AST::ArrayExpr &)
{}

void
ResolverBase::visit (AST::ArrayIndexExpr &)
{}

void
ResolverBase::visit (AST::TupleExpr &)
{}

void
ResolverBase::visit (AST::TupleIndexExpr &)
{}

void
ResolverBase::visit (AST::StructExprStruct &)
{}

void
ResolverBase::visit (AST::StructExprFieldIdentifier &)
{}

void
ResolverBase::visit (AST::StructExprFieldIdentifierValue &)
{}

void
ResolverBase::visit (AST::StructExprFieldIndexValue &)
{}

void
ResolverBase::visit (AST::StructExprStructFields &)
{}

void
ResolverBase::visit (AST::StructExprStructBase &)
{}

void
ResolverBase::visit (AST::CallExpr &)
{}

void
ResolverBase::visit (AST::MethodCallExpr &)
{}

void
ResolverBase::visit (AST::FieldAccessExpr &)
{}

void
ResolverBase::visit (AST::ClosureExprInner &)
{}

void
ResolverBase::visit (AST::BlockExpr &)
{}

void
ResolverBase::visit (AST::ClosureExprInnerTyped &)
{}

void
ResolverBase::visit (AST::ContinueExpr &)
{}

void
ResolverBase::visit (AST::BreakExpr &)
{}

void
ResolverBase::visit (AST::RangeFromToExpr &)
{}

void
ResolverBase::visit (AST::RangeFromExpr &)
{}

void
ResolverBase::visit (AST::RangeToExpr &)
{}

void
ResolverBase::visit (AST::RangeFullExpr &)
{}

void
ResolverBase::visit (AST::RangeFromToInclExpr &)
{}

void
ResolverBase::visit (AST::RangeToInclExpr &)
{}

void
ResolverBase::visit (AST::ReturnExpr &)
{}

void
ResolverBase::visit (AST::UnsafeBlockExpr &)
{}

void
ResolverBase::visit (AST::LoopExpr &)
{}

void
ResolverBase::visit (AST::WhileLoopExpr &)
{}

void
ResolverBase::visit (AST::WhileLetLoopExpr &)
{}

void
ResolverBase::visit (AST::ForLoopExpr &)
{}

void
ResolverBase::visit (AST::IfExpr &)
{}

void
ResolverBase::visit (AST::IfExprConseqElse &)
{}

void
ResolverBase::visit (AST::IfExprConseqIf &)
{}

void
ResolverBase::visit (AST::IfExprConseqIfLet &)
{}

void
ResolverBase::visit (AST::IfLetExpr &)
{}

void
ResolverBase::visit (AST::IfLetExprConseqElse &)
{}

void
ResolverBase::visit (AST::IfLetExprConseqIf &)
{}

void
ResolverBase::visit (AST::IfLetExprConseqIfLet &)
{}

void
ResolverBase::visit (AST::MatchExpr &)
{}

void
ResolverBase::visit (AST::AwaitExpr &)
{}

void
ResolverBase::visit (AST::AsyncBlockExpr &)
{}

void
ResolverBase::visit (AST::TypeParam &)
{}

void
ResolverBase::visit (AST::LifetimeWhereClauseItem &)
{}

void
ResolverBase::visit (AST::TypeBoundWhereClauseItem &)
{}

void
ResolverBase::visit (AST::Method &)
{}

void
ResolverBase::visit (AST::Module &)
{}

void
ResolverBase::visit (AST::ExternCrate &)
{}

void
ResolverBase::visit (AST::UseTreeGlob &)
{}

void
ResolverBase::visit (AST::UseTreeList &)
{}

void
ResolverBase::visit (AST::UseTreeRebind &)
{}

void
ResolverBase::visit (AST::UseDeclaration &)
{}

void
ResolverBase::visit (AST::Function &)
{}

void
ResolverBase::visit (AST::TypeAlias &)
{}

void
ResolverBase::visit (AST::StructStruct &)
{}

void
ResolverBase::visit (AST::TupleStruct &)
{}

void
ResolverBase::visit (AST::EnumItem &)
{}

void
ResolverBase::visit (AST::EnumItemTuple &)
{}

void
ResolverBase::visit (AST::EnumItemStruct &)
{}

void
ResolverBase::visit (AST::EnumItemDiscriminant &)
{}

void
ResolverBase::visit (AST::Enum &)
{}

void
ResolverBase::visit (AST::Union &)
{}

void
ResolverBase::visit (AST::ConstantItem &)
{}

void
ResolverBase::visit (AST::StaticItem &)
{}

void
ResolverBase::visit (AST::TraitItemFunc &)
{}

void
ResolverBase::visit (AST::TraitItemMethod &)
{}

void
ResolverBase::visit (AST::TraitItemConst &)
{}

void
ResolverBase::visit (AST::TraitItemType &)
{}

void
ResolverBase::visit (AST::Trait &)
{}

void
ResolverBase::visit (AST::InherentImpl &)
{}

void
ResolverBase::visit (AST::TraitImpl &)
{}

void
ResolverBase::visit (AST::ExternalStaticItem &)
{}

void
ResolverBase::visit (AST::ExternalFunctionItem &)
{}

void
ResolverBase::visit (AST::ExternBlock &)
{}

void
ResolverBase::visit (AST::MacroMatchFragment &)
{}

void
ResolverBase::visit (AST::MacroMatchRepetition &)
{}

void
ResolverBase::visit (AST::MacroMatcher &)
{}

void
ResolverBase::visit (AST::MacroRulesDefinition &)
{}

void
ResolverBase::visit (AST::MacroInvocation &)
{}

void
ResolverBase::visit (AST::MetaItemPath &)
{}

void
ResolverBase::visit (AST::MetaItemSeq &)
{}

void
ResolverBase::visit (AST::MetaWord &)
{}

void
ResolverBase::visit (AST::MetaNameValueStr &)
{}

void
ResolverBase::visit (AST::MetaListPaths &)
{}

void
ResolverBase::visit (AST::MetaListNameValueStr &)
{}

void
ResolverBase::visit (AST::LiteralPattern &)
{}

void
ResolverBase::visit (AST::IdentifierPattern &)
{}

void
ResolverBase::visit (AST::WildcardPattern &)
{}

void
ResolverBase::visit (AST::RangePatternBoundLiteral &)
{}

void
ResolverBase::visit (AST::RangePatternBoundPath &)
{}

void
ResolverBase::visit (AST::RangePatternBoundQualPath &)
{}

void
ResolverBase::visit (AST::RangePattern &)
{}

void
ResolverBase::visit (AST::ReferencePattern &)
{}

void
ResolverBase::visit (AST::StructPatternFieldTuplePat &)
{}

void
ResolverBase::visit (AST::StructPatternFieldIdentPat &)
{}

void
ResolverBase::visit (AST::StructPatternFieldIdent &)
{}

void
ResolverBase::visit (AST::StructPattern &)
{}

void
ResolverBase::visit (AST::TupleStructItemsNoRange &)
{}

void
ResolverBase::visit (AST::TupleStructItemsRange &)
{}

void
ResolverBase::visit (AST::TupleStructPattern &)
{}

void
ResolverBase::visit (AST::TuplePatternItemsMultiple &)
{}

void
ResolverBase::visit (AST::TuplePatternItemsRanged &)
{}

void
ResolverBase::visit (AST::TuplePattern &)
{}

void
ResolverBase::visit (AST::GroupedPattern &)
{}

void
ResolverBase::visit (AST::SlicePattern &)
{}

void
ResolverBase::visit (AST::AltPattern &)
{}

void
ResolverBase::visit (AST::EmptyStmt &)
{}

void
ResolverBase::visit (AST::LetStmt &)
{}

void
ResolverBase::visit (AST::ExprStmtWithoutBlock &)
{}

void
ResolverBase::visit (AST::ExprStmtWithBlock &)
{}

void
ResolverBase::visit (AST::TraitBound &)
{}

void
ResolverBase::visit (AST::ImplTraitType &)
{}

void
ResolverBase::visit (AST::TraitObjectType &)
{}

void
ResolverBase::visit (AST::ParenthesisedType &)
{}

void
ResolverBase::visit (AST::ImplTraitTypeOneBound &)
{}

void
ResolverBase::visit (AST::TraitObjectTypeOneBound &)
{}

void
ResolverBase::visit (AST::TupleType &)
{}

void
ResolverBase::visit (AST::NeverType &)
{}

void
ResolverBase::visit (AST::RawPointerType &)
{}

void
ResolverBase::visit (AST::ReferenceType &)
{}

void
ResolverBase::visit (AST::ArrayType &)
{}

void
ResolverBase::visit (AST::SliceType &)
{}

void
ResolverBase::visit (AST::InferredType &)
{}

void
ResolverBase::visit (AST::BareFunctionType &)
{}

} // namespace Resolver
} // namespace Rust
