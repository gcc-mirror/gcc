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

#ifndef RUST_AST_VISITOR_H
#define RUST_AST_VISITOR_H
// Visitor base for AST

// full include not required - only forward decls
#include "rust-ast-full-decls.h"

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
  virtual void visit (IfExprConseqIf &expr) = 0;
  virtual void visit (IfExprConseqIfLet &expr) = 0;
  virtual void visit (IfLetExpr &expr) = 0;
  virtual void visit (IfLetExprConseqElse &expr) = 0;
  virtual void visit (IfLetExprConseqIf &expr) = 0;
  virtual void visit (IfLetExprConseqIfLet &expr) = 0;
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
  virtual void visit (ExprStmtWithoutBlock &stmt) = 0;
  virtual void visit (ExprStmtWithBlock &stmt) = 0;

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
} // namespace AST
} // namespace Rust

#endif
