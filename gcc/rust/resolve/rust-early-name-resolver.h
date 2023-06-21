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

#ifndef RUST_EARLY_NAME_RESOLVER_H
#define RUST_EARLY_NAME_RESOLVER_H

#include "rust-name-resolver.h"
#include "rust-system.h"
#include "rust-ast.h"
#include "rust-ast-visitor.h"

namespace Rust {
namespace Resolver {

class EarlyNameResolver : public AST::ASTVisitor
{
public:
  EarlyNameResolver ();

  void go (AST::Crate &crate);

private:
  /**
   * Execute a lambda within a scope. This is equivalent to calling
   * `enter_scope` before your code and `exit_scope` after. This ensures
   * no errors can be committed
   */
  void scoped (NodeId scope_id, std::function<void ()> fn)
  {
    auto old_scope = current_scope;
    current_scope = scope_id;
    resolver.get_macro_scope ().push (scope_id);
    resolver.push_new_macro_rib (resolver.get_macro_scope ().peek ());

    fn ();

    resolver.get_macro_scope ().pop ();
    current_scope = old_scope;
  }

  /**
   * The "scope" we are currently in.
   *
   * This involves lexical scopes:
   *
   * ```rust
   * // current_scope = crate_id;
   * macro_rules! foo { () => {} )
   *
   * {
   *     // current_scope = current_block_id;
   *     macro_rules! foo { () => { something!(); } }
   * }
   * // current_scope = crate_id;
   * ```
   *
   * as well as any sort of scope-like structure that might impact import name
   * resolution or macro name resolution:
   *
   * ```rust
   * macro_rules! foo {
   *     () => { fn empty() {} }
   * }
   *
   *
   * trait Foo {
   *     fn foo() {
   *         fn inner_foo() {
   *             macro_rules! foo { () => {} )
   *
   *             foo!();
   *         }
   *
   *         foo!();
   *     }
   *
   *     foo!();
   * }
   *
   * foo!();
   * ```
   */
  NodeId current_scope;

  /* The crate's scope */
  NodeId crate_scope;

  Resolver &resolver;
  Analysis::Mappings &mappings;

  /**
   * Early name-resolve generic args, which can be macro invocations
   */
  void resolve_generic_args (AST::GenericArgs &generic_args);

  /**
   * Early name-resolve a qualified path type, which can contain macro
   * invocations
   */
  void resolve_qualified_path_type (AST::QualifiedPathType &path);

  virtual void visit (AST::Token &tok);
  virtual void visit (AST::DelimTokenTree &delim_tok_tree);
  virtual void visit (AST::AttrInputMetaItemContainer &input);
  virtual void visit (AST::IdentifierExpr &ident_expr);
  virtual void visit (AST::Lifetime &lifetime);
  virtual void visit (AST::LifetimeParam &lifetime_param);
  virtual void visit (AST::ConstGenericParam &const_param);
  virtual void visit (AST::PathInExpression &path);
  virtual void visit (AST::TypePathSegment &segment);
  virtual void visit (AST::TypePathSegmentGeneric &segment);
  virtual void visit (AST::TypePathSegmentFunction &segment);
  virtual void visit (AST::TypePath &path);
  virtual void visit (AST::QualifiedPathInExpression &path);
  virtual void visit (AST::QualifiedPathInType &path);
  virtual void visit (AST::LiteralExpr &expr);
  virtual void visit (AST::AttrInputLiteral &attr_input);
  virtual void visit (AST::MetaItemLitExpr &meta_item);
  virtual void visit (AST::MetaItemPathLit &meta_item);
  virtual void visit (AST::BorrowExpr &expr);
  virtual void visit (AST::DereferenceExpr &expr);
  virtual void visit (AST::ErrorPropagationExpr &expr);
  virtual void visit (AST::NegationExpr &expr);
  virtual void visit (AST::ArithmeticOrLogicalExpr &expr);
  virtual void visit (AST::ComparisonExpr &expr);
  virtual void visit (AST::LazyBooleanExpr &expr);
  virtual void visit (AST::TypeCastExpr &expr);
  virtual void visit (AST::AssignmentExpr &expr);
  virtual void visit (AST::CompoundAssignmentExpr &expr);
  virtual void visit (AST::GroupedExpr &expr);
  virtual void visit (AST::ArrayElemsValues &elems);
  virtual void visit (AST::ArrayElemsCopied &elems);
  virtual void visit (AST::ArrayExpr &expr);
  virtual void visit (AST::ArrayIndexExpr &expr);
  virtual void visit (AST::TupleExpr &expr);
  virtual void visit (AST::TupleIndexExpr &expr);
  virtual void visit (AST::StructExprStruct &expr);
  virtual void visit (AST::StructExprFieldIdentifier &field);
  virtual void visit (AST::StructExprFieldIdentifierValue &field);
  virtual void visit (AST::StructExprFieldIndexValue &field);
  virtual void visit (AST::StructExprStructFields &expr);
  virtual void visit (AST::StructExprStructBase &expr);
  virtual void visit (AST::CallExpr &expr);
  virtual void visit (AST::MethodCallExpr &expr);
  virtual void visit (AST::FieldAccessExpr &expr);
  virtual void visit (AST::ClosureExprInner &expr);
  virtual void visit (AST::BlockExpr &expr);
  virtual void visit (AST::ClosureExprInnerTyped &expr);
  virtual void visit (AST::ContinueExpr &expr);
  virtual void visit (AST::BreakExpr &expr);
  virtual void visit (AST::RangeFromToExpr &expr);
  virtual void visit (AST::RangeFromExpr &expr);
  virtual void visit (AST::RangeToExpr &expr);
  virtual void visit (AST::RangeFullExpr &expr);
  virtual void visit (AST::RangeFromToInclExpr &expr);
  virtual void visit (AST::RangeToInclExpr &expr);
  virtual void visit (AST::ReturnExpr &expr);
  virtual void visit (AST::UnsafeBlockExpr &expr);
  virtual void visit (AST::LoopExpr &expr);
  virtual void visit (AST::WhileLoopExpr &expr);
  virtual void visit (AST::WhileLetLoopExpr &expr);
  virtual void visit (AST::ForLoopExpr &expr);
  virtual void visit (AST::IfExpr &expr);
  virtual void visit (AST::IfExprConseqElse &expr);
  virtual void visit (AST::IfExprConseqIf &expr);
  virtual void visit (AST::IfExprConseqIfLet &expr);
  virtual void visit (AST::IfLetExpr &expr);
  virtual void visit (AST::IfLetExprConseqElse &expr);
  virtual void visit (AST::IfLetExprConseqIf &expr);
  virtual void visit (AST::IfLetExprConseqIfLet &expr);
  virtual void visit (AST::MatchExpr &expr);
  virtual void visit (AST::AwaitExpr &expr);
  virtual void visit (AST::AsyncBlockExpr &expr);
  virtual void visit (AST::TypeParam &param);
  virtual void visit (AST::LifetimeWhereClauseItem &item);
  virtual void visit (AST::TypeBoundWhereClauseItem &item);
  virtual void visit (AST::Method &method);
  virtual void visit (AST::Module &module);
  virtual void visit (AST::ExternCrate &crate);
  virtual void visit (AST::UseTreeGlob &use_tree);
  virtual void visit (AST::UseTreeList &use_tree);
  virtual void visit (AST::UseTreeRebind &use_tree);
  virtual void visit (AST::UseDeclaration &use_decl);
  virtual void visit (AST::Function &function);
  virtual void visit (AST::TypeAlias &type_alias);
  virtual void visit (AST::StructStruct &struct_item);
  virtual void visit (AST::TupleStruct &tuple_struct);
  virtual void visit (AST::EnumItem &item);
  virtual void visit (AST::EnumItemTuple &item);
  virtual void visit (AST::EnumItemStruct &item);
  virtual void visit (AST::EnumItemDiscriminant &item);
  virtual void visit (AST::Enum &enum_item);
  virtual void visit (AST::Union &union_item);
  virtual void visit (AST::ConstantItem &const_item);
  virtual void visit (AST::StaticItem &static_item);
  virtual void visit (AST::TraitItemFunc &item);
  virtual void visit (AST::TraitItemMethod &item);
  virtual void visit (AST::TraitItemConst &item);
  virtual void visit (AST::TraitItemType &item);
  virtual void visit (AST::Trait &trait);
  virtual void visit (AST::InherentImpl &impl);
  virtual void visit (AST::TraitImpl &impl);
  virtual void visit (AST::ExternalStaticItem &item);
  virtual void visit (AST::ExternalFunctionItem &item);
  virtual void visit (AST::ExternBlock &block);
  virtual void visit (AST::MacroMatchFragment &match);
  virtual void visit (AST::MacroMatchRepetition &match);
  virtual void visit (AST::MacroMatcher &matcher);
  virtual void visit (AST::MacroRulesDefinition &rules_def);
  virtual void visit (AST::MacroInvocation &macro_invoc);
  virtual void visit (AST::MetaItemPath &meta_item);
  virtual void visit (AST::MetaItemSeq &meta_item);
  virtual void visit (AST::MetaWord &meta_item);
  virtual void visit (AST::MetaNameValueStr &meta_item);
  virtual void visit (AST::MetaListPaths &meta_item);
  virtual void visit (AST::MetaListNameValueStr &meta_item);
  virtual void visit (AST::LiteralPattern &pattern);
  virtual void visit (AST::IdentifierPattern &pattern);
  virtual void visit (AST::WildcardPattern &pattern);
  virtual void visit (AST::RangePatternBoundLiteral &bound);
  virtual void visit (AST::RangePatternBoundPath &bound);
  virtual void visit (AST::RangePatternBoundQualPath &bound);
  virtual void visit (AST::RangePattern &pattern);
  virtual void visit (AST::ReferencePattern &pattern);
  virtual void visit (AST::StructPatternFieldTuplePat &field);
  virtual void visit (AST::StructPatternFieldIdentPat &field);
  virtual void visit (AST::StructPatternFieldIdent &field);
  virtual void visit (AST::StructPattern &pattern);
  virtual void visit (AST::TupleStructItemsNoRange &tuple_items);
  virtual void visit (AST::TupleStructItemsRange &tuple_items);
  virtual void visit (AST::TupleStructPattern &pattern);
  virtual void visit (AST::TuplePatternItemsMultiple &tuple_items);
  virtual void visit (AST::TuplePatternItemsRanged &tuple_items);
  virtual void visit (AST::TuplePattern &pattern);
  virtual void visit (AST::GroupedPattern &pattern);
  virtual void visit (AST::SlicePattern &pattern);
  virtual void visit (AST::AltPattern &pattern);
  virtual void visit (AST::EmptyStmt &stmt);
  virtual void visit (AST::LetStmt &stmt);
  virtual void visit (AST::ExprStmtWithoutBlock &stmt);
  virtual void visit (AST::ExprStmtWithBlock &stmt);
  virtual void visit (AST::TraitBound &bound);
  virtual void visit (AST::ImplTraitType &type);
  virtual void visit (AST::TraitObjectType &type);
  virtual void visit (AST::ParenthesisedType &type);
  virtual void visit (AST::ImplTraitTypeOneBound &type);
  virtual void visit (AST::TraitObjectTypeOneBound &type);
  virtual void visit (AST::TupleType &type);
  virtual void visit (AST::NeverType &type);
  virtual void visit (AST::RawPointerType &type);
  virtual void visit (AST::ReferenceType &type);
  virtual void visit (AST::ArrayType &type);
  virtual void visit (AST::SliceType &type);
  virtual void visit (AST::InferredType &type);
  virtual void visit (AST::BareFunctionType &type);
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_EARLY_NAME_RESOLVER_H
