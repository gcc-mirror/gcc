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

#ifndef RUST_EXPAND_VISITOR_H
#define RUST_EXPAND_VISITOR_H

#include "rust-ast-visitor.h"
#include "rust-macro-expand.h"
#include "rust-proc-macro.h"

namespace Rust {

/**
 * Whether or not an attribute is a derive attribute
 */
bool
is_derive (AST::Attribute &attr);

/**
 * Whether or not an attribute is builtin
 */
bool
is_builtin (AST::Attribute &attr);

class ExpandVisitor : public AST::DefaultASTVisitor
{
public:
  ExpandVisitor (MacroExpander &expander) : expander (expander) {}

  /* Expand all of the macro invocations currently contained in a crate */
  void go (AST::Crate &crate);

  using AST::DefaultASTVisitor::visit;

  /*
     Maybe expand a macro invocation in lieu of an expression
     expr : Core guidelines R33, this function reseat the pointer.
  */
  void maybe_expand_expr (std::unique_ptr<AST::Expr> &expr);

  /*
     Maybe expand a macro invocation in lieu of a type
     type : Core guidelines R33, this function reseat the pointer.
   */
  void maybe_expand_type (std::unique_ptr<AST::Type> &type);

  /**
   * Expand all macro invocations in lieu of types within a vector of struct
   * fields
   */
  void expand_struct_fields (std::vector<AST::StructField> &fields);

  /**
   * Expand all macro invocations in lieu of types within a vector of tuple
   * fields
   */
  void expand_tuple_fields (std::vector<AST::TupleField> &fields);

  /**
   * Expand all macro invocations in lieu of types within a list of function
   * parameters
   */
  void
  expand_function_params (std::vector<std::unique_ptr<AST::Param>> &params);

  /**
   * Expand all macro invocations in lieu of types within a list of generic
   * arguments
   */
  void expand_generic_args (AST::GenericArgs &args);

  /**
   * Expand a macro invocation in lieu of a qualified path type
   */
  void expand_qualified_path_type (AST::QualifiedPathType &path_type);

  // FIXME: Add documentation
  void expand_closure_params (std::vector<AST::ClosureParam> &params);
  void expand_where_clause (AST::WhereClause &where_clause);

  /**
   * Expand a set of values, erasing them if they are marked for strip, and
   * replacing them with expanded macro nodes if necessary.
   * This function is slightly different from `expand_pointer_allow_strip` as
   * it can only be called in certain expansion contexts - where macro
   * invocations are allowed.
   *
   * @param ctx Context to use for macro expansion
   * @param values Iterable reference over values to replace or erase
   * @param extractor Function to call when replacing values with the content
   * 		of an expanded AST node
   */
  template <typename T, typename U>
  void expand_macro_children (MacroExpander::ContextType ctx, T &values,
			      std::function<U (AST::SingleASTNode)> extractor)
  {
    expander.push_context (ctx);

    expand_macro_children (values, extractor);

    expander.pop_context ();
  }

  /**
   * Same as `expand_macro_children`, but does not push a context. This is
   * useful if you're already pushing the context manually anyway for proc macro
   * expansion, like in `expand_inner_{items, stmts}`
   */
  template <typename T, typename U>
  void expand_macro_children (T &values,
			      std::function<U (AST::SingleASTNode)> extractor)
  {
    for (auto it = values.begin (); it != values.end ();)
      {
	auto &value = *it;

	// Perform expansion
	value->accept_vis (*this);

	auto final_fragment = expander.take_expanded_fragment ();

	// FIXME: Is that correct? It seems *extremely* dodgy
	if (final_fragment.should_expand ())
	  {
	    it = values.erase (it);
	    for (auto &node : final_fragment.get_nodes ())
	      {
		auto new_node = extractor (node);
		if (new_node != nullptr)
		  {
		    it = values.insert (it, std::move (new_node));
		    it++;
		  }
	      }
	  }
	else
	  {
	    ++it;
	  }
      }
  }

  /**
   * Perform in-place expansion of procedural macros and macro invocations for
   * an item container or statement container, such as `AST::Crate`,
   * `AST::Module` or `AST::BlockExpr`. This function will insert the expanded
   * nodes in place, and replace macro invocations with their expanded nodes.
   *
   * @param values Vector of values to mutate in-place and append into
   */
  void expand_inner_items (std::vector<std::unique_ptr<AST::Item>> &values);
  void expand_inner_stmts (AST::BlockExpr &expr);

  // TODO: See if possible to make more specialization for Impl items, Block
  // stmts etc? This could allow us to remove expand_macro_children or at least
  // its extractor parameter
  /**
   * These functions allow to easily visit `std::unique_ptr`s as well as
   * _replace_ them when necessary, e.g when expanding macro invocations in a
   * list of expressions or types. The most generic version of the function will
   * simply call the visitor again on the pointer, but there are two
   * specializations for `std::unique_ptr<Expr>` and `std::unique_ptr<Type>` to
   * enable replacing as well.
   */
  template <typename T> void visit (std::unique_ptr<T> &value)
  {
    value->accept_vis (*this);
  }

  template <typename T> void visit (std::unique_ptr<AST::Expr> &expr)
  {
    maybe_expand_expr (expr);
  }

  template <typename T> void visit (std::unique_ptr<AST::Type> &type)
  {
    maybe_expand_type (type);
  }

  void visit (AST::Crate &crate) override;
  void visit (AST::DelimTokenTree &) override;
  void visit (AST::AttrInputMetaItemContainer &) override;
  void visit (AST::IdentifierExpr &ident_expr) override;
  void visit (AST::LifetimeParam &) override;
  void visit (AST::ConstGenericParam &) override;

  void visit (AST::MacroInvocation &macro_invoc) override;

  void visit (AST::PathInExpression &path) override;
  void visit (AST::TypePathSegmentGeneric &segment) override;
  void visit (AST::TypePathSegmentFunction &segment) override;
  void visit (AST::QualifiedPathInExpression &path) override;
  void visit (AST::QualifiedPathInType &path) override;

  void visit (AST::LiteralExpr &expr) override;
  void visit (AST::AttrInputLiteral &) override;
  void visit (AST::AttrInputMacro &) override;
  void visit (AST::MetaItemLitExpr &) override;
  void visit (AST::MetaItemPathLit &) override;
  void visit (AST::ErrorPropagationExpr &expr) override;
  void visit (AST::ArithmeticOrLogicalExpr &expr) override;
  void visit (AST::ComparisonExpr &expr) override;
  void visit (AST::LazyBooleanExpr &expr) override;
  void visit (AST::AssignmentExpr &expr) override;
  void visit (AST::CompoundAssignmentExpr &expr) override;
  void visit (AST::GroupedExpr &expr) override;
  void visit (AST::StructExprStruct &expr) override;

  void visit (AST::CallExpr &expr) override;
  void visit (AST::MethodCallExpr &expr) override;
  void visit (AST::ClosureExprInner &expr) override;

  void visit (AST::BlockExpr &expr) override;

  void visit (AST::ClosureExprInnerTyped &expr) override;
  void visit (AST::ContinueExpr &expr) override;
  void visit (AST::IfExpr &expr) override;
  void visit (AST::IfExprConseqElse &expr) override;
  void visit (AST::IfLetExpr &expr) override;
  void visit (AST::IfLetExprConseqElse &expr) override;
  void visit (AST::MatchExpr &expr) override;
  void visit (AST::TypeParam &param) override;
  void visit (AST::LifetimeWhereClauseItem &) override;
  void visit (AST::TypeBoundWhereClauseItem &item) override;
  void visit (AST::ExternCrate &crate) override;
  void visit (AST::UseTreeGlob &) override;
  void visit (AST::UseTreeList &) override;
  void visit (AST::UseTreeRebind &) override;
  void visit (AST::UseDeclaration &use_decl) override;
  void visit (AST::Function &function) override;
  void visit (AST::StructStruct &struct_item) override;
  void visit (AST::TupleStruct &tuple_struct) override;
  void visit (AST::EnumItem &item) override;
  void visit (AST::EnumItemTuple &item) override;
  void visit (AST::EnumItemStruct &item) override;
  void visit (AST::EnumItemDiscriminant &item) override;
  void visit (AST::Union &union_item) override;
  void visit (AST::ConstantItem &const_item) override;
  void visit (AST::StaticItem &static_item) override;
  void visit (AST::TraitItemConst &item) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::InherentImpl &impl) override;
  void visit (AST::TraitImpl &impl) override;
  void visit (AST::ExternalTypeItem &item) override;
  void visit (AST::ExternalStaticItem &item) override;
  void visit (AST::ExternBlock &block) override;

  // I don't think it would be possible to strip macros without expansion
  void visit (AST::MacroMatchRepetition &) override;
  void visit (AST::MacroMatcher &) override;
  void visit (AST::MacroRulesDefinition &rules_def) override;
  void visit (AST::MetaItemPath &) override;
  void visit (AST::MetaItemSeq &) override;
  void visit (AST::MetaListPaths &) override;
  void visit (AST::MetaListNameValueStr &) override;
  void visit (AST::StructPatternFieldIdent &field) override;
  void visit (AST::GroupedPattern &pattern) override;

  void visit (AST::LetStmt &stmt) override;
  void visit (AST::ExprStmt &stmt) override;

  void visit (AST::BareFunctionType &type) override;
  void visit (AST::FunctionParam &type) override;
  void visit (AST::SelfParam &type) override;

  template <typename T>
  void expand_inner_attribute (T &item, AST::SimplePath &Path);

  template <typename T>
  void visit_inner_using_attrs (T &item, std::vector<AST::Attribute> &attrs);

  template <typename T> void visit_inner_attrs (T &item);

private:
  MacroExpander &expander;
};

} // namespace Rust

#endif // RUST_EXPAND_VISITOR_H
