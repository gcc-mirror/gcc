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

#include "rust-expand-visitor.h"

namespace Rust {

/* Expand all of the macro invocations currently contained in a crate */
void
ExpandVisitor::go (AST::Crate &crate)
{
  expander.push_context (MacroExpander::ContextType::ITEM);

  // expand attributes recursively and strip items if required
  //  AttrVisitor attr_visitor (*this);
  auto &items = crate.items;
  for (auto it = items.begin (); it != items.end ();)
    {
      auto &item = *it;
      item->accept_vis (*this);

      auto fragment = expander.take_expanded_fragment ();
      if (fragment.should_expand ())
	{
	  // Remove the current expanded invocation
	  it = items.erase (it);
	  for (auto &node : fragment.get_nodes ())
	    {
	      it = items.insert (it, node.take_item ());
	      it++;
	    }
	}
      else
	it++;
    }

  expander.pop_context ();
}

void
ExpandVisitor::maybe_expand_expr (std::unique_ptr<AST::Expr> &expr)
{
  // FIXME: ARTHUR: Why isn't there a ContextType::EXPR? We can only
  // reach `parse_expr` once in MacroExpander::transcribe_rule(), but it
  // would make things clearer wouldn't it?
  expr->accept_vis (*this);

  auto final_fragment = expander.take_expanded_fragment ();
  if (final_fragment.should_expand ()
      && final_fragment.is_expression_fragment ())
    expr = final_fragment.take_expression_fragment ();
}

void
ExpandVisitor::maybe_expand_type (std::unique_ptr<AST::Type> &type)
{
  expander.push_context (MacroExpander::ContextType::TYPE);

  type->accept_vis (*this);
  auto final_fragment = expander.take_expanded_fragment ();
  if (final_fragment.should_expand () && final_fragment.is_type_fragment ())
    type = final_fragment.take_type_fragment ();

  expander.pop_context ();
}

// FIXME: Can this be refactored into a `scoped` method? Which takes a
// ContextType as parameter and a lambda? And maybe just an std::vector<T>&?
void
ExpandVisitor::expand_struct_fields (std::vector<AST::StructField> &fields)
{
  for (auto &field : fields)
    maybe_expand_type (field.get_field_type ());
}

void
ExpandVisitor::expand_tuple_fields (std::vector<AST::TupleField> &fields)
{
  for (auto &field : fields)
    maybe_expand_type (field.get_field_type ());
}

// FIXME: This can definitely be refactored with the method above
void
ExpandVisitor::expand_function_params (std::vector<AST::FunctionParam> &params)
{
  for (auto &param : params)
    maybe_expand_type (param.get_type ());
}

void
ExpandVisitor::expand_generic_args (AST::GenericArgs &args)
{
  for (auto &arg : args.get_generic_args ())
    {
      switch (arg.get_kind ())
	{
	case AST::GenericArg::Kind::Type:
	  maybe_expand_type (arg.get_type ());
	  break;
	case AST::GenericArg::Kind::Const:
	  maybe_expand_expr (arg.get_expression ());
	  break;
	default:
	  break;
	  // FIXME: Figure out what to do here if there is ambiguity. Since the
	  // resolver comes after the expansion, we need to figure out a way to
	  // strip ambiguous values here
	  // TODO: ARTHUR: Probably add a `mark_as_strip` method to `GenericArg`
	  // or something. This would clean up this whole thing
	}
    }

  // FIXME: Can we have macro invocations in generic type bindings?
  // expand binding args - strip sub-types only
  // FIXME: ARTHUR: This needs a test! Foo<Item = macro!()>
  for (auto &binding : args.get_binding_args ())
    maybe_expand_type (binding.get_type ());
}

void
ExpandVisitor::expand_qualified_path_type (AST::QualifiedPathType &path_type)
{
  maybe_expand_type (path_type.get_type ());

  // FIXME: ARTHUR: Can we do macro expansion in there? Needs a test!
  if (path_type.has_as_clause ())
    path_type.get_as_type_path ().accept_vis (*this);
}

void
ExpandVisitor::expand_closure_params (std::vector<AST::ClosureParam> &params)
{
  for (auto &param : params)
    if (param.has_type_given ())
      maybe_expand_type (param.get_type ());
}

void
ExpandVisitor::expand_self_param (AST::SelfParam &self_param)
{
  if (self_param.has_type ())
    maybe_expand_type (self_param.get_type ());

  /* TODO: maybe check for invariants being violated - e.g. both type and
   * lifetime? */
}

void
ExpandVisitor::expand_where_clause (AST::WhereClause &where_clause)
{
  for (auto &item : where_clause.get_items ())
    visit (item);
}

void
ExpandVisitor::expand_trait_function_decl (AST::TraitFunctionDecl &decl)
{
  // just expand sub-stuff - can't actually strip generic params themselves
  for (auto &param : decl.get_generic_params ())
    visit (param);

  /* strip function parameters if required - this is specifically
   * allowed by spec */
  expand_function_params (decl.get_function_params ());

  if (decl.has_return_type ())
    maybe_expand_type (decl.get_return_type ());

  if (decl.has_where_clause ())
    expand_where_clause (decl.get_where_clause ());
}

void
ExpandVisitor::expand_trait_method_decl (AST::TraitMethodDecl &decl)
{
  for (auto &param : decl.get_generic_params ())
    visit (param);

  /* assuming you can't strip self param - wouldn't be a method
   * anymore. spec allows outer attrs on self param, but doesn't
   * specify whether cfg is used. */
  expand_self_param (decl.get_self_param ());

  /* strip function parameters if required - this is specifically
   * allowed by spec */
  expand_function_params (decl.get_function_params ());

  if (decl.has_return_type ())

    maybe_expand_type (decl.get_return_type ());

  if (decl.has_where_clause ())
    expand_where_clause (decl.get_where_clause ());
}

void
ExpandVisitor::visit (AST::Token &)
{}

void
ExpandVisitor::visit (AST::DelimTokenTree &)
{}

void
ExpandVisitor::visit (AST::AttrInputMetaItemContainer &)
{}

void
ExpandVisitor::visit (AST::IdentifierExpr &ident_expr)
{}

void
ExpandVisitor::visit (AST::Lifetime &)
{}

void
ExpandVisitor::visit (AST::LifetimeParam &)
{}

void
ExpandVisitor::visit (AST::ConstGenericParam &)
{}

void
ExpandVisitor::visit (AST::MacroInvocation &macro_invoc)
{
  // TODO: Can we do the AST fragment replacing here? Probably not, right?
  expander.expand_invoc (macro_invoc, macro_invoc.has_semicolon ());
}

void
ExpandVisitor::visit (AST::PathInExpression &path)
{
  for (auto &segment : path.get_segments ())
    if (segment.has_generic_args ())
      expand_generic_args (segment.get_generic_args ());
}

void
ExpandVisitor::visit (AST::TypePathSegment &)
{}

void
ExpandVisitor::visit (AST::TypePathSegmentGeneric &segment)
{}

void
ExpandVisitor::visit (AST::TypePathSegmentFunction &segment)
{
  auto &type_path_function = segment.get_type_path_function ();

  for (auto &type : type_path_function.get_params ())
    visit (type);

  if (type_path_function.has_return_type ())
    maybe_expand_type (type_path_function.get_return_type ());
}

void
ExpandVisitor::visit (AST::TypePath &path)
{
  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
ExpandVisitor::visit (AST::QualifiedPathInExpression &path)
{
  expand_qualified_path_type (path.get_qualified_path_type ());

  for (auto &segment : path.get_segments ())
    if (segment.has_generic_args ())
      expand_generic_args (segment.get_generic_args ());
}

void
ExpandVisitor::visit (AST::QualifiedPathInType &path)
{
  expand_qualified_path_type (path.get_qualified_path_type ());

  // this shouldn't strip any segments, but can strip inside them
  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
ExpandVisitor::visit (AST::LiteralExpr &expr)
{}

void
ExpandVisitor::visit (AST::AttrInputLiteral &)
{}

void
ExpandVisitor::visit (AST::MetaItemLitExpr &)
{}

void
ExpandVisitor::visit (AST::MetaItemPathLit &)
{}

void
ExpandVisitor::visit (AST::BorrowExpr &expr)
{
  visit (expr.get_borrowed_expr ());
}

void
ExpandVisitor::visit (AST::DereferenceExpr &expr)
{
  visit (expr.get_dereferenced_expr ());
}

void
ExpandVisitor::visit (AST::ErrorPropagationExpr &expr)
{
  visit (expr.get_propagating_expr ());
}

void
ExpandVisitor::visit (AST::NegationExpr &expr)
{
  visit (expr.get_negated_expr ());
}

void
ExpandVisitor::visit (AST::ArithmeticOrLogicalExpr &expr)
{
  maybe_expand_expr (expr.get_left_expr ());
  maybe_expand_expr (expr.get_right_expr ());
}

void
ExpandVisitor::visit (AST::ComparisonExpr &expr)
{
  maybe_expand_expr (expr.get_left_expr ());
  maybe_expand_expr (expr.get_right_expr ());
}

void
ExpandVisitor::visit (AST::LazyBooleanExpr &expr)
{
  maybe_expand_expr (expr.get_left_expr ());
  maybe_expand_expr (expr.get_right_expr ());
}

void
ExpandVisitor::visit (AST::TypeCastExpr &expr)
{
  visit (expr.get_casted_expr ());

  visit (expr.get_type_to_cast_to ());
}

void
ExpandVisitor::visit (AST::AssignmentExpr &expr)
{
  maybe_expand_expr (expr.get_left_expr ());
  maybe_expand_expr (expr.get_right_expr ());
}

void
ExpandVisitor::visit (AST::CompoundAssignmentExpr &expr)
{
  maybe_expand_expr (expr.get_left_expr ());
  maybe_expand_expr (expr.get_right_expr ());
}

void
ExpandVisitor::visit (AST::GroupedExpr &expr)
{
  visit (expr.get_expr_in_parens ());
}

void
ExpandVisitor::visit (AST::ArrayElemsValues &elems)
{
  for (auto &elem : elems.get_values ())
    visit (elem);
}

void
ExpandVisitor::visit (AST::ArrayElemsCopied &elems)
{
  visit (elems.get_elem_to_copy ());
  visit (elems.get_num_copies ());
}

void
ExpandVisitor::visit (AST::ArrayExpr &expr)
{
  visit (expr.get_array_elems ());
}

void
ExpandVisitor::visit (AST::ArrayIndexExpr &expr)
{
  visit (expr.get_array_expr ());
  visit (expr.get_index_expr ());
}

void
ExpandVisitor::visit (AST::TupleExpr &expr)
{
  for (auto &element : expr.get_tuple_elems ())
    visit (element);
}

void
ExpandVisitor::visit (AST::TupleIndexExpr &expr)
{
  visit (expr.get_tuple_expr ());

  // We can't have macro invocations for tuple indexes, right? Need a test!
}

void
ExpandVisitor::visit (AST::StructExprStruct &expr)
{}

void
ExpandVisitor::visit (AST::StructExprFieldIdentifier &)
{}

void
ExpandVisitor::visit (AST::StructExprFieldIdentifierValue &field)
{
  visit (field.get_value ());
}

void
ExpandVisitor::visit (AST::StructExprFieldIndexValue &field)
{
  visit (field.get_value ());
}

void
ExpandVisitor::visit (AST::StructExprStructFields &expr)
{
  for (auto &field : expr.get_fields ())
    visit (field);

  if (expr.has_struct_base ())
    visit (expr.get_struct_base ().get_base_struct ());
}

void
ExpandVisitor::visit (AST::StructExprStructBase &expr)
{
  visit (expr.get_struct_base ().get_base_struct ());
}

void
ExpandVisitor::visit (AST::CallExpr &expr)
{
  visit (expr.get_function_expr ());

  for (auto &param : expr.get_params ())
    maybe_expand_expr (param);
}

void
ExpandVisitor::visit (AST::MethodCallExpr &expr)
{
  visit (expr.get_receiver_expr ());

  for (auto &param : expr.get_params ())
    maybe_expand_expr (param);
}

void
ExpandVisitor::visit (AST::FieldAccessExpr &expr)
{
  visit (expr.get_receiver_expr ());
}

void
ExpandVisitor::visit (AST::ClosureExprInner &expr)
{
  expand_closure_params (expr.get_params ());

  visit (expr.get_definition_expr ());
}

void
ExpandVisitor::visit (AST::BlockExpr &expr)
{
  std::function<std::unique_ptr<AST::Stmt> (AST::SingleASTNode)> extractor
    = [] (AST::SingleASTNode node) { return node.take_stmt (); };

  expand_macro_children (MacroExpander::ContextType::BLOCK,
			 expr.get_statements (), extractor);

  expander.push_context (MacroExpander::ContextType::BLOCK);

  if (expr.has_tail_expr ())
    maybe_expand_expr (expr.get_tail_expr ());

  expander.pop_context ();
}

void
ExpandVisitor::visit (AST::ClosureExprInnerTyped &expr)
{
  expand_closure_params (expr.get_params ());

  maybe_expand_type (expr.get_return_type ());

  visit (expr.get_definition_block ());
}

void
ExpandVisitor::visit (AST::ContinueExpr &expr)
{}

void
ExpandVisitor::visit (AST::BreakExpr &expr)
{
  if (expr.has_break_expr ())
    visit (expr.get_break_expr ());
}

void
ExpandVisitor::visit (AST::RangeFromToExpr &expr)
{
  visit (expr.get_from_expr ());
  visit (expr.get_to_expr ());
}

void
ExpandVisitor::visit (AST::RangeFromExpr &expr)
{
  visit (expr.get_from_expr ());
}

void
ExpandVisitor::visit (AST::RangeToExpr &expr)
{
  visit (expr.get_to_expr ());
}

void
ExpandVisitor::visit (AST::RangeFullExpr &)
{}

void
ExpandVisitor::visit (AST::RangeFromToInclExpr &expr)
{
  visit (expr.get_from_expr ());
  visit (expr.get_to_expr ());
}

void
ExpandVisitor::visit (AST::RangeToInclExpr &expr)
{
  visit (expr.get_to_expr ());
}

void
ExpandVisitor::visit (AST::ReturnExpr &expr)
{
  if (expr.has_returned_expr ())
    visit (expr.get_returned_expr ());
}

void
ExpandVisitor::visit (AST::UnsafeBlockExpr &expr)
{
  visit (expr.get_block_expr ());
}

void
ExpandVisitor::visit (AST::LoopExpr &expr)
{
  visit (expr.get_loop_block ());
}

void
ExpandVisitor::visit (AST::WhileLoopExpr &expr)
{
  visit (expr.get_predicate_expr ());
  visit (expr.get_loop_block ());
}

void
ExpandVisitor::visit (AST::WhileLetLoopExpr &expr)
{
  for (auto &pattern : expr.get_patterns ())
    visit (pattern);

  visit (expr.get_scrutinee_expr ());
  visit (expr.get_loop_block ());
}

void
ExpandVisitor::visit (AST::ForLoopExpr &expr)
{
  visit (expr.get_pattern ());
  visit (expr.get_iterator_expr ());
  visit (expr.get_loop_block ());
}

void
ExpandVisitor::visit (AST::IfExpr &expr)
{
  maybe_expand_expr (expr.get_condition_expr ());

  visit (expr.get_if_block ());
}

void
ExpandVisitor::visit (AST::IfExprConseqElse &expr)
{
  maybe_expand_expr (expr.get_condition_expr ());

  visit (expr.get_if_block ());
  visit (expr.get_else_block ());
}

void
ExpandVisitor::visit (AST::IfExprConseqIf &expr)
{
  maybe_expand_expr (expr.get_condition_expr ());

  visit (expr.get_if_block ());
  visit (expr.get_conseq_if_expr ());
}

void
ExpandVisitor::visit (AST::IfExprConseqIfLet &expr)
{
  maybe_expand_expr (expr.get_condition_expr ());

  visit (expr.get_if_block ());
  visit (expr.get_conseq_if_let_expr ());
}

void
ExpandVisitor::visit (AST::IfLetExpr &expr)
{
  maybe_expand_expr (expr.get_value_expr ());

  visit (expr.get_if_block ());
}

void
ExpandVisitor::visit (AST::IfLetExprConseqElse &expr)
{
  maybe_expand_expr (expr.get_value_expr ());

  visit (expr.get_if_block ());
  visit (expr.get_else_block ());
}

void
ExpandVisitor::visit (AST::IfLetExprConseqIf &expr)
{
  maybe_expand_expr (expr.get_value_expr ());

  visit (expr.get_if_block ());
  visit (expr.get_conseq_if_expr ());
}

void
ExpandVisitor::visit (AST::IfLetExprConseqIfLet &expr)
{
  maybe_expand_expr (expr.get_value_expr ());

  visit (expr.get_if_block ());
  visit (expr.get_conseq_if_let_expr ());
}

void
ExpandVisitor::visit (AST::MatchExpr &expr)
{
  visit (expr.get_scrutinee_expr ());

  for (auto &match_case : expr.get_match_cases ())
    {
      auto &arm = match_case.get_arm ();

      for (auto &pattern : arm.get_patterns ())
	visit (pattern);

      if (arm.has_match_arm_guard ())
	visit (arm.get_guard_expr ());

      visit (match_case.get_expr ());
    }
}

void
ExpandVisitor::visit (AST::AwaitExpr &expr)
{
  visit (expr.get_awaited_expr ());
}

void
ExpandVisitor::visit (AST::AsyncBlockExpr &expr)
{
  visit (expr.get_block_expr ());
}

void
ExpandVisitor::visit (AST::TypeParam &param)
{
  for (auto &bound : param.get_type_param_bounds ())
    visit (bound);

  if (param.has_type ())
    maybe_expand_type (param.get_type ());
}

void
ExpandVisitor::visit (AST::LifetimeWhereClauseItem &)
{}

void
ExpandVisitor::visit (AST::TypeBoundWhereClauseItem &item)
{
  maybe_expand_type (item.get_type ());

  for (auto &bound : item.get_type_param_bounds ())
    visit (bound);
}

void
ExpandVisitor::visit (AST::Method &method)
{
  for (auto &param : method.get_generic_params ())
    visit (param);

  expand_self_param (method.get_self_param ());
  expand_function_params (method.get_function_params ());

  if (method.has_return_type ())
    visit (method.get_return_type ());

  if (method.has_where_clause ())
    expand_where_clause (method.get_where_clause ());

  visit (method.get_definition ());
}

void
ExpandVisitor::visit (AST::Module &module)
{
  if (module.get_kind () == AST::Module::ModuleKind::LOADED)
    {
      visit_inner_attrs (module);
      for (auto &item : module.get_items ())
	visit (item);
    }
}

void
ExpandVisitor::visit (AST::ExternCrate &crate)
{}

void
ExpandVisitor::visit (AST::UseTreeGlob &)
{}

void
ExpandVisitor::visit (AST::UseTreeList &)
{}

void
ExpandVisitor::visit (AST::UseTreeRebind &)
{}

void
ExpandVisitor::visit (AST::UseDeclaration &use_decl)
{}

void
ExpandVisitor::visit (AST::Function &function)
{
  visit_inner_using_attrs (function,
			   function.get_definition ()->get_inner_attrs ());
  for (auto &param : function.get_generic_params ())
    visit (param);

  expand_function_params (function.get_function_params ());

  if (function.has_return_type ())
    maybe_expand_type (function.get_return_type ());

  if (function.has_where_clause ())
    expand_where_clause (function.get_where_clause ());

  visit (function.get_definition ());
}

void
ExpandVisitor::visit (AST::TypeAlias &type_alias)
{
  visit (type_alias.get_type_aliased ());
}

void
ExpandVisitor::visit (AST::StructStruct &struct_item)
{
  visit_attrs_with_derive (struct_item);
  for (auto &generic : struct_item.get_generic_params ())
    visit (generic);

  if (struct_item.has_where_clause ())
    expand_where_clause (struct_item.get_where_clause ());

  expand_struct_fields (struct_item.get_fields ());
}

void
ExpandVisitor::visit (AST::TupleStruct &tuple_struct)
{
  visit_attrs_with_derive (tuple_struct);
  for (auto &generic : tuple_struct.get_generic_params ())
    visit (generic);

  if (tuple_struct.has_where_clause ())
    expand_where_clause (tuple_struct.get_where_clause ());

  expand_tuple_fields (tuple_struct.get_fields ());
}

void
ExpandVisitor::visit (AST::EnumItem &item)
{}

void
ExpandVisitor::visit (AST::EnumItemTuple &item)
{
  expand_tuple_fields (item.get_tuple_fields ());
}

void
ExpandVisitor::visit (AST::EnumItemStruct &item)
{
  expand_struct_fields (item.get_struct_fields ());
}

void
ExpandVisitor::visit (AST::EnumItemDiscriminant &item)
{
  visit (item.get_expr ());
}

void
ExpandVisitor::visit (AST::Enum &enum_item)
{
  visit_attrs_with_derive (enum_item);
  for (auto &generic : enum_item.get_generic_params ())
    visit (generic);

  for (auto &variant : enum_item.get_variants ())
    visit (variant);
}

void
ExpandVisitor::visit (AST::Union &union_item)
{
  visit_attrs_with_derive (union_item);
  for (auto &generic : union_item.get_generic_params ())
    visit (generic);

  expand_struct_fields (union_item.get_variants ());
}

void
ExpandVisitor::visit (AST::ConstantItem &const_item)
{
  maybe_expand_type (const_item.get_type ());

  visit (const_item.get_expr ());
}

void
ExpandVisitor::visit (AST::StaticItem &static_item)
{
  maybe_expand_type (static_item.get_type ());

  visit (static_item.get_expr ());
}

void
ExpandVisitor::visit (AST::TraitItemFunc &item)
{
  expand_trait_function_decl (item.get_trait_function_decl ());

  if (item.has_definition ())
    visit (item.get_definition ());
}

void
ExpandVisitor::visit (AST::TraitItemMethod &item)
{
  expand_trait_method_decl (item.get_trait_method_decl ());

  if (item.has_definition ())
    visit (item.get_definition ());
}

void
ExpandVisitor::visit (AST::TraitItemConst &const_item)
{
  maybe_expand_type (const_item.get_type ());

  if (const_item.has_expr ())
    visit (const_item.get_expr ());
}

void
ExpandVisitor::visit (AST::TraitItemType &item)
{
  for (auto &type : item.get_type_param_bounds ())
    visit (type);
}

void
ExpandVisitor::visit (AST::Trait &trait)
{
  for (auto &generic : trait.get_generic_params ())
    visit (generic);

  for (auto &bound : trait.get_type_param_bounds ())
    visit (bound);

  if (trait.has_where_clause ())
    expand_where_clause (trait.get_where_clause ());

  expander.push_context (MacroExpander::ContextType::TRAIT);

  std::function<std::unique_ptr<AST::TraitItem> (AST::SingleASTNode)> extractor
    = [] (AST::SingleASTNode node) { return node.take_trait_item (); };

  expand_macro_children (MacroExpander::ContextType::TRAIT,
			 trait.get_trait_items (), extractor);

  expander.pop_context ();
}

void
ExpandVisitor::visit (AST::InherentImpl &impl)
{
  visit_inner_attrs (impl);
  // just expand sub-stuff - can't actually strip generic params themselves
  for (auto &generic : impl.get_generic_params ())
    visit (generic);

  // FIXME: Is that correct? How do we test that?
  expander.push_context (MacroExpander::ContextType::ITEM);

  maybe_expand_type (impl.get_type ());

  expander.pop_context ();

  if (impl.has_where_clause ())
    expand_where_clause (impl.get_where_clause ());

  std::function<std::unique_ptr<AST::InherentImplItem> (AST::SingleASTNode)>
    extractor = [] (AST::SingleASTNode node) { return node.take_impl_item (); };

  expand_macro_children (MacroExpander::ContextType::IMPL,
			 impl.get_impl_items (), extractor);
}

void
ExpandVisitor::visit (AST::TraitImpl &impl)
{
  visit_inner_attrs (impl);
  // just expand sub-stuff - can't actually strip generic params themselves
  for (auto &param : impl.get_generic_params ())
    visit (param);

  // FIXME: Is that correct? How do we test that?
  expander.push_context (MacroExpander::ContextType::ITEM);

  maybe_expand_type (impl.get_type ());

  expander.pop_context ();

  visit (impl.get_trait_path ());

  if (impl.has_where_clause ())
    expand_where_clause (impl.get_where_clause ());

  std::function<std::unique_ptr<AST::TraitImplItem> (AST::SingleASTNode)>
    extractor
    = [] (AST::SingleASTNode node) { return node.take_trait_impl_item (); };

  expand_macro_children (MacroExpander::ContextType::TRAIT_IMPL,
			 impl.get_impl_items (), extractor);
}

void
ExpandVisitor::visit (AST::ExternalTypeItem &item)
{}

void
ExpandVisitor::visit (AST::ExternalStaticItem &static_item)
{
  maybe_expand_type (static_item.get_type ());
}

void
ExpandVisitor::visit (AST::ExternalFunctionItem &item)
{
  for (auto &param : item.get_generic_params ())
    visit (param);

  // FIXME: Should this work? What is the difference between NamedFunctionParam
  // and FunctionParam?
  // expand_function_params (item.get_function_params ());

  for (auto &param : item.get_function_params ())
    maybe_expand_type (param.get_type ());

  if (item.has_return_type ())
    maybe_expand_type (item.get_return_type ());

  if (item.has_where_clause ())
    expand_where_clause (item.get_where_clause ());
}

void
ExpandVisitor::visit (AST::ExternBlock &block)
{
  visit_inner_attrs (block);
  std::function<std::unique_ptr<AST::ExternalItem> (AST::SingleASTNode)>
    extractor
    = [] (AST::SingleASTNode node) { return node.take_external_item (); };

  expand_macro_children (MacroExpander::ContextType::EXTERN,
			 block.get_extern_items (), extractor);
}

// I don't think it would be possible to strip macros without expansion
void
ExpandVisitor::visit (AST::MacroMatchFragment &)
{}

void
ExpandVisitor::visit (AST::MacroMatchRepetition &)
{}

void
ExpandVisitor::visit (AST::MacroMatcher &)
{}

void
ExpandVisitor::visit (AST::MacroRulesDefinition &rules_def)
{}

void
ExpandVisitor::visit (AST::MetaItemPath &)
{}

void
ExpandVisitor::visit (AST::MetaItemSeq &)
{}

void
ExpandVisitor::visit (AST::MetaWord &)
{}

void
ExpandVisitor::visit (AST::MetaNameValueStr &)
{}

void
ExpandVisitor::visit (AST::MetaListPaths &)
{}

void
ExpandVisitor::visit (AST::MetaListNameValueStr &)
{}

void
ExpandVisitor::visit (AST::LiteralPattern &)
{}

void
ExpandVisitor::visit (AST::IdentifierPattern &pattern)
{
  if (pattern.has_pattern_to_bind ())
    visit (pattern.get_pattern_to_bind ());
}

void
ExpandVisitor::visit (AST::WildcardPattern &)
{}

void
ExpandVisitor::visit (AST::RestPattern &)
{}

void
ExpandVisitor::visit (AST::RangePatternBoundLiteral &)
{}

void
ExpandVisitor::visit (AST::RangePatternBoundPath &bound)
{
  visit (bound.get_path ());
}

void
ExpandVisitor::visit (AST::RangePatternBoundQualPath &bound)
{
  visit (bound.get_qualified_path ());
}

void
ExpandVisitor::visit (AST::RangePattern &pattern)
{
  visit (pattern.get_lower_bound ());
  visit (pattern.get_upper_bound ());
}

void
ExpandVisitor::visit (AST::ReferencePattern &pattern)
{
  visit (pattern.get_referenced_pattern ());
}

void
ExpandVisitor::visit (AST::StructPatternFieldTuplePat &field)
{
  visit (field.get_index_pattern ());
}

void
ExpandVisitor::visit (AST::StructPatternFieldIdentPat &field)
{
  visit (field.get_ident_pattern ());
}

void
ExpandVisitor::visit (AST::StructPatternFieldIdent &field)
{}

void
ExpandVisitor::visit (AST::StructPattern &pattern)
{
  visit (pattern.get_path ());

  for (auto &inner :
       pattern.get_struct_pattern_elems ().get_struct_pattern_fields ())
    visit (inner);
}

void
ExpandVisitor::visit (AST::TupleStructItemsNoRange &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    visit (pattern);
}

void
ExpandVisitor::visit (AST::TupleStructItemsRange &tuple_items)
{
  for (auto &lower_pattern : tuple_items.get_lower_patterns ())
    visit (lower_pattern);
  for (auto &upper_pattern : tuple_items.get_upper_patterns ())
    visit (upper_pattern);
}

void
ExpandVisitor::visit (AST::TupleStructPattern &pattern)
{
  visit (pattern.get_path ());

  if (pattern.has_items ())
    visit (pattern.get_items ());
}

void
ExpandVisitor::visit (AST::TuplePatternItemsMultiple &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    visit (pattern);
}

void
ExpandVisitor::visit (AST::TuplePatternItemsRanged &tuple_items)
{
  for (auto &pattern : tuple_items.get_lower_patterns ())
    visit (pattern);
  for (auto &pattern : tuple_items.get_upper_patterns ())
    visit (pattern);
}

void
ExpandVisitor::visit (AST::TuplePattern &pattern)
{
  if (pattern.has_tuple_pattern_items ())
    visit (pattern.get_items ());
}

void
ExpandVisitor::visit (AST::GroupedPattern &pattern)
{
  visit (pattern.get_pattern_in_parens ());
}

void
ExpandVisitor::visit (AST::SlicePattern &pattern)
{
  for (auto &item : pattern.get_items ())
    visit (item);
}

void
ExpandVisitor::visit (AST::AltPattern &pattern)
{
  for (auto &alt : pattern.get_alts ())
    visit (alt);
}

void
ExpandVisitor::visit (AST::EmptyStmt &)
{}

void
ExpandVisitor::visit (AST::LetStmt &stmt)
{
  visit (stmt.get_pattern ());

  if (stmt.has_type ())
    maybe_expand_type (stmt.get_type ());

  if (stmt.has_init_expr ())
    maybe_expand_expr (stmt.get_init_expr ());
}

void
ExpandVisitor::visit (AST::ExprStmtWithoutBlock &stmt)
{
  visit (stmt.get_expr ());
}

void
ExpandVisitor::visit (AST::ExprStmtWithBlock &stmt)
{
  visit (stmt.get_expr ());
}

void
ExpandVisitor::visit (AST::TraitBound &bound)
{
  visit (bound.get_type_path ());
}

void
ExpandVisitor::visit (AST::ImplTraitType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    visit (bound);
}

void
ExpandVisitor::visit (AST::TraitObjectType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    visit (bound);
}

void
ExpandVisitor::visit (AST::ParenthesisedType &type)
{
  visit (type.get_type_in_parens ());
}

void
ExpandVisitor::visit (AST::ImplTraitTypeOneBound &type)
{
  visit (type.get_trait_bound ());
}

void
ExpandVisitor::visit (AST::TraitObjectTypeOneBound &type)
{
  visit (type.get_trait_bound ());
}

void
ExpandVisitor::visit (AST::TupleType &type)
{
  for (auto &elem : type.get_elems ())
    visit (elem);
}

void
ExpandVisitor::visit (AST::NeverType &)
{}

void
ExpandVisitor::visit (AST::RawPointerType &type)
{
  visit (type.get_type_pointed_to ());
}

void
ExpandVisitor::visit (AST::ReferenceType &type)
{
  visit (type.get_type_referenced ());
}

void
ExpandVisitor::visit (AST::ArrayType &type)
{
  visit (type.get_elem_type ());
}

void
ExpandVisitor::visit (AST::SliceType &type)
{
  visit (type.get_elem_type ());
}

void
ExpandVisitor::visit (AST::InferredType &)
{}

void
ExpandVisitor::visit (AST::BareFunctionType &type)
{
  for (auto &param : type.get_function_params ())
    maybe_expand_type (param.get_type ());

  if (type.has_return_type ())
    visit (type.get_return_type ());
}

template <typename T>
void
ExpandVisitor::expand_outer_attribute (T &item, AST::SimplePath &path)
{
  // FIXME: Implement outer attribute expansion
}

template <typename T>
void
ExpandVisitor::visit_outer_attrs (T &item, std::vector<AST::Attribute> &attrs)
{
  for (auto it = attrs.begin (); it != attrs.end (); /* erase => No increment*/)
    {
      auto current = *it;

      it = attrs.erase (it);
      expand_outer_attribute (item, current.get_path ());
    }
}

template <typename T>
void
ExpandVisitor::visit_outer_attrs (T &item)
{
  visit_outer_attrs (item, item.get_outer_attrs ());
}

template <typename T>
void
ExpandVisitor::expand_inner_attribute (T &item, AST::SimplePath &path)
{
  // TODO: Warn about instability ?
  // FIXME: Implement expansion for that particular path
}

template <typename T>
void
ExpandVisitor::visit_inner_using_attrs (T &item,
					std::vector<AST::Attribute> &attrs)
{
  for (auto it = attrs.begin (); it != attrs.end (); /* erase => No increment*/)
    {
      auto current = *it;

      it = attrs.erase (it);
      expand_inner_attribute (item, current.get_path ());
    }
}

template <typename T>
void
ExpandVisitor::visit_inner_attrs (T &item)
{
  visit_inner_using_attrs (item, item.get_inner_attrs ());
}

template <typename T>
void
ExpandVisitor::expand_derive (const T &item,
			      std::unique_ptr<AST::TokenTree> &trait)
{
  // FIXME: Implement expansion for that particular trait
}

template <typename T>
void
ExpandVisitor::expand_derive (const T &item, AST::DelimTokenTree &attr)
{
  // Item is const because even though the tokenstream might be modified, it
  // should appear as the same input for every derive proc macro.
  auto &trees = attr.get_token_trees ();
  if (trees.size () > 2)
    {
      // Skipping begin and end parenthesis
      for (auto it = trees.begin () + 1; it < trees.end () - 1;
	   it += 2 /* Increment + skip comma */)
	{
	  expand_derive (item, *it);
	}
    }
}

template <typename T>
void
ExpandVisitor::visit_attrs_with_derive (T &item)
{
  auto &attrs = item.get_outer_attrs ();
  for (auto it = attrs.begin (); it != attrs.end (); /* erase => No increment*/)
    {
      auto current = *it;

      if (is_derive (current))
	{
	  it = attrs.erase (it);
	  // Downcasting checked in is_derive
	  expand_derive (item, static_cast<AST::DelimTokenTree &> (
				 current.get_attr_input ()));
	}
      else // Skip unknwown
	{
	  it++;
	}
    }
}

bool
ExpandVisitor::is_derive (AST::Attribute &attr)
{
  auto &segments = attr.get_path ().get_segments ();
  return attr.has_attr_input ()
	 && attr.get_attr_input ().get_attr_input_type ()
	      == AST::AttrInput::TOKEN_TREE
	 && !segments.empty () && "derive" == segments[0].get_segment_name ();
}

} // namespace Rust
