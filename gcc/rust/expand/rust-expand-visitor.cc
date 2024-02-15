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

#include "rust-expand-visitor.h"
#include "rust-proc-macro.h"
#include "rust-attributes.h"
#include "rust-ast.h"
#include "rust-type.h"
#include "rust-derive.h"

namespace Rust {

bool
is_builtin (AST::Attribute &attr)
{
  auto &segments = attr.get_path ().get_segments ();
  return !segments.empty ()
	 && !Analysis::BuiltinAttributeMappings::get ()
	       ->lookup_builtin (segments[0].get_segment_name ())
	       .is_error ();
}

/* Expand all of the macro invocations currently contained in a crate */
void
ExpandVisitor::go (AST::Crate &crate)
{
  visit (crate);
}

static std::unique_ptr<AST::Item>
builtin_derive_item (AST::Item &item, const AST::Attribute &derive,
		     BuiltinMacro to_derive)
{
  return AST::DeriveVisitor::derive (item, derive, to_derive);
}

static std::vector<std::unique_ptr<AST::Item>>
derive_item (AST::Item &item, AST::SimplePath &to_derive,
	     MacroExpander &expander)
{
  std::vector<std::unique_ptr<AST::Item>> result;
  auto frag = expander.expand_derive_proc_macro (item, to_derive);
  if (!frag.is_error ())
    {
      for (auto &node : frag.get_nodes ())
	{
	  switch (node.get_kind ())
	    {
	    case AST::SingleASTNode::ITEM:
	      result.push_back (node.take_item ());
	      break;
	    default:
	      rust_unreachable ();
	    }
	}
    }
  return result;
}

static std::vector<std::unique_ptr<AST::Item>>
expand_item_attribute (AST::Item &item, AST::SimplePath &name,
		       MacroExpander &expander)
{
  std::vector<std::unique_ptr<AST::Item>> result;
  auto frag = expander.expand_attribute_proc_macro (item, name);
  if (!frag.is_error ())
    {
      for (auto &node : frag.get_nodes ())
	{
	  switch (node.get_kind ())
	    {
	    case AST::SingleASTNode::ITEM:
	      result.push_back (node.take_item ());
	      break;
	    default:
	      rust_unreachable ();
	    }
	}
    }
  return result;
}

/* Helper function to expand a given attribute on a statement and collect back
 * statements.
 * T should be anything that can be used as a statement accepting outer
 * attributes.
 */
template <typename T>
static std::vector<std::unique_ptr<AST::Stmt>>
expand_stmt_attribute (T &statement, AST::SimplePath &attribute,
		       MacroExpander &expander)
{
  std::vector<std::unique_ptr<AST::Stmt>> result;
  auto frag = expander.expand_attribute_proc_macro (statement, attribute);
  if (!frag.is_error ())
    {
      for (auto &node : frag.get_nodes ())
	{
	  switch (node.get_kind ())
	    {
	    case AST::SingleASTNode::STMT:
	      result.push_back (node.take_stmt ());
	      break;
	    default:
	      rust_unreachable ();
	    }
	}
    }
  return result;
}

void
expand_tail_expr (AST::BlockExpr &block_expr, MacroExpander &expander)
{
  if (block_expr.has_tail_expr ())
    {
      auto tail = block_expr.take_tail_expr ();
      auto attrs = tail->get_outer_attrs ();
      bool changed = false;
      for (auto it = attrs.begin (); it != attrs.end ();)
	{
	  auto current = *it;
	  if (is_builtin (current))
	    {
	      it++;
	    }
	  else
	    {
	      it = attrs.erase (it);
	      changed = true;
	      auto new_stmts
		= expand_stmt_attribute (block_expr, current.get_path (),
					 expander);
	      auto &stmts = block_expr.get_statements ();
	      std::move (new_stmts.begin (), new_stmts.end (),
			 std::inserter (stmts, stmts.end ()));
	    }
	}
      if (changed)
	block_expr.normalize_tail_expr ();
      else
	block_expr.set_tail_expr (std::move (tail));
    }
}

void
ExpandVisitor::expand_inner_items (
  std::vector<std::unique_ptr<AST::Item>> &items)
{
  expander.push_context (MacroExpander::ContextType::ITEM);

  for (auto it = items.begin (); it != items.end (); it++)
    {
      auto &item = *it;
      if (item->has_outer_attrs ())
	{
	  auto &attrs = item->get_outer_attrs ();

	  for (auto attr_it = attrs.begin (); attr_it != attrs.end ();
	       /* erase => No increment*/)
	    {
	      auto current = *attr_it;

	      if (current.is_derive ())
		{
		  current.parse_attr_to_meta_item ();
		  attr_it = attrs.erase (attr_it);
		  // Get traits to derive in the current attribute
		  auto traits_to_derive = current.get_traits_to_derive ();
		  for (auto &to_derive : traits_to_derive)
		    {
		      auto maybe_builtin = MacroBuiltin::builtins.lookup (
			to_derive.get ().as_string ());
		      if (MacroBuiltin::builtins.is_iter_ok (maybe_builtin))
			{
			  auto new_item
			    = builtin_derive_item (*item, current,
						   maybe_builtin->second);
			  // this inserts the derive *before* the item - is it a
			  // problem?
			  it = items.insert (it, std::move (new_item));
			}
		      else
			{
			  auto new_items
			    = derive_item (*item, to_derive, expander);
			  std::move (new_items.begin (), new_items.end (),
				     std::inserter (items, it));
			}
		    }
		}
	      else /* Attribute */
		{
		  if (is_builtin (current))
		    {
		      attr_it++;
		    }
		  else
		    {
		      attr_it = attrs.erase (attr_it);
		      auto new_items
			= expand_item_attribute (*item, current.get_path (),
						 expander);
		      it = items.erase (it);
		      std::move (new_items.begin (), new_items.end (),
				 std::inserter (items, it));
		      // TODO: Improve this ?
		      // item is invalid since it refers to now deleted,
		      // cancel the loop increment and break.
		      it--;
		      break;
		    }
		}
	    }
	}
    }

  std::function<std::unique_ptr<AST::Item> (AST::SingleASTNode)> extractor
    = [] (AST::SingleASTNode node) { return node.take_item (); };

  expand_macro_children (items, extractor);

  expander.pop_context ();
}

void
ExpandVisitor::expand_inner_stmts (AST::BlockExpr &expr)
{
  auto &stmts = expr.get_statements ();
  expander.push_context (MacroExpander::ContextType::STMT);

  for (auto it = stmts.begin (); it != stmts.end (); it++)
    {
      auto &stmt = *it;

      // skip all non-item statements
      if (stmt->get_stmt_kind () != AST::Stmt::Kind::Item)
	continue;

      auto &item = static_cast<AST::Item &> (*stmt.get ());

      if (item.has_outer_attrs ())
	{
	  auto &attrs = item.get_outer_attrs ();

	  for (auto attr_it = attrs.begin (); attr_it != attrs.end ();
	       /* erase => No increment*/)
	    {
	      auto current = *attr_it;

	      if (current.is_derive ())
		{
		  attr_it = attrs.erase (attr_it);
		  // Get traits to derive in the current attribute
		  auto traits_to_derive = current.get_traits_to_derive ();
		  for (auto &to_derive : traits_to_derive)
		    {
		      auto maybe_builtin = MacroBuiltin::builtins.lookup (
			to_derive.get ().as_string ());
		      if (MacroBuiltin::builtins.is_iter_ok (maybe_builtin))
			{
			  auto new_item
			    = builtin_derive_item (item, current,
						   maybe_builtin->second);
			  // this inserts the derive *before* the item - is it a
			  // problem?
			  it = stmts.insert (it, std::move (new_item));
			}
		      else
			{
			  auto new_items
			    = derive_item (item, to_derive, expander);
			  std::move (new_items.begin (), new_items.end (),
				     std::inserter (stmts, it));
			}
		    }
		}
	      else /* Attribute */
		{
		  if (is_builtin (current))
		    {
		      attr_it++;
		    }
		  else
		    {
		      attr_it = attrs.erase (attr_it);
		      auto new_items
			= expand_stmt_attribute (item, current.get_path (),
						 expander);
		      it = stmts.erase (it);
		      std::move (new_items.begin (), new_items.end (),
				 std::inserter (stmts, it));
		      // TODO: Improve this ?
		      // item is invalid since it refers to now deleted,
		      // cancel the loop increment and break.
		      it--;
		      break;
		    }
		}
	    }
	}
    }

  if (!expr.has_tail_expr ())
    expr.normalize_tail_expr ();

  std::function<std::unique_ptr<AST::Stmt> (AST::SingleASTNode)> extractor
    = [] (AST::SingleASTNode node) { return node.take_stmt (); };

  expand_macro_children (stmts, extractor);

  expander.pop_context ();
}

void
ExpandVisitor::maybe_expand_expr (std::unique_ptr<AST::Expr> &expr)
{
  expander.push_context (MacroExpander::ContextType::EXPR);
  expr->accept_vis (*this);
  expander.pop_context ();

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
    {
      maybe_expand_type (field.get_field_type ());
    }
}

void
ExpandVisitor::expand_tuple_fields (std::vector<AST::TupleField> &fields)
{
  for (auto &field : fields)
    maybe_expand_type (field.get_field_type ());
}

// FIXME: This can definitely be refactored with the method above
void
ExpandVisitor::expand_function_params (
  std::vector<std::unique_ptr<AST::Param>> &params)
{
  for (auto &p : params)
    visit (p);
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
    {
      if (param.has_type_given ())
	maybe_expand_type (param.get_type ());
    }
}

void
ExpandVisitor::expand_where_clause (AST::WhereClause &where_clause)
{
  for (auto &item : where_clause.get_items ())
    visit (item);
}

void
ExpandVisitor::visit (AST::Crate &crate)
{
  expand_inner_items (crate.items);
}

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
ExpandVisitor::visit (AST::AttrInputMacro &macro)
{
  rust_sorry_at (UNDEF_LOCATION, "macros in attributes not supported");
}

void
ExpandVisitor::visit (AST::MetaItemLitExpr &)
{}

void
ExpandVisitor::visit (AST::MetaItemPathLit &)
{}

void
ExpandVisitor::visit (AST::ErrorPropagationExpr &expr)
{
  visit (expr.get_propagating_expr ());
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
  maybe_expand_expr (expr.get_expr_in_parens ());
}

void
ExpandVisitor::visit (AST::StructExprStruct &expr)
{}

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
ExpandVisitor::visit (AST::ClosureExprInner &expr)
{
  expand_closure_params (expr.get_params ());

  visit (expr.get_definition_expr ());
}

void
ExpandVisitor::visit (AST::BlockExpr &expr)
{
  expand_inner_stmts (expr);

  expand_tail_expr (expr, expander);
  if (expr.has_tail_expr ())
    maybe_expand_expr (expr.get_tail_expr ());
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
ExpandVisitor::visit (AST::MatchExpr &expr)
{
  visit (expr.get_scrutinee_expr ());

  for (auto &match_case : expr.get_match_cases ())
    {
      auto &arm = match_case.get_arm ();

      for (auto &pattern : arm.get_patterns ())
	visit (pattern);

      if (arm.has_match_arm_guard ())
	maybe_expand_expr (arm.get_guard_expr ());

      maybe_expand_expr (match_case.get_expr ());
    }
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
  if (function.has_body ())
    visit_inner_using_attrs (
      function, function.get_definition ().value ()->get_inner_attrs ());
  for (auto &param : function.get_generic_params ())
    visit (param);

  expand_function_params (function.get_function_params ());

  if (function.has_return_type ())
    maybe_expand_type (function.get_return_type ());

  if (function.has_where_clause ())
    expand_where_clause (function.get_where_clause ());

  if (function.has_body ())
    visit (*function.get_definition ());
}

void
ExpandVisitor::visit (AST::StructStruct &struct_item)
{
  for (auto &generic : struct_item.get_generic_params ())
    visit (generic);

  if (struct_item.has_where_clause ())
    expand_where_clause (struct_item.get_where_clause ());

  expand_struct_fields (struct_item.get_fields ());
}

void
ExpandVisitor::visit (AST::TupleStruct &tuple_struct)
{
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
  maybe_expand_expr (item.get_expr ());
}

void
ExpandVisitor::visit (AST::Union &union_item)
{
  for (auto &generic : union_item.get_generic_params ())
    visit (generic);

  expand_struct_fields (union_item.get_variants ());
}

void
ExpandVisitor::visit (AST::ConstantItem &const_item)
{
  maybe_expand_type (const_item.get_type ());

  if (const_item.has_expr ())
    maybe_expand_expr (const_item.get_expr ());
}

void
ExpandVisitor::visit (AST::StaticItem &static_item)
{
  maybe_expand_type (static_item.get_type ());

  maybe_expand_expr (static_item.get_expr ());
}

void
ExpandVisitor::visit (AST::TraitItemConst &const_item)
{
  maybe_expand_type (const_item.get_type ());

  if (const_item.has_expr ())
    maybe_expand_expr (const_item.get_expr ());
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

  std::function<std::unique_ptr<AST::AssociatedItem> (AST::SingleASTNode)>
    extractor
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

  std::function<std::unique_ptr<AST::AssociatedItem> (AST::SingleASTNode)>
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

  std::function<std::unique_ptr<AST::AssociatedItem> (AST::SingleASTNode)>
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

  for (auto &param : item.get_function_params ())
    if (!param.is_variadic ())
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
ExpandVisitor::visit (AST::MetaListPaths &)
{}

void
ExpandVisitor::visit (AST::MetaListNameValueStr &)
{}

void
ExpandVisitor::visit (AST::StructPatternFieldIdent &field)
{}

void
ExpandVisitor::visit (AST::GroupedPattern &pattern)
{
  visit (pattern.get_pattern_in_parens ());
}

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
ExpandVisitor::visit (AST::ExprStmt &stmt)
{
  maybe_expand_expr (stmt.get_expr ());
}

void
ExpandVisitor::visit (AST::BareFunctionType &type)
{
  for (auto &param : type.get_function_params ())
    {
      maybe_expand_type (param.get_type ());
    }

  if (type.has_return_type ())
    visit (type.get_return_type ());
}

void
ExpandVisitor::visit (AST::FunctionParam &param)
{
  maybe_expand_type (param.get_type ());
}

void
ExpandVisitor::visit (AST::SelfParam &param)
{
  /* TODO: maybe check for invariants being violated - e.g. both type and
   * lifetime? */
  if (param.has_type ())
    maybe_expand_type (param.get_type ());
}

template <typename T>
void
ExpandVisitor::expand_inner_attribute (T &item, AST::SimplePath &path)
{
  // FIXME: Retrieve path from segments + local use statements instead of string
  expander.expand_attribute_proc_macro (item, path);
}

template <typename T>
void
ExpandVisitor::visit_inner_using_attrs (T &item,
					std::vector<AST::Attribute> &attrs)
{
  for (auto it = attrs.begin (); it != attrs.end (); /* erase => No increment*/)
    {
      auto current = *it;

      if (!is_builtin (current) && !current.is_derive ())
	{
	  it = attrs.erase (it);
	  expand_inner_attribute (item, current.get_path ());
	}
      else
	{
	  it++;
	}
    }
}

template <typename T>
void
ExpandVisitor::visit_inner_attrs (T &item)
{
  visit_inner_using_attrs (item, item.get_inner_attrs ());
}

} // namespace Rust
