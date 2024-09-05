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

#include "rust-default-resolver.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-item.h"

namespace Rust {
namespace Resolver2_0 {

void
DefaultResolver::visit (AST::BlockExpr &expr)
{
  // extracting the lambda from the `scoped` call otherwise the code looks like
  // a hot turd thanks to our .clang-format

  auto inner_fn = [this, &expr] () {
    for (auto &stmt : expr.get_statements ())
      stmt->accept_vis (*this);

    if (expr.has_tail_expr ())
      expr.get_tail_expr ().accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::Module &module)
{
  auto item_fn = [this, &module] () {
    for (auto &item : module.get_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Module, module.get_node_id (), item_fn,
	      module.get_name ());
}

void
DefaultResolver::visit (AST::Function &function)
{
  auto def_fn = [this, &function] () {
    for (auto &p : function.get_function_params ())
      {
	if (p->is_variadic ())
	  {
	    auto &param = static_cast<AST::VariadicParam &> (*p);
	    if (param.has_pattern ())
	      param.get_pattern ().accept_vis (*this);
	  }
	else if (p->is_self ())
	  {
	    auto &param = static_cast<AST::SelfParam &> (*p);

	    if (param.has_type ())
	      param.get_type ().accept_vis (*this);

	    param.get_lifetime ().accept_vis (*this);
	  }
	else
	  {
	    auto &param = static_cast<AST::FunctionParam &> (*p);
	    param.get_pattern ().accept_vis (*this);
	    param.get_type ().accept_vis (*this);
	  }
      }

    if (function.has_return_type ())
      visit (function.get_return_type ());

    if (function.has_body ())
      function.get_definition ().value ()->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Function, function.get_node_id (), def_fn);
}

void
DefaultResolver::visit (AST::ForLoopExpr &expr)
{
  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), [this, &expr] () {
    expr.get_pattern ().accept_vis (*this);
    expr.get_iterator_expr ().accept_vis (*this);
    expr.get_loop_block ().accept_vis (*this);
  });
}

void
DefaultResolver::visit (AST::Trait &trait)
{
  auto inner_fn = [this, &trait] () {
    for (auto &item : trait.get_trait_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, trait.get_node_id (), inner_fn,
	      trait.get_identifier () /* FIXME: Is that valid?*/);
}

void
DefaultResolver::visit (AST::InherentImpl &impl)
{
  auto inner_fn = [this, &impl] () {
    visit (impl.get_type ());
    for (auto &item : impl.get_impl_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::TraitImpl &impl)
{
  auto inner_fn = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      item->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::StructStruct &type)
{
  // do we need to scope anything here? no, right?

  // we also can't visit `StructField`s by default, so there's nothing to do -
  // correct? or should we do something like

  AST::DefaultASTVisitor::visit (type);

  // FIXME: ???
}

void
DefaultResolver::visit (AST::Enum &type)
{
  // FIXME: Do we need to scope anything by default?

  auto variant_fn = [this, &type] () {
    for (auto &variant : type.get_variants ())
      variant->accept_vis (*this);
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      variant_fn, type.get_identifier ());
}

void
DefaultResolver::visit (AST::ClosureExprInner &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  for (auto &param : expr.get_params ())
    {
      if (param.is_error ())
	continue;

      param.get_pattern ().accept_vis (*this);
      if (param.has_type_given ())
	param.get_type ().accept_vis (*this);
    }

  expr.get_definition_expr ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::ClosureExprInnerTyped &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  for (auto &param : expr.get_params ())
    {
      if (param.is_error ())
	continue;

      param.get_pattern ().accept_vis (*this);
      if (param.has_type_given ())
	param.get_type ().accept_vis (*this);
    }

  expr.get_definition_block ().accept_vis (*this);
  expr.get_return_type ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::CallExpr &expr)
{
  expr.get_function_expr ().accept_vis (*this);

  for (auto &param : expr.get_params ())
    param->accept_vis (*this);
}

void
DefaultResolver::visit (AST::MethodCallExpr &expr)
{
  expr.get_receiver_expr ().accept_vis (*this);

  if (expr.get_method_name ().has_generic_args ())
    {
      auto &args = expr.get_method_name ().get_generic_args ();
      for (auto &arg : args.get_generic_args ())
	arg.accept_vis (*this);
      for (auto &arg : args.get_binding_args ())
	if (!arg.is_error ())
	  arg.get_type ().accept_vis (*this);
      for (auto &arg : args.get_lifetime_args ())
	arg.accept_vis (*this);
    }

  for (auto &param : expr.get_params ())
    param->accept_vis (*this);
}

void
DefaultResolver::visit (AST::IfExpr &expr)
{
  expr.get_condition_expr ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::IfExprConseqElse &expr)
{
  expr.get_condition_expr ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
  expr.get_else_block ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::MatchExpr &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  expr.get_scrutinee_expr ().accept_vis (*this);
  for (auto &arm : expr.get_match_cases ())
    {
      arm.get_expr ().accept_vis (*this);
      for (auto &pat : arm.get_arm ().get_patterns ())
	pat->accept_vis (*this);
      if (arm.get_arm ().has_match_arm_guard ())
	arm.get_arm ().get_guard_expr ().accept_vis (*this);
    }
}

void
DefaultResolver::visit (AST::PathInExpression &expr)
{
  for (auto &seg : expr.get_segments ())
    if (seg.has_generic_args ())
      {
	auto &args = seg.get_generic_args ();
	for (auto &arg : args.get_generic_args ())
	  arg.accept_vis (*this);
	for (auto &arg : args.get_binding_args ())
	  if (!arg.is_error ())
	    arg.get_type ().accept_vis (*this);
	for (auto &arg : args.get_lifetime_args ())
	  arg.accept_vis (*this);
      }
}

void
DefaultResolver::visit (AST::EnumItemTuple &item)
{
  for (auto &field : item.get_tuple_fields ())
    field.get_field_type ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::EnumItemStruct &item)
{
  for (auto &field : item.get_struct_fields ())
    field.get_field_type ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::EnumItemDiscriminant &item)
{
  if (item.has_expr ())
    item.get_expr ().accept_vis (*this);
}

void
DefaultResolver::visit (AST::ConstantItem &item)
{
  if (item.has_expr ())
    {
      auto expr_vis = [this, &item] () {
	item.get_expr ().accept_vis (*this);
	visit (item.get_type ());
      };

      // FIXME: Why do we need a Rib here?
      ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis);
    }
}

void
DefaultResolver::visit (AST::StaticItem &item)
{
  auto expr_vis = [this, &item] () { item.get_expr ().accept_vis (*this); };

  // FIXME: Why do we need a Rib here?
  ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis);
}

} // namespace Resolver2_0
} // namespace Rust
