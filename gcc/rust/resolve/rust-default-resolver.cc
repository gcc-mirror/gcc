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

  auto inner_fn = [this, &expr] () { AST::DefaultASTVisitor::visit (expr); };

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), inner_fn);
}

void
DefaultResolver::visit (AST::Module &module)
{
  auto item_fn = [this, &module] () { AST::DefaultASTVisitor::visit (module); };

  ctx.scoped (Rib::Kind::Module, module.get_node_id (), item_fn,
	      module.get_name ());
}

void
DefaultResolver::visit (AST::Function &function)
{
  auto def_fn
    = [this, &function] () { AST::DefaultASTVisitor::visit (function); };

  ctx.scoped (Rib::Kind::Function, function.get_node_id (), def_fn);
}

void
DefaultResolver::visit (AST::ForLoopExpr &expr)
{
  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (),
	      [this, &expr] () { AST::DefaultASTVisitor::visit (expr); });
}

void
DefaultResolver::visit_if_let_patterns (AST::IfLetExpr &expr)
{
  for (auto &pattern : expr.get_patterns ())
    visit (pattern);
}

void
DefaultResolver::visit (AST::IfLetExpr &expr)
{
  auto inner_vis = [this, &expr] () {
    visit_if_let_patterns (expr);
    visit (expr.get_if_block ());
  };

  visit_outer_attrs (expr);

  visit (expr.get_value_expr ());

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), inner_vis);
}

void
DefaultResolver::visit (AST::Trait &trait)
{
  auto inner_fn = [this, &trait] () { AST::DefaultASTVisitor::visit (trait); };

  ctx.scoped (Rib::Kind::TraitOrImpl, trait.get_node_id (), inner_fn,
	      trait.get_identifier () /* FIXME: Is that valid?*/);
}

void
DefaultResolver::visit (AST::InherentImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());
  visit_inner_attrs (impl);

  auto inner_fn_inner = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      visit (item);
  };

  auto inner_fn_outer = [this, &impl, &inner_fn_inner] () {
    maybe_insert_big_self (impl);
    for (auto &generic : impl.get_generic_params ())
      visit (generic);
    if (impl.has_where_clause ())
      visit (impl.get_where_clause ());
    visit_impl_type (impl.get_type ());

    ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn_inner);
  };

  ctx.scoped (Rib::Kind::Generics, impl.get_node_id (), inner_fn_outer);
}

void
DefaultResolver::visit (AST::TraitImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());
  visit_inner_attrs (impl);

  auto inner_fn_inner = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      visit (item);
  };

  auto inner_fn_outer = [this, &impl, &inner_fn_inner] () {
    maybe_insert_big_self (impl);
    for (auto &generic : impl.get_generic_params ())
      visit (generic);
    if (impl.has_where_clause ())
      visit (impl.get_where_clause ());
    visit_impl_type (impl.get_type ());
    visit (impl.get_trait_path ());

    ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn_inner);
  };

  ctx.scoped (Rib::Kind::Generics, impl.get_node_id (), inner_fn_outer);
}

void
DefaultResolver::visit (AST::StructStruct &type)
{
  auto inner_fn = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn, type.get_struct_name ());
}

void
DefaultResolver::visit (AST::TupleStruct &type)
{
  auto inner_fn = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn, type.get_struct_name ());
}

void
DefaultResolver::visit (AST::Enum &type)
{
  auto variant_fn = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      variant_fn, type.get_identifier ());
}

void
DefaultResolver::visit (AST::Union &type)
{
  auto inner_fn = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn, type.get_identifier ());
}

void
DefaultResolver::visit (AST::TypeAlias &type)
{
  auto inner_fn = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn, type.get_new_type_name ());
}

void
DefaultResolver::visit (AST::ClosureExprInner &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  AST::DefaultASTVisitor::visit (expr);
}

void
DefaultResolver::visit (AST::ClosureExprInnerTyped &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  AST::DefaultASTVisitor::visit (expr);
}

void
DefaultResolver::visit (AST::MatchExpr &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  AST::DefaultASTVisitor::visit (expr);
}

void
DefaultResolver::visit (AST::ConstantItem &item)
{
  if (item.has_expr ())
    {
      auto expr_vis
	= [this, &item] () { AST::DefaultASTVisitor::visit (item); };

      // FIXME: Why do we need a Rib here?
      ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis);
    }
}

void
DefaultResolver::visit (AST::StaticItem &item)
{
  auto expr_vis = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  // FIXME: Why do we need a Rib here?
  ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis);
}

void
DefaultResolver::visit (AST::TypeParam &param)
{
  auto expr_vis = [this, &param] () { AST::DefaultASTVisitor::visit (param); };

  ctx.scoped (Rib::Kind::ForwardTypeParamBan, param.get_node_id (), expr_vis);
}

} // namespace Resolver2_0
} // namespace Rust
