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
DefaultResolver::visit (AST::Crate &crate)
{
  auto inner_fn = [this, &crate] () { AST::DefaultASTVisitor::visit (crate); };

  auto &mappings = Analysis::Mappings::get ();

  auto crate_num = mappings.lookup_crate_num (crate.get_node_id ());
  rust_assert (crate_num.has_value ());
  auto crate_name = mappings.get_crate_name (*crate_num);
  rust_assert (crate_name.has_value ());

  ctx.canonical_ctx.scope_crate (crate.get_node_id (), *crate_name, inner_fn);
}

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
  auto item_fn_1
    = [this, &module] () { AST::DefaultASTVisitor::visit (module); };

  auto item_fn_2 = [this, &module, &item_fn_1] () {
    ctx.canonical_ctx.scope (module.get_node_id (), module.get_name (),
			     std::move (item_fn_1));
  };

  ctx.scoped (Rib::Kind::Module, module.get_node_id (), item_fn_2,
	      module.get_name ());
}

void
DefaultResolver::visit (AST::Function &function)
{
  auto def_fn_1
    = [this, &function] () { AST::DefaultASTVisitor::visit (function); };

  auto def_fn_2 = [this, &function, &def_fn_1] () {
    ctx.canonical_ctx.scope (function.get_node_id (),
			     function.get_function_name (),
			     std::move (def_fn_1));
  };

  ctx.scoped (Rib::Kind::Function, function.get_node_id (), def_fn_2,
	      function.get_function_name ());
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
  visit_outer_attrs (trait);
  visit (trait.get_visibility ());
  visit_inner_attrs (trait);

  auto inner_fn_1 = [this, &trait] () {
    for (auto &item : trait.get_trait_items ())
      visit (item);
  };

  auto inner_fn_2 = [this, &trait, &inner_fn_1] () {
    visit (trait.get_implicit_self ());
    for (auto &generic : trait.get_generic_params ())
      visit (generic);
    if (trait.has_where_clause ())
      visit (trait.get_where_clause ());
    for (auto &bound : trait.get_type_param_bounds ())
      visit (bound);

    ctx.scoped (Rib::Kind::TraitOrImpl, trait.get_node_id (), inner_fn_1);
  };

  auto inner_fn_3 = [this, &trait, &inner_fn_2] () {
    ctx.canonical_ctx.scope (trait.get_node_id (), trait.get_identifier (),
			     std::move (inner_fn_2));
  };

  ctx.scoped (Rib::Kind::Generics, trait.get_node_id (), inner_fn_3,
	      trait.get_identifier () /* FIXME: Is that valid?*/);
}

void
DefaultResolver::visit (AST::InherentImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());
  visit_inner_attrs (impl);

  auto inner_fn_1 = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      visit (item);
  };

  auto inner_fn_2 = [this, &impl, &inner_fn_1] () {
    maybe_insert_big_self (impl);
    for (auto &generic : impl.get_generic_params ())
      visit (generic);
    if (impl.has_where_clause ())
      visit (impl.get_where_clause ());
    visit_impl_type (impl.get_type ());

    ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn_1);
  };

  auto inner_fn_3 = [this, &impl, &inner_fn_2] () {
    ctx.canonical_ctx.scope_impl (impl, std::move (inner_fn_2));
  };

  ctx.scoped (Rib::Kind::Generics, impl.get_node_id (), inner_fn_3);
}

void
DefaultResolver::visit (AST::TraitImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());
  visit_inner_attrs (impl);

  auto inner_fn_1 = [this, &impl] () {
    for (auto &item : impl.get_impl_items ())
      visit (item);
  };

  auto inner_fn_2 = [this, &impl, &inner_fn_1] () {
    maybe_insert_big_self (impl);
    for (auto &generic : impl.get_generic_params ())
      visit (generic);
    if (impl.has_where_clause ())
      visit (impl.get_where_clause ());
    visit_impl_type (impl.get_type ());
    visit (impl.get_trait_path ());

    ctx.scoped (Rib::Kind::TraitOrImpl, impl.get_node_id (), inner_fn_1);
  };

  auto inner_fn_3 = [this, &impl, &inner_fn_2] () {
    ctx.canonical_ctx.scope_impl (impl, std::move (inner_fn_2));
  };

  ctx.scoped (Rib::Kind::Generics, impl.get_node_id (), inner_fn_3);
}

void
DefaultResolver::visit (AST::StructStruct &type)
{
  auto inner_fn_1 = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  auto inner_fn_2 = [this, &type, &inner_fn_1] () {
    ctx.canonical_ctx.scope (type.get_node_id (), type.get_struct_name (),
			     std::move (inner_fn_1));
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn_2, type.get_struct_name ());
}

void
DefaultResolver::visit (AST::TupleStruct &type)
{
  auto inner_fn_1 = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  auto inner_fn_2 = [this, &type, &inner_fn_1] () {
    ctx.canonical_ctx.scope (type.get_node_id (), type.get_struct_name (),
			     std::move (inner_fn_1));
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn_2, type.get_struct_name ());
}

void
DefaultResolver::visit (AST::EnumItem &item)
{
  auto inner_fn = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  ctx.canonical_ctx.scope (item.get_node_id (), item.get_identifier (),
			   inner_fn);
}

void
DefaultResolver::visit (AST::EnumItemTuple &item)
{
  auto inner_fn = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  ctx.canonical_ctx.scope (item.get_node_id (), item.get_identifier (),
			   inner_fn);
}

void
DefaultResolver::visit (AST::EnumItemStruct &item)
{
  auto inner_fn = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  ctx.canonical_ctx.scope (item.get_node_id (), item.get_identifier (),
			   inner_fn);
}

void
DefaultResolver::visit (AST::EnumItemDiscriminant &item)
{
  auto inner_fn = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  ctx.canonical_ctx.scope (item.get_node_id (), item.get_identifier (),
			   inner_fn);
}

void
DefaultResolver::visit (AST::Enum &type)
{
  auto inner_fn_1 = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  auto inner_fn_2 = [this, &type, &inner_fn_1] () {
    ctx.canonical_ctx.scope (type.get_node_id (), type.get_identifier (),
			     std::move (inner_fn_1));
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn_2, type.get_identifier ());
}

void
DefaultResolver::visit (AST::Union &type)
{
  auto inner_fn_1 = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  auto inner_fn_2 = [this, &type, &inner_fn_1] () {
    ctx.canonical_ctx.scope (type.get_node_id (), type.get_identifier (),
			     std::move (inner_fn_1));
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn_2, type.get_identifier ());
}

void
DefaultResolver::visit (AST::TypeAlias &type)
{
  auto inner_fn_1 = [this, &type] () { AST::DefaultASTVisitor::visit (type); };

  auto inner_fn_2 = [this, &type, &inner_fn_1] () {
    ctx.canonical_ctx.scope (type.get_node_id (), type.get_new_type_name (),
			     std::move (inner_fn_1));
  };

  ctx.scoped (Rib::Kind::Item /* FIXME: Correct? */, type.get_node_id (),
	      inner_fn_2, type.get_new_type_name ());
}

void
DefaultResolver::visit_closure_params (AST::ClosureExpr &expr)
{
  for (auto &param : expr.get_params ())
    visit (param);
}

void
DefaultResolver::visit (AST::ClosureExpr &expr)
{
  auto expr_fn = [this, &expr] () {
    visit_closure_params (expr);
    visit (expr.get_definition_expr ());
  };

  visit_outer_attrs (expr);

  ctx.scoped (Rib::Kind::Normal, expr.get_node_id (), expr_fn);
}

void
DefaultResolver::visit (AST::ClosureExprInner &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  visit (static_cast<AST::ClosureExpr &> (expr));
}

void
DefaultResolver::visit (AST::ClosureExprInnerTyped &expr)
{
  if (expr.is_marked_for_strip ())
    return;

  visit (static_cast<AST::ClosureExpr &> (expr));
  visit (expr.get_return_type ());
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
  auto expr_vis_1 = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  auto expr_vis_2 = [this, &item, &expr_vis_1] () {
    ctx.canonical_ctx.scope (item.get_node_id (), item.get_identifier (),
			     std::move (expr_vis_1));
  };

  // FIXME: Why do we need a Rib here?
  ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis_2);
}

void
DefaultResolver::visit (AST::StaticItem &item)
{
  auto expr_vis_1 = [this, &item] () { AST::DefaultASTVisitor::visit (item); };

  auto expr_vis_2 = [this, &item, &expr_vis_1] () {
    ctx.canonical_ctx.scope (item.get_node_id (), item.get_identifier (),
			     std::move (expr_vis_1));
  };

  // FIXME: Why do we need a Rib here?
  ctx.scoped (Rib::Kind::ConstantItem, item.get_node_id (), expr_vis_2);
}

void
DefaultResolver::visit (AST::TypeParam &param)
{
  auto expr_vis = [this, &param] () { AST::DefaultASTVisitor::visit (param); };

  ctx.scoped (Rib::Kind::ForwardTypeParamBan, param.get_node_id (), expr_vis);
}

void
DefaultResolver::visit_extern_crate (AST::ExternCrate &extern_crate,
				     AST::Crate &crate, CrateNum num)
{
  visit (crate);
}

void
DefaultResolver::visit (AST::ExternCrate &crate)
{
  auto &mappings = Analysis::Mappings::get ();
  auto num_opt = mappings.lookup_crate_name (crate.get_referenced_crate ());

  if (!num_opt)
    {
      rust_error_at (crate.get_locus (), "unknown crate %qs",
		     crate.get_referenced_crate ().c_str ());
      return;
    }

  CrateNum num = *num_opt;

  AST::Crate &referenced_crate = mappings.get_ast_crate (num);

  auto sub_visitor_1
    = [&, this] () { visit_extern_crate (crate, referenced_crate, num); };

  auto sub_visitor_2 = [&] () {
    ctx.canonical_ctx.scope_crate (referenced_crate.get_node_id (),
				   crate.get_referenced_crate (),
				   std::move (sub_visitor_1));
  };

  if (crate.has_as_clause ())
    ctx.scoped (Rib::Kind::Module, referenced_crate.get_node_id (),
		sub_visitor_2, crate.get_as_clause ());
  else
    ctx.scoped (Rib::Kind::Module, referenced_crate.get_node_id (),
		sub_visitor_2, crate.get_referenced_crate ());
}

} // namespace Resolver2_0
} // namespace Rust
