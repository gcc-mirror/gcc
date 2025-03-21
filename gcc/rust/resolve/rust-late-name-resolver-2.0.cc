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

#include "optional.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-late-name-resolver-2.0.h"
#include "rust-default-resolver.h"
#include "rust-name-resolution-context.h"
#include "rust-path.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver2_0 {

Late::Late (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

static NodeId
next_node_id ()
{
  return Analysis::Mappings::get ().get_next_node_id ();
};

static HirId
next_hir_id ()
{
  return Analysis::Mappings::get ().get_next_hir_id ();
};

void
Late::setup_builtin_types ()
{
  // access the global type context to setup the TyTys
  auto &ty_ctx = *Resolver::TypeCheckContext::get ();

  // Late builtin type struct helper
  struct LType
  {
    std::string name;
    NodeId node_id;
    NodeId hir_id;
    TyTy::BaseType *type;

    explicit LType (std::string name, TyTy::BaseType *type)
      : name (name), node_id (next_node_id ()), hir_id (type->get_ref ()),
	type (type)
    {}
  };

  static const LType builtins[] = {
    {LType ("bool", new TyTy::BoolType (next_hir_id ()))},
    {LType ("u8", new TyTy::UintType (next_hir_id (), TyTy::UintType::U8))},
    {LType ("u16", new TyTy::UintType (next_hir_id (), TyTy::UintType::U16))},
    {LType ("u32", new TyTy::UintType (next_hir_id (), TyTy::UintType::U32))},
    {LType ("u64", new TyTy::UintType (next_hir_id (), TyTy::UintType::U64))},
    {LType ("u128", new TyTy::UintType (next_hir_id (), TyTy::UintType::U128))},
    {LType ("i8", new TyTy::IntType (next_hir_id (), TyTy::IntType::I8))},
    {LType ("i16", new TyTy::IntType (next_hir_id (), TyTy::IntType::I16))},
    {LType ("i32", new TyTy::IntType (next_hir_id (), TyTy::IntType::I32))},
    {LType ("i64", new TyTy::IntType (next_hir_id (), TyTy::IntType::I64))},
    {LType ("i128", new TyTy::IntType (next_hir_id (), TyTy::IntType::I128))},
    {LType ("f32", new TyTy::FloatType (next_hir_id (), TyTy::FloatType::F32))},
    {LType ("f64", new TyTy::FloatType (next_hir_id (), TyTy::FloatType::F64))},
    {LType ("usize", new TyTy::USizeType (next_hir_id ()))},
    {LType ("isize", new TyTy::ISizeType (next_hir_id ()))},
    {LType ("char", new TyTy::CharType (next_hir_id ()))},
    {LType ("str", new TyTy::StrType (next_hir_id ()))},
    {LType ("!", new TyTy::NeverType (next_hir_id ()))},

    // the unit type `()` does not play a part in name-resolution - so we only
    // insert it in the type context...
  };

  for (const auto &builtin : builtins)
    {
      // we should be able to use `insert_at_root` or `insert` here, since we're
      // at the root :) hopefully!
      auto ok = ctx.types.insert (builtin.name, builtin.node_id);
      rust_assert (ok);

      ctx.mappings.insert_node_to_hir (builtin.node_id, builtin.hir_id);
      ty_ctx.insert_builtin (builtin.hir_id, builtin.node_id, builtin.type);
    }

  // ...here!
  auto *unit_type = TyTy::TupleType::get_unit_type ();
  ty_ctx.insert_builtin (unit_type->get_ref (), next_node_id (), unit_type);
}

void
Late::go (AST::Crate &crate)
{
  setup_builtin_types ();

  for (auto &item : crate.items)
    item->accept_vis (*this);
}

void
Late::new_label (Identifier name, NodeId id)
{
  // labels can always shadow, so `insert` should never fail. if it does, we're
  // in big trouble!
  auto ok = ctx.labels.insert (name, id);

  rust_assert (ok);
}

void
Late::visit (AST::LetStmt &let)
{
  DefaultASTVisitor::visit_outer_attrs (let);
  if (let.has_type ())
    visit (let.get_type ());
  // visit expression before pattern
  // this makes variable shadowing work properly
  if (let.has_init_expr ())
    visit (let.get_init_expr ());
  visit (let.get_pattern ());

  // how do we deal with the fact that `let a = blipbloup` should look for a
  // label and cannot go through function ribs, but `let a = blipbloup()` can?

  // how do we insert ribs here, and only pop them when we exit the current
  // function?
  // keep a list of ribs to pop when a scope exits? so only for blocks?
  // how do we pop ribs that need to be popped not in order?
  // I think it's not important if we have shadowing, correct?

  // if we have shadowing, it should work! we'll see

  // ctx.insert(Identifier name, NodeId id, Namespace ns)
  // ctx.scoped (Rib::Kind::Normal /* FIXME: Is that valid? */,
  // Namespace::Labels,
  //      let.get_node_id (), [] () {});
}

void
Late::visit (AST::IdentifierPattern &identifier)
{
  // do we insert in labels or in values
  // but values does not allow shadowing... since functions cannot shadow
  // do we insert functions in labels as well?

  // We do want to ignore duplicated data because some situations rely on it.
  std::ignore = ctx.values.insert_shadowable (identifier.get_ident (),
					      identifier.get_node_id ());
}

void
Late::visit (AST::SelfParam &param)
{
  // handle similar to AST::IdentifierPattern

  DefaultResolver::visit (param);
  // FIXME: this location should be a bit off
  // ex: would point to the begining of "mut self" instead of the "self"
  std::ignore = ctx.values.insert (Identifier ("self", param.get_locus ()),
				   param.get_node_id ());
}

void
Late::visit (AST::IdentifierExpr &expr)
{
  // TODO: same thing as visit(PathInExpression) here?

  tl::optional<Rib::Definition> resolved = tl::nullopt;

  if (auto value = ctx.values.get (expr.get_ident ()))
    {
      resolved = value;
    }
  else if (auto type = ctx.types.get (expr.get_ident ()))
    {
      resolved = type;
    }
  else
    {
      rust_error_at (expr.get_locus (),
		     "could not resolve identifier expression: %qs",
		     expr.get_ident ().as_string ().c_str ());
      return;
    }

  ctx.map_usage (Usage (expr.get_node_id ()),
		 Definition (resolved->get_node_id ()));

  // in the old resolver, resolutions are kept in the resolver, not the mappings
  // :/ how do we deal with that?
  // ctx.mappings.insert_resolved_name(expr, resolved);

  // For empty types, do we perform a lookup in ctx.types or should the
  // toplevel instead insert a name in ctx.values? (like it currently does)
}

void
Late::visit (AST::PathInExpression &expr)
{
  // TODO: How do we have a nice error with `can't capture dynamic environment
  // in a function item` error here?
  // do we emit it in `get<Namespace::Labels>`?

  auto resolved
    = ctx.values.resolve_path (expr.get_segments ()).or_else ([&] () {
	return ctx.types.resolve_path (expr.get_segments ());
      });

  if (!resolved)
    {
      rust_error_at (expr.get_locus (),
		     "could not resolve path expression: %qs",
		     expr.as_simple_path ().as_string ().c_str ());
      return;
    }

  if (resolved->is_ambiguous ())
    {
      rust_error_at (expr.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     expr.as_string ().c_str ());
      return;
    }

  ctx.map_usage (Usage (expr.get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit (AST::TypePath &type)
{
  // should we add type path resolution in `ForeverStack` directly? Since it's
  // quite more complicated.
  // maybe we can overload `resolve_path<Namespace::Types>` to only do
  // typepath-like path resolution? that sounds good

  auto str = type.get_segments ().back ()->get_ident_segment ().as_string ();
  auto values = ctx.types.peek ().get_values ();

  if (auto resolved = ctx.types.get (str))
    ctx.map_usage (Usage (type.get_node_id ()),
		   Definition (resolved->get_node_id ()));
  else
    rust_error_at (type.get_locus (), "could not resolve type path %qs",
		   str.c_str ());

  DefaultResolver::visit (type);
}

void
Late::visit (AST::Trait &trait)
{
  // kind of weird how this is done
  // names are resolved to the node id of trait.get_implicit_self ()
  // which is then resolved to the node id of trait
  // we set up the latter mapping here
  ctx.map_usage (Usage (trait.get_implicit_self ().get_node_id ()),
		 Definition (trait.get_node_id ()));

  DefaultResolver::visit (trait);
}

void
Late::visit (AST::StructStruct &s)
{
  auto s_vis = [this, &s] () { AST::DefaultASTVisitor::visit (s); };
  ctx.scoped (Rib::Kind::Item, s.get_node_id (), s_vis);
}

void
Late::visit (AST::StructExprStruct &s)
{
  auto resolved = ctx.types.resolve_path (s.get_struct_name ().get_segments ());

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit (AST::StructExprStructBase &s)
{
  auto resolved = ctx.types.resolve_path (s.get_struct_name ().get_segments ());

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));
  DefaultResolver::visit (s);
}

void
Late::visit (AST::StructExprStructFields &s)
{
  auto resolved = ctx.types.resolve_path (s.get_struct_name ().get_segments ());

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));

  DefaultResolver::visit (s);
}

// needed because Late::visit (AST::GenericArg &) is non-virtual
void
Late::visit (AST::GenericArgs &args)
{
  for (auto &lifetime : args.get_lifetime_args ())
    visit (lifetime);

  for (auto &generic : args.get_generic_args ())
    visit (generic);

  for (auto &binding : args.get_binding_args ())
    visit (binding);
}

void
Late::visit (AST::GenericArg &arg)
{
  if (arg.get_kind () == AST::GenericArg::Kind::Either)
    {
      // prefer type parameter to const parameter on ambiguity
      auto type = ctx.types.get (arg.get_path ());
      auto value = ctx.values.get (arg.get_path ());

      if (!type.has_value () && value.has_value ())
	arg = arg.disambiguate_to_const ();
      else
	arg = arg.disambiguate_to_type ();
    }

  DefaultResolver::visit (arg);
}

} // namespace Resolver2_0
} // namespace Rust
