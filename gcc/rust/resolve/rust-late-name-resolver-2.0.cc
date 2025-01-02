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
#include "rust-hir-map.h"
#include "rust-late-name-resolver-2.0.h"
#include "rust-default-resolver.h"
#include "rust-name-resolution-context.h"
#include "rust-path.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver2_0 {

Late::Late (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

static NodeId
next_node_id ()
{
  return Analysis::Mappings::get ()->get_next_node_id ();
};

static HirId
next_hir_id ()
{
  return Analysis::Mappings::get ()->get_next_hir_id ();
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
  auto *unit_type = TyTy::TupleType::get_unit_type (next_hir_id ());
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
  // so we don't need that method
  DefaultResolver::visit (let);

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
  auto ok
    = ctx.values.insert (identifier.get_ident (), identifier.get_node_id ());

  rust_assert (ok);
}

void
Late::visit (AST::IdentifierExpr &expr)
{
  // TODO: same thing as visit(PathInExpression) here?

  tl::optional<Rib::Definition> resolved = tl::nullopt;
  auto label = ctx.labels.get (expr.get_ident ());
  auto value = ctx.values.get (expr.get_ident ());

  if (label)
    {
      resolved = label;
    }
  else if (value)
    {
      resolved = value;
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

  auto value = ctx.values.resolve_path (expr.get_segments ());
  if (!value.has_value ())
    rust_unreachable (); // Should have been resolved earlier

  if (value->is_ambiguous ())
    {
      rust_error_at (expr.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     expr.as_string ().c_str ());
      return;
    }
  ctx.map_usage (Usage (expr.get_node_id ()),
		 Definition (value->get_node_id ()));
}

void
Late::visit (AST::TypePath &type)
{
  // should we add type path resolution in `ForeverStack` directly? Since it's
  // quite more complicated.
  // maybe we can overload `resolve_path<Namespace::Types>` to only do
  // typepath-like path resolution? that sounds good

  auto resolved = ctx.types.get (type.get_segments ().back ()->as_string ());

  ctx.map_usage (Usage (type.get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit (AST::StructExprStructBase &s)
{
  auto resolved = ctx.types.get (s.get_struct_name ().as_string ());

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));
  DefaultResolver::visit (s);
}

void
Late::visit (AST::StructExprStructFields &s)
{
  auto resolved = ctx.types.get (s.get_struct_name ().as_string ());

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));

  DefaultResolver::visit (s);
}

} // namespace Resolver2_0
} // namespace Rust
