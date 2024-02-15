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

#include "optional.h"
#include "rust-ast-full.h"
#include "rust-late-name-resolver-2.0.h"
#include "rust-default-resolver.h"
#include "rust-path.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver2_0 {

Late::Late (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

void
Late::setup_builtin_types ()
{
  auto next_id = [this] () { return ctx.mappings.get_next_hir_id (); };

  static const std::pair<std::string, TyTy::BaseType *> builtins[] = {
    {"u8", new TyTy::UintType (next_id (), TyTy::UintType::U8)},
    {"u16", new TyTy::UintType (next_id (), TyTy::UintType::U16)},
    {"u32", new TyTy::UintType (next_id (), TyTy::UintType::U32)},
    {"u64", new TyTy::UintType (next_id (), TyTy::UintType::U64)},
    {"u128", new TyTy::UintType (next_id (), TyTy::UintType::U128)},
    {"i8", new TyTy::IntType (next_id (), TyTy::IntType::I8)},
    {"i16", new TyTy::IntType (next_id (), TyTy::IntType::I16)},
    {"i32", new TyTy::IntType (next_id (), TyTy::IntType::I32)},
    {"i64", new TyTy::IntType (next_id (), TyTy::IntType::I64)},
    {"i128", new TyTy::IntType (next_id (), TyTy::IntType::I128)},
    {"f32", new TyTy::FloatType (next_id (), TyTy::FloatType::F32)},
    {"f64", new TyTy::FloatType (next_id (), TyTy::FloatType::F64)},
    {"usize", new TyTy::USizeType (next_id ())},
    {"isize", new TyTy::ISizeType (next_id ())},
    // missing char, str, never, ()
    // does name resolution play a part for this? or is it all at typechecking?
    // yeah it seems to be name resolution as well, which makes sense
  };

  for (const auto &builtin : builtins)
    {
      // we should be able to use `insert_at_root` or `insert` here, since we're
      // at the root :) hopefully!
      auto ok
	= ctx.types.insert (builtin.first, builtin.second->get_ref ()
			    /* FIXME: Invalid! This returns an *HirId* */);

      rust_assert (ok);
    }
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
  new_label (identifier.get_ident (), identifier.get_node_id ());
}

void
Late::visit (AST::IdentifierExpr &expr)
{
  // TODO: same thing as visit(PathInExpression) here?

  tl::optional<NodeId> resolved = tl::nullopt;
  auto label = ctx.labels.get (expr.get_ident ());
  auto value = ctx.values.get (expr.get_ident ());

  if (label)
    resolved = label;
  else if (value)
    resolved = value;
  // TODO: else emit error?

  ctx.map_usage (expr.get_node_id (), *resolved);

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

  auto label = ctx.labels.resolve_path (expr.get_segments ());
  auto value = ctx.values.resolve_path (expr.get_segments ());
}

void
Late::visit (AST::TypePath &type)
{
  // should we add type path resolution in `ForeverStack` directly? Since it's
  // quite more complicated.
  // maybe we can overload `resolve_path<Namespace::Types>` to only do
  // typepath-like path resolution? that sounds good

  auto resolved = ctx.types.get (type.get_segments ().back ()->as_string ());

  ctx.map_usage (type.get_node_id (), *resolved);
}

} // namespace Resolver2_0
} // namespace Rust
