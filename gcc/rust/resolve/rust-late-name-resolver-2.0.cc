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

#include "rust-ast-full.h"
#include "rust-late-name-resolver-2.0.h"
#include "rust-default-resolver.h"

namespace Rust {
namespace Resolver2_0 {

Late::Late (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

void
Late::go (AST::Crate &crate)
{
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

  auto label = ctx.labels.get (expr.get_ident ());
  auto value = ctx.values.get (expr.get_ident ());

  rust_debug ("[ARTHUR] label: %d", label ? *label : -1);
  rust_debug ("[ARTHUR] value: %d", value ? *value : -1);
}

void
Late::visit (AST::PathInExpression &expr)
{
  // TODO: How do we have a nice error with `can't capture dynamic environment
  // in a function item` error here?
  // do we emit it in `get<Namespace::Labels>`?

  auto label = ctx.labels.resolve_path (expr.get_segments ());

  auto value = ctx.values.resolve_path (expr.get_segments ());

  rust_debug ("[ARTHUR] label: %d", label ? *label : -1);
  rust_debug ("[ARTHUR] value: %d", value ? *value : -1);
}

} // namespace Resolver2_0
} // namespace Rust
