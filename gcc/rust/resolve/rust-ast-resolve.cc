// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "rust-ast-resolve.h"
#include "rust-ast-full.h"
#include "rust-tyty.h"
#include "rust-ast-resolve-toplevel.h"
#include "rust-ast-resolve-item.h"
#include "rust-ast-resolve-expr.h"

#define MKBUILTIN_TYPE(_X, _R, _TY)                                            \
  do                                                                           \
    {                                                                          \
      AST::PathIdentSegment seg (_X);                                          \
      auto typePath = ::std::unique_ptr<AST::TypePathSegment> (                \
	new AST::TypePathSegment (::std::move (seg), false,                    \
				  Linemap::predeclared_location ()));          \
      ::std::vector< ::std::unique_ptr<AST::TypePathSegment> > segs;           \
      segs.push_back (::std::move (typePath));                                 \
      auto builtin_type                                                        \
	= new AST::TypePath (::std::move (segs),                               \
			     Linemap::predeclared_location (), false);         \
      _R.push_back (builtin_type);                                             \
      tyctx->insert_builtin (_TY->get_ref (), builtin_type->get_node_id (),    \
			     _TY);                                             \
    }                                                                          \
  while (0)

namespace Rust {
namespace Resolver {

// Resolver

Resolver::Resolver ()
  : mappings (Analysis::Mappings::get ()), tyctx (TypeCheckContext::get ()),
    name_scope (Scope (mappings->get_current_crate ())),
    type_scope (Scope (mappings->get_current_crate ()))
{
  generate_builtins ();
}

Resolver *
Resolver::get ()
{
  static Resolver *instance;
  if (instance == nullptr)
    instance = new Resolver ();

  return instance;
}

void
Resolver::push_new_name_rib (Rib *r)
{
  rust_assert (name_ribs.find (r->get_node_id ()) == name_ribs.end ());
  name_ribs[r->get_node_id ()] = r;
}

void
Resolver::push_new_type_rib (Rib *r)
{
  if (type_ribs.size () == 0)
    global_type_node_id = r->get_node_id ();

  rust_assert (type_ribs.find (r->get_node_id ()) == type_ribs.end ());
  type_ribs[r->get_node_id ()] = r;
}

bool
Resolver::find_name_rib (NodeId id, Rib **rib)
{
  auto it = name_ribs.find (id);
  if (it == name_ribs.end ())
    return false;

  *rib = it->second;
  return true;
}

bool
Resolver::find_type_rib (NodeId id, Rib **rib)
{
  auto it = type_ribs.find (id);
  if (it == type_ribs.end ())
    return false;

  *rib = it->second;
  return true;
}

void
Resolver::insert_builtin_types (Rib *r)
{
  auto builtins = get_builtin_types ();
  for (auto it = builtins.begin (); it != builtins.end (); it++)
    r->insert_name ((*it)->as_string (), (*it)->get_node_id ());
}

std::vector<AST::TypePath *> &
Resolver::get_builtin_types ()
{
  return builtins;
}

void
Resolver::generate_builtins ()
{
  auto u8
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U8);
  auto u16
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U16);
  auto u32
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U32);
  auto u64
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U64);
  auto u128
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U128);
  auto i8 = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I8);
  auto i16
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I16);
  auto i32
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I32);
  auto i64
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I64);
  auto i128
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I128);
  auto rbool = new TyTy::BoolType (mappings->get_next_hir_id ());

  MKBUILTIN_TYPE ("u8", builtins, u8);
  MKBUILTIN_TYPE ("u16", builtins, u16);
  MKBUILTIN_TYPE ("u32", builtins, u32);
  MKBUILTIN_TYPE ("u64", builtins, u64);
  MKBUILTIN_TYPE ("u128", builtins, u128);
  MKBUILTIN_TYPE ("i8", builtins, i8);
  MKBUILTIN_TYPE ("i16", builtins, i16);
  MKBUILTIN_TYPE ("i32", builtins, i32);
  MKBUILTIN_TYPE ("i64", builtins, i64);
  MKBUILTIN_TYPE ("i128", builtins, i128);
  MKBUILTIN_TYPE ("bool", builtins, rbool);
}

void
Resolver::insert_new_definition (NodeId id, Definition def)
{
  auto it = name_definitions.find (id);
  rust_assert (it == name_definitions.end ());

  name_definitions[id] = def;
}

bool
Resolver::lookup_definition (NodeId id, Definition *def)
{
  auto it = name_definitions.find (id);
  if (it == name_definitions.end ())
    return false;

  *def = it->second;
  return true;
}

void
Resolver::insert_resolved_name (NodeId refId, NodeId defId)
{
  auto it = resolved_names.find (refId);
  rust_assert (it == resolved_names.end ());

  resolved_names[refId] = defId;
  get_name_scope ().peek ()->append_reference_for_def (defId, refId);
}

bool
Resolver::lookup_resolved_name (NodeId refId, NodeId *defId)
{
  auto it = resolved_names.find (refId);
  if (it == resolved_names.end ())
    return false;

  *defId = it->second;
  return true;
}

void
Resolver::insert_resolved_type (NodeId refId, NodeId defId)
{
  auto it = resolved_types.find (refId);
  rust_assert (it == resolved_types.end ());

  resolved_types[refId] = defId;
  get_type_scope ().peek ()->append_reference_for_def (defId, refId);
}

bool
Resolver::lookup_resolved_type (NodeId refId, NodeId *defId)
{
  auto it = resolved_types.find (refId);
  if (it == resolved_types.end ())
    return false;

  *defId = it->second;
  return true;
}

// NameResolution

NameResolution *
NameResolution::get ()
{
  static NameResolution *instance;
  if (instance == nullptr)
    instance = new NameResolution ();

  return instance;
}

NameResolution::NameResolution ()
  : resolver (Resolver::get ()), mappings (Analysis::Mappings::get ())
{
  // these are global
  resolver->get_type_scope ().push (mappings->get_next_node_id ());
  resolver->insert_builtin_types (resolver->get_type_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
}

void
NameResolution::Resolve (AST::Crate &crate)
{
  auto resolver = get ();
  resolver->go (crate);
}

void
NameResolution::go (AST::Crate &crate)
{
  // setup parent scoping for names
  resolver->get_name_scope ().push (crate.get_node_id ());
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  // setup parent scoping for new types
  resolver->get_type_scope ().push (mappings->get_next_node_id ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

  // first gather the top-level namespace names then we drill down
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    ResolveTopLevel::go (it->get ());

  // next we can drill down into the items and their scopes
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    ResolveItem::go (it->get ());
}

// rust-ast-resolve-expr.h

void
ResolveExpr::visit (AST::BlockExpr &expr)
{
  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

  expr.iterate_stmts ([&] (AST::Stmt *s) mutable -> bool {
    ResolveStmt::go (s, s->get_node_id ());
    return true;
  });

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
}

// rust-ast-resolve-struct-expr-field.h

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifierValue &field)
{
  ResolveExpr::go (field.get_value ().get (), field.get_node_id ());
}

} // namespace Resolver
} // namespace Rust
