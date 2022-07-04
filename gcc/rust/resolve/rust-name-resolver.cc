// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-name-resolver.h"
#include "rust-ast-full.h"

#define MKBUILTIN_TYPE(_X, _R, _TY)                                            \
  do                                                                           \
    {                                                                          \
      AST::PathIdentSegment seg (_X, Linemap::predeclared_location ());        \
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
      mappings->insert_node_to_hir (builtin_type->get_node_id (),              \
				    _TY->get_ref ());                          \
      mappings->insert_canonical_path (                                        \
	builtin_type->get_node_id (),                                          \
	CanonicalPath::new_seg (builtin_type->get_node_id (), _X));            \
    }                                                                          \
  while (0)

namespace Rust {
namespace Resolver {

Rib::Rib (CrateNum crateNum, NodeId node_id)
  : crate_num (crateNum), node_id (node_id),
    mappings (Analysis::Mappings::get ())
{}

void
Rib::insert_name (
  const CanonicalPath &path, NodeId id, Location locus, bool shadow,
  std::function<void (const CanonicalPath &, NodeId, Location)> dup_cb)
{
  auto it = path_mappings.find (path);
  bool path_already_exists = it != path_mappings.end ();
  if (path_already_exists && !shadow)
    {
      const auto &decl = decls_within_rib.find (it->second);
      if (decl != decls_within_rib.end ())
	dup_cb (path, it->second, decl->second);
      else
	dup_cb (path, it->second, locus);

      return;
    }

  path_mappings[path] = id;
  reverse_path_mappings.insert (std::pair<NodeId, CanonicalPath> (id, path));
  decls_within_rib.insert (std::pair<NodeId, Location> (id, locus));
  references[id] = {};
}

bool
Rib::lookup_name (const CanonicalPath &ident, NodeId *id)
{
  auto it = path_mappings.find (ident);
  if (it == path_mappings.end ())
    return false;

  *id = it->second;
  return true;
}

void
Rib::clear_name (const CanonicalPath &ident, NodeId id)
{
  auto ii = path_mappings.find (ident);
  if (ii != path_mappings.end ())
    path_mappings.erase (ii);

  auto ij = reverse_path_mappings.find (id);
  if (ij != reverse_path_mappings.end ())
    reverse_path_mappings.erase (ij);

  auto ik = decls_within_rib.find (id);
  if (ik != decls_within_rib.end ())
    decls_within_rib.erase (ik);
}

void
Rib::append_reference_for_def (NodeId def, NodeId ref)
{
  references[def].insert (ref);
}

bool
Rib::have_references_for_node (NodeId def) const
{
  auto it = references.find (def);
  if (it == references.end ())
    return false;

  return !it->second.empty ();
}

bool
Rib::decl_was_declared_here (NodeId def) const
{
  for (auto &it : decls_within_rib)
    {
      if (it.first == def)
	return true;
    }
  return false;
}

void
Rib::debug () const
{
  fprintf (stderr, "%s\n", debug_str ().c_str ());
}

std::string
Rib::debug_str () const
{
  std::string buffer;
  for (const auto &it : path_mappings)
    {
      buffer += it.first.get () + "=" + std::to_string (it.second);
      buffer += ",";
    }
  return "{" + buffer + "}";
}

Scope::Scope (CrateNum crate_num) : crate_num (crate_num) {}

void
Scope::insert (
  const CanonicalPath &ident, NodeId id, Location locus, bool shadow,
  std::function<void (const CanonicalPath &, NodeId, Location)> dup_cb)
{
  peek ()->insert_name (ident, id, locus, shadow, dup_cb);
}

void
Scope::insert (const CanonicalPath &ident, NodeId id, Location locus)
{
  peek ()->insert_name (ident, id, locus, true,
			[] (const CanonicalPath &, NodeId, Location) -> void {
			});
}

bool
Scope::lookup (const CanonicalPath &ident, NodeId *id)
{
  NodeId lookup = UNKNOWN_NODEID;
  iterate ([&] (Rib *r) mutable -> bool {
    if (r->lookup_name (ident, &lookup))
      return false;
    return true;
  });

  *id = lookup;
  return lookup != UNKNOWN_NODEID;
}

void
Scope::iterate (std::function<bool (Rib *)> cb)
{
  for (auto it = stack.rbegin (); it != stack.rend (); ++it)
    {
      if (!cb (*it))
	return;
    }
}

void
Scope::iterate (std::function<bool (const Rib *)> cb) const
{
  for (auto it = stack.rbegin (); it != stack.rend (); ++it)
    {
      if (!cb (*it))
	return;
    }
}

Rib *
Scope::peek ()
{
  return stack.back ();
}

void
Scope::push (NodeId id)
{
  stack.push_back (new Rib (get_crate_num (), id));
}

Rib *
Scope::pop ()
{
  Rib *r = peek ();
  stack.pop_back ();
  return r;
}

void
Scope::append_reference_for_def (NodeId refId, NodeId defId)
{
  bool ok = false;
  iterate ([&] (Rib *r) mutable -> bool {
    if (r->decl_was_declared_here (defId))
      {
	ok = true;
	r->append_reference_for_def (defId, refId);
      }
    return true;
  });
  rust_assert (ok);
}

bool
Scope::decl_was_declared_here (NodeId def) const
{
  bool found = false;
  iterate ([&] (const Rib *r) -> bool {
    if (r->decl_was_declared_here (def))
      {
	found = true;
	return false;
      }
    return true;
  });
  return found;
}

Resolver::Resolver ()
  : mappings (Analysis::Mappings::get ()), tyctx (TypeCheckContext::get ()),
    name_scope (Scope (mappings->get_current_crate ())),
    type_scope (Scope (mappings->get_current_crate ())),
    label_scope (Scope (mappings->get_current_crate ())),
    macro_scope (Scope (mappings->get_current_crate ())),
    global_type_node_id (UNKNOWN_NODEID), unit_ty_node_id (UNKNOWN_NODEID)
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

void
Resolver::push_new_label_rib (Rib *r)
{
  rust_assert (label_ribs.find (r->get_node_id ()) == label_ribs.end ());
  label_ribs[r->get_node_id ()] = r;
}

void
Resolver::push_new_macro_rib (Rib *r)
{
  rust_assert (label_ribs.find (r->get_node_id ()) == label_ribs.end ());
  macro_ribs[r->get_node_id ()] = r;
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

bool
Resolver::find_macro_rib (NodeId id, Rib **rib)
{
  auto it = macro_ribs.find (id);
  if (it == macro_ribs.end ())
    return false;

  *rib = it->second;
  return true;
}

void
Resolver::insert_builtin_types (Rib *r)
{
  auto builtins = get_builtin_types ();
  for (auto &builtin : builtins)
    {
      CanonicalPath builtin_path
	= CanonicalPath::new_seg (builtin->get_node_id (),
				  builtin->as_string ());
      r->insert_name (builtin_path, builtin->get_node_id (),
		      Linemap::predeclared_location (), false,
		      [] (const CanonicalPath &, NodeId, Location) -> void {});
    }
}

std::vector<AST::Type *> &
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
  auto f32
    = new TyTy::FloatType (mappings->get_next_hir_id (), TyTy::FloatType::F32);
  auto f64
    = new TyTy::FloatType (mappings->get_next_hir_id (), TyTy::FloatType::F64);
  auto usize = new TyTy::USizeType (mappings->get_next_hir_id ());
  auto isize = new TyTy::ISizeType (mappings->get_next_hir_id ());
  auto char_tyty = new TyTy::CharType (mappings->get_next_hir_id ());
  auto str = new TyTy::StrType (mappings->get_next_hir_id ());
  auto never = new TyTy::NeverType (mappings->get_next_hir_id ());

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
  MKBUILTIN_TYPE ("f32", builtins, f32);
  MKBUILTIN_TYPE ("f64", builtins, f64);
  MKBUILTIN_TYPE ("usize", builtins, usize);
  MKBUILTIN_TYPE ("isize", builtins, isize);
  MKBUILTIN_TYPE ("char", builtins, char_tyty);
  MKBUILTIN_TYPE ("str", builtins, str);
  MKBUILTIN_TYPE ("!", builtins, never);

  // unit type ()
  TyTy::TupleType *unit_tyty
    = TyTy::TupleType::get_unit_type (mappings->get_next_hir_id ());
  std::vector<std::unique_ptr<AST::Type> > elems;
  AST::TupleType *unit_type
    = new AST::TupleType (std::move (elems), Linemap::predeclared_location ());
  builtins.push_back (unit_type);
  tyctx->insert_builtin (unit_tyty->get_ref (), unit_type->get_node_id (),
			 unit_tyty);
  set_unit_type_node_id (unit_type->get_node_id ());
}

void
Resolver::insert_resolved_name (NodeId refId, NodeId defId)
{
  resolved_names[refId] = defId;
  get_name_scope ().append_reference_for_def (refId, defId);
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
  // auto it = resolved_types.find (refId);
  // rust_assert (it == resolved_types.end ());

  resolved_types[refId] = defId;
  get_type_scope ().append_reference_for_def (refId, defId);
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

void
Resolver::insert_resolved_label (NodeId refId, NodeId defId)
{
  auto it = resolved_labels.find (refId);
  rust_assert (it == resolved_labels.end ());

  resolved_labels[refId] = defId;
  get_label_scope ().append_reference_for_def (refId, defId);
}

bool
Resolver::lookup_resolved_label (NodeId refId, NodeId *defId)
{
  auto it = resolved_labels.find (refId);
  if (it == resolved_labels.end ())
    return false;

  *defId = it->second;
  return true;
}

void
Resolver::insert_resolved_macro (NodeId refId, NodeId defId)
{
  auto it = resolved_macros.find (refId);
  rust_assert (it == resolved_macros.end ());

  resolved_labels[refId] = defId;
  get_label_scope ().append_reference_for_def (refId, defId);
}

bool
Resolver::lookup_resolved_macro (NodeId refId, NodeId *defId)
{
  auto it = resolved_macros.find (refId);
  if (it == resolved_macros.end ())
    return false;

  *defId = it->second;
  return true;
}

} // namespace Resolver
} // namespace Rust
