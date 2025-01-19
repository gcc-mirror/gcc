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

#include "rust-name-resolver.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

Rib::Rib (CrateNum crateNum, NodeId node_id)
  : crate_num (crateNum), node_id (node_id)
{}

void
Rib::insert_name (
  const CanonicalPath &path, NodeId id, location_t locus, bool shadow,
  ItemType type,
  std::function<void (const CanonicalPath &, NodeId, location_t)> dup_cb)
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
  reverse_path_mappings.insert ({id, path});
  decls_within_rib.insert ({id, locus});
  references[id] = {};
  decl_type_mappings.insert ({id, type});
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

bool
Rib::lookup_decl_type (NodeId def, ItemType *type) const
{
  auto it = decl_type_mappings.find (def);
  if (it == decl_type_mappings.end ())
    return false;

  *type = it->second;
  return true;
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
  const CanonicalPath &ident, NodeId id, location_t locus, bool shadow,
  Rib::ItemType type,
  std::function<void (const CanonicalPath &, NodeId, location_t)> dup_cb)
{
  peek ()->insert_name (ident, id, locus, shadow, type, dup_cb);
}

void
Scope::insert (const CanonicalPath &ident, NodeId id, location_t locus,
	       Rib::ItemType type)
{
  peek ()->insert_name (ident, id, locus, true, type,
			[] (const CanonicalPath &, NodeId, location_t) -> void {
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

bool
Scope::lookup_decl_type (NodeId id, Rib::ItemType *type)
{
  bool found = false;
  iterate ([&] (const Rib *r) -> bool {
    if (r->decl_was_declared_here (id))
      {
	bool ok = r->lookup_decl_type (id, type);
	rust_assert (ok);
	found = true;
	return false;
      }
    return true;
  });
  return found;
}

bool
Scope::lookup_rib_for_decl (NodeId id, const Rib **rib)
{
  bool found = false;
  iterate ([&] (const Rib *r) -> bool {
    if (r->decl_was_declared_here (id))
      {
	*rib = r;
	found = true;
	return false;
      }
    return true;
  });
  return found;
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
      r->insert_name (builtin_path, builtin->get_node_id (), BUILTINS_LOCATION,
		      false, Rib::ItemType::Type,
		      [] (const CanonicalPath &, NodeId, location_t) -> void {
		      });
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

  setup_builtin ("u8", u8);
  setup_builtin ("u16", u16);
  setup_builtin ("u32", u32);
  setup_builtin ("u64", u64);
  setup_builtin ("u128", u128);
  setup_builtin ("i8", i8);
  setup_builtin ("i16", i16);
  setup_builtin ("i32", i32);
  setup_builtin ("i64", i64);
  setup_builtin ("i128", i128);
  setup_builtin ("bool", rbool);
  setup_builtin ("f32", f32);
  setup_builtin ("f64", f64);
  setup_builtin ("usize", usize);
  setup_builtin ("isize", isize);
  setup_builtin ("char", char_tyty);
  setup_builtin ("str", str);
  setup_builtin ("!", never);

  // unit type ()
  TyTy::TupleType *unit_tyty
    = TyTy::TupleType::get_unit_type (mappings->get_next_hir_id ());
  std::vector<std::unique_ptr<AST::Type> > elems;
  AST::TupleType *unit_type
    = new AST::TupleType (std::move (elems), BUILTINS_LOCATION);
  builtins.push_back (unit_type);
  tyctx->insert_builtin (unit_tyty->get_ref (), unit_type->get_node_id (),
			 unit_tyty);
  set_unit_type_node_id (unit_type->get_node_id ());
}

void
Resolver::setup_builtin (const std::string &name, TyTy::BaseType *tyty)
{
  AST::PathIdentSegment seg (name, BUILTINS_LOCATION);
  auto typePath = ::std::unique_ptr<AST::TypePathSegment> (
    new AST::TypePathSegment (::std::move (seg), false, BUILTINS_LOCATION));
  ::std::vector< ::std::unique_ptr<AST::TypePathSegment> > segs;
  segs.push_back (::std::move (typePath));
  auto builtin_type
    = new AST::TypePath (::std::move (segs), BUILTINS_LOCATION, false);
  builtins.push_back (builtin_type);
  tyctx->insert_builtin (tyty->get_ref (), builtin_type->get_node_id (), tyty);
  mappings->insert_node_to_hir (builtin_type->get_node_id (), tyty->get_ref ());
  mappings->insert_canonical_path (
    builtin_type->get_node_id (),
    CanonicalPath::new_seg (builtin_type->get_node_id (), name));
}

void
Resolver::insert_resolved_name (NodeId refId, NodeId defId)
{
  resolved_names[refId] = defId;
  get_name_scope ().append_reference_for_def (refId, defId);
  insert_captured_item (defId);
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

void
Resolver::insert_resolved_misc (NodeId refId, NodeId defId)
{
  auto it = misc_resolved_items.find (refId);
  rust_assert (it == misc_resolved_items.end ());

  misc_resolved_items[refId] = defId;
}

bool
Resolver::lookup_resolved_misc (NodeId refId, NodeId *defId)
{
  auto it = misc_resolved_items.find (refId);
  if (it == misc_resolved_items.end ())
    return false;

  *defId = it->second;
  return true;
}

void
Resolver::push_closure_context (NodeId closure_expr_id)
{
  auto it = closures_capture_mappings.find (closure_expr_id);
  rust_assert (it == closures_capture_mappings.end ());

  closures_capture_mappings.insert ({closure_expr_id, {}});
  closure_context.push_back (closure_expr_id);
}

void
Resolver::pop_closure_context ()
{
  rust_assert (!closure_context.empty ());
  closure_context.pop_back ();
}

void
Resolver::insert_captured_item (NodeId id)
{
  // nothing to do unless we are in a closure context
  if (closure_context.empty ())
    return;

  // check that this is a VAR_DECL?
  Scope &name_scope = get_name_scope ();
  Rib::ItemType type = Rib::ItemType::Unknown;
  bool found = name_scope.lookup_decl_type (id, &type);
  if (!found)
    return;

  // RIB Function { let a, let b } id = 1;
  //   RIB Closure { let c } id = 2;
  //     RIB IfStmt { <bind a>} id = 3;
  //   RIB ... { ... } id = 4
  //
  // if we have a resolved_node_id of 'a' and the current rib is '3' we know
  // this is binding exists in a rib with id < the closure rib id, other wise
  // its just a normal binding and we don't care
  //
  // Problem the node id's dont work like this because the inner most items are
  // created first so this means the root will have a larger id and a simple
  // less than or greater than check wont work for more complex scoping cases
  // but we can use our current rib context to figure this out by checking if
  // the rib id the decl we care about exists prior to the rib for the closure
  // id

  const Rib *r = nullptr;
  bool ok = name_scope.lookup_rib_for_decl (id, &r);
  rust_assert (ok);
  NodeId decl_rib_node_id = r->get_node_id ();

  // iterate the closure context and add in the mapping for all to handle the
  // case of nested closures
  for (auto &closure_expr_id : closure_context)
    {
      if (!decl_needs_capture (decl_rib_node_id, closure_expr_id, name_scope))
	continue;

      // is this a valid binding to take
      bool is_var_decl_p = type == Rib::ItemType::Var;
      if (!is_var_decl_p)
	{
	  // FIXME is this an error case?
	  return;
	}

      // append it to the context info
      auto it = closures_capture_mappings.find (closure_expr_id);
      rust_assert (it != closures_capture_mappings.end ());

      it->second.insert (id);
    }
}

bool
Resolver::decl_needs_capture (NodeId decl_rib_node_id,
			      NodeId closure_rib_node_id, const Scope &scope)
{
  for (const auto &rib : scope.get_context ())
    {
      bool rib_is_closure = rib->get_node_id () == closure_rib_node_id;
      bool rib_is_decl = rib->get_node_id () == decl_rib_node_id;
      if (rib_is_closure)
	return false;
      else if (rib_is_decl)
	return true;
    }
  return false;
}

const std::set<NodeId> &
Resolver::get_captures (NodeId id) const
{
  auto it = closures_capture_mappings.find (id);
  rust_assert (it != closures_capture_mappings.end ());
  return it->second;
}

} // namespace Resolver
} // namespace Rust
