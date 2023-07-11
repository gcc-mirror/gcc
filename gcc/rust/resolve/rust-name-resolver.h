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

#ifndef RUST_NAME_RESOLVER_H
#define RUST_NAME_RESOLVER_H

#include "rust-system.h"
#include "rust-canonical-path.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

class Rib
{
public:
  enum ItemType
  {
    Var,
    Param,
    Function,
    Type,
    Module,
    Static,
    Const,
    Trait,
    Impl,
    TraitImpl,
    ExternCrate,
    MacroDecl,
    Label,
    Unknown
  };

  // FIXME
  // Rust uses local_def_ids assigned by def_collector on the AST. Consider
  // moving to a local-def-id
  Rib (CrateNum crateNum, NodeId node_id);

  // this takes the relative paths of items within a compilation unit for lookup
  void insert_name (
    const CanonicalPath &path, NodeId id, location_t locus, bool shadow,
    ItemType type,
    std::function<void (const CanonicalPath &, NodeId, location_t)> dup_cb);

  bool lookup_canonical_path (const NodeId &id, CanonicalPath *ident);
  bool lookup_name (const CanonicalPath &ident, NodeId *id);
  void clear_name (const CanonicalPath &ident, NodeId id);
  void append_reference_for_def (NodeId def, NodeId ref);
  bool have_references_for_node (NodeId def) const;
  bool decl_was_declared_here (NodeId def) const;
  bool lookup_decl_type (NodeId def, ItemType *type) const;
  void debug () const;
  std::string debug_str () const;

  CrateNum get_crate_num () const { return crate_num; }
  NodeId get_node_id () const { return node_id; }
  std::map<NodeId, location_t> &get_declarations () { return decls_within_rib; }

private:
  CrateNum crate_num;
  NodeId node_id;
  std::map<CanonicalPath, NodeId> path_mappings;
  std::map<NodeId, CanonicalPath> reverse_path_mappings;
  std::map<NodeId, location_t> decls_within_rib;
  std::map<NodeId, std::set<NodeId>> references;
  std::map<NodeId, ItemType> decl_type_mappings;
};

class Scope
{
public:
  Scope (CrateNum crate_num);

  void insert (
    const CanonicalPath &ident, NodeId id, location_t locus, bool shadow,
    Rib::ItemType type,
    std::function<void (const CanonicalPath &, NodeId, location_t)> dup_cb);

  void insert (const CanonicalPath &ident, NodeId id, location_t locus,
	       Rib::ItemType type = Rib::ItemType::Unknown);
  bool lookup (const CanonicalPath &ident, NodeId *id);
  bool lookup_decl_type (NodeId id, Rib::ItemType *type);
  bool lookup_rib_for_decl (NodeId id, const Rib **rib);

  void iterate (std::function<bool (Rib *)> cb);
  void iterate (std::function<bool (const Rib *)> cb) const;

  Rib *peek ();
  void push (NodeId id);
  Rib *pop ();

  bool decl_was_declared_here (NodeId def) const;
  void append_reference_for_def (NodeId refId, NodeId defId);

  CrateNum get_crate_num () const { return crate_num; }

  const std::vector<Rib *> &get_context () const { return stack; };

private:
  CrateNum crate_num;
  std::vector<Rib *> stack;
};

class Resolver
{
public:
  static Resolver *get ();
  ~Resolver () {}

  // these builtin types
  void insert_builtin_types (Rib *r);

  // these will be required for type resolution passes to
  // map back to tyty nodes
  std::vector<AST::Type *> &get_builtin_types ();

  void push_new_name_rib (Rib *r);
  void push_new_type_rib (Rib *r);
  void push_new_label_rib (Rib *r);
  void push_new_macro_rib (Rib *r);

  bool find_name_rib (NodeId id, Rib **rib);
  bool find_type_rib (NodeId id, Rib **rib);
  bool find_label_rib (NodeId id, Rib **rib);
  bool find_macro_rib (NodeId id, Rib **rib);

  void insert_resolved_name (NodeId refId, NodeId defId);
  bool lookup_resolved_name (NodeId refId, NodeId *defId);

  void insert_resolved_type (NodeId refId, NodeId defId);
  bool lookup_resolved_type (NodeId refId, NodeId *defId);

  void insert_resolved_label (NodeId refId, NodeId defId);
  bool lookup_resolved_label (NodeId refId, NodeId *defId);

  void insert_resolved_macro (NodeId refId, NodeId defId);
  bool lookup_resolved_macro (NodeId refId, NodeId *defId);

  void insert_resolved_misc (NodeId refId, NodeId defId);
  bool lookup_resolved_misc (NodeId refId, NodeId *defId);

  // proxy for scoping
  Scope &get_name_scope () { return name_scope; }
  Scope &get_type_scope () { return type_scope; }
  Scope &get_label_scope () { return label_scope; }
  Scope &get_macro_scope () { return macro_scope; }

  NodeId get_global_type_node_id () { return global_type_node_id; }
  void set_unit_type_node_id (NodeId id) { unit_ty_node_id = id; }
  NodeId get_unit_type_node_id () { return unit_ty_node_id; }

  void push_new_module_scope (NodeId module_id)
  {
    current_module_stack.push_back (module_id);
  }

  void pop_module_scope ()
  {
    rust_assert (!current_module_stack.empty ());
    current_module_stack.pop_back ();
  }

  NodeId peek_current_module_scope () const
  {
    rust_assert (!current_module_stack.empty ());
    return current_module_stack.back ();
  }

  NodeId peek_crate_module_scope () const
  {
    rust_assert (!current_module_stack.empty ());
    return current_module_stack.front ();
  }

  NodeId peek_parent_module_scope () const
  {
    rust_assert (current_module_stack.size () > 1);
    return current_module_stack.at (current_module_stack.size () - 2);
  }

  void push_closure_context (NodeId closure_expr_id);
  void pop_closure_context ();
  void insert_captured_item (NodeId id);
  const std::set<NodeId> &get_captures (NodeId id) const;

protected:
  bool decl_needs_capture (NodeId decl_rib_node_id, NodeId closure_rib_node_id,
			   const Scope &scope);

private:
  Resolver ();

  void generate_builtins ();
  void setup_builtin (const std::string &name, TyTy::BaseType *tyty);

  Analysis::Mappings *mappings;
  TypeCheckContext *tyctx;

  std::vector<AST::Type *> builtins;

  Scope name_scope;
  Scope type_scope;
  Scope label_scope;
  Scope macro_scope;

  NodeId global_type_node_id;
  NodeId unit_ty_node_id;

  // map a AST Node to a Rib
  std::map<NodeId, Rib *> name_ribs;
  std::map<NodeId, Rib *> type_ribs;
  std::map<NodeId, Rib *> label_ribs;
  std::map<NodeId, Rib *> macro_ribs;

  // Rust uses DefIds to namespace these under a crate_num
  // but then it uses the def_collector to assign local_defids
  // to each ast node as well. not sure if this is going to fit
  // with gcc very well to compile a full crate in one go but we will
  // see.

  // these are of the form ref->Def-NodeId
  // we need two namespaces one for names and ones for types
  std::map<NodeId, NodeId> resolved_names;
  std::map<NodeId, NodeId> resolved_types;
  std::map<NodeId, NodeId> resolved_labels;
  std::map<NodeId, NodeId> resolved_macros;

  // misc
  std::map<NodeId, NodeId> misc_resolved_items;

  // keep track of the current module scope ids
  std::vector<NodeId> current_module_stack;

  // captured variables mappings
  std::vector<NodeId> closure_context;
  std::map<NodeId, std::set<NodeId>> closures_capture_mappings;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_NAME_RESOLVER_H
