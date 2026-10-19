// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

class Resolver
{
public:
  static Resolver *get ();
  ~Resolver () {}

  void insert_resolved_name (NodeId refId, NodeId defId);
  bool lookup_resolved_name (NodeId refId, NodeId *defId);

  void insert_resolved_type (NodeId refId, NodeId defId);
  bool lookup_resolved_type (NodeId refId, NodeId *defId);

  void insert_resolved_misc (NodeId refId, NodeId defId);
  bool lookup_resolved_misc (NodeId refId, NodeId *defId);

private:
  Resolver ();
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_NAME_RESOLVER_H
