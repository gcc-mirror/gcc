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

#ifndef RUST_HIR_MAP_H
#define RUST_HIR_MAP_H

#include "rust-system.h"
#include "rust-location.h"

#include "rust-ast-full-decls.h"
#include "rust-hir-full-decls.h"

namespace Rust {

// refers to a Crate
typedef uint32_t CrateNum;
// refers to any node in the AST in current Crate
typedef uint32_t NodeId;
// refers to any node in the HIR for the current crate
typedef uint32_t HirId;
// refers to any top-level decl in HIR
typedef uint32_t LocalDefId;
// refers to <Crate><DefId>
typedef uint64_t DefId;

#define DEF_ID_CRATE_MASK 0xFFFFFFFF00000000
#define DEF_ID_LOCAL_DEF_MASK 0x00000000FFFFFFFF

#define UNKNOWN_CREATENUM ((uint32_t) (0))
#define UNKNOWN_NODEID ((uint32_t) (0))
#define UNKNOWN_HIRID ((uint32_t) (0))
#define UNKNOWN_LOCAL_DEFID ((uint32_t) (0))
#define UNKNOWN_DEFID ((uint64_t) (0))

namespace Analysis {

class NodeMapping
{
public:
  NodeMapping (CrateNum crateNum, NodeId nodeId, HirId hirId,
	       LocalDefId localDefId);
  ~NodeMapping ();

  static NodeMapping get_error ();

  CrateNum get_crate_num () const;
  NodeId get_nodeid () const;
  HirId get_hirid () const;
  LocalDefId get_local_defid () const;
  DefId get_defid () const;

  static DefId get_defid (CrateNum crate_num, LocalDefId local_defid);

  std::string as_string () const;

private:
  CrateNum crateNum;
  NodeId nodeId;
  HirId hirId;
  LocalDefId localDefId;
};

class Mappings
{
public:
  static Mappings *get ();
  ~Mappings ();

  CrateNum get_next_crate_num ();
  void set_current_crate (CrateNum crateNum);
  CrateNum get_current_crate () const;
  CrateNum setup_crate_mappings (std::string crate_name);

  bool get_crate_name (CrateNum crate_num, std::string &name) const
  {
    auto it = crate_names.find (crate_num);
    if (it == crate_names.end ())
      return false;

    name.assign (it->second);
    return true;
  }

  std::string get_current_crate_name () const
  {
    std::string name;
    bool ok = get_crate_name (get_current_crate (), name);
    rust_assert (ok);
    return name;
  }

  NodeId get_next_node_id () { return get_next_node_id (get_current_crate ()); }
  NodeId get_next_node_id (CrateNum crateNum);

  HirId get_next_hir_id () { return get_next_hir_id (get_current_crate ()); }
  HirId get_next_hir_id (CrateNum crateNum);

  LocalDefId get_next_localdef_id (CrateNum crateNum);

  AST::Crate *get_ast_crate (CrateNum crateNum);
  void insert_ast_crate (AST::Crate *crate);

  HIR::Crate *get_hir_crate (CrateNum crateNum);
  void insert_hir_crate (HIR::Crate *crate);

  void insert_defid_mapping (DefId id, HIR::Item *item);
  HIR::Item *lookup_defid (DefId id);

  void insert_local_defid_mapping (CrateNum crateNum, LocalDefId id,
				   HIR::Item *item);
  HIR::Item *lookup_local_defid (CrateNum crateNum, LocalDefId id);

  void insert_hir_item (CrateNum crateNum, HirId id, HIR::Item *item);
  HIR::Item *lookup_hir_item (CrateNum crateNum, HirId id);

  void insert_hir_impl_block (CrateNum crateNum, HirId id,
			      HIR::ImplBlock *item);
  HIR::ImplBlock *lookup_hir_impl_block (CrateNum crateNum, HirId id);

  void insert_hir_implitem (CrateNum crateNum, HirId id, HirId parent_impl_id,
			    HIR::ImplItem *item);
  HIR::ImplItem *lookup_hir_implitem (CrateNum crateNum, HirId id,
				      HirId *parent_impl_id);

  void insert_hir_expr (CrateNum crateNum, HirId id, HIR::Expr *expr);
  HIR::Expr *lookup_hir_expr (CrateNum crateNum, HirId id);

  void insert_hir_type (CrateNum crateNum, HirId id, HIR::Type *type);
  HIR::Type *lookup_hir_type (CrateNum crateNum, HirId id);

  void insert_hir_stmt (CrateNum crateNum, HirId id, HIR::Stmt *type);
  HIR::Stmt *lookup_hir_stmt (CrateNum crateNum, HirId id);

  void insert_hir_param (CrateNum crateNum, HirId id, HIR::FunctionParam *type);
  HIR::FunctionParam *lookup_hir_param (CrateNum crateNum, HirId id);

  void insert_hir_self_param (CrateNum crateNum, HirId id,
			      HIR::SelfParam *type);
  HIR::SelfParam *lookup_hir_self_param (CrateNum crateNum, HirId id);

  void insert_hir_struct_field (CrateNum crateNum, HirId id,
				HIR::StructExprField *type);
  HIR::StructExprField *lookup_hir_struct_field (CrateNum crateNum, HirId id);

  void walk_local_defids_for_crate (CrateNum crateNum,
				    std::function<bool (HIR::Item *)> cb);

  void insert_node_to_hir (CrateNum crate, NodeId id, HirId ref);
  bool lookup_node_to_hir (CrateNum crate, NodeId id, HirId *ref);

  void insert_location (CrateNum crate, HirId id, Location locus);
  Location lookup_location (CrateNum crate, HirId id);
  Location lookup_location (HirId id)
  {
    return lookup_location (get_current_crate (), id);
  }

  bool resolve_nodeid_to_stmt (CrateNum crate, NodeId id, HIR::Stmt **stmt);
  bool resolve_nodeid_to_stmt (NodeId id, HIR::Stmt **stmt)
  {
    return resolve_nodeid_to_stmt (get_current_crate (), id, stmt);
  }

  std::set<HirId> &get_hirids_within_crate (CrateNum crate)
  {
    return hirNodesWithinCrate[crate];
  }

  void insert_impl_item_mapping (HirId impl_item_id, HIR::ImplBlock *impl)
  {
    rust_assert (hirImplItemsToImplMappings.find (impl_item_id)
		 == hirImplItemsToImplMappings.end ());
    hirImplItemsToImplMappings[impl_item_id] = impl;
  }

  HIR::ImplBlock *lookup_associated_impl (HirId impl_item_id)
  {
    auto lookup = hirImplItemsToImplMappings.find (impl_item_id);
    rust_assert (lookup != hirImplItemsToImplMappings.end ());
    return lookup->second;
  }

  void iterate_impl_items (
    std::function<bool (HirId, HIR::ImplItem *, HIR::ImplBlock *)> cb);

  void iterate_impl_blocks (std::function<bool (HirId, HIR::ImplBlock *)> cb);

  bool is_impl_item (HirId id)
  {
    HirId parent_impl_block_id = UNKNOWN_HIRID;
    return lookup_hir_implitem (get_current_crate (), id, &parent_impl_block_id)
	   != nullptr;
  }

private:
  Mappings ();

  CrateNum crateNumItr = 0;
  CrateNum currentCrateNum;

  std::map<CrateNum, HirId> hirIdIter;
  std::map<CrateNum, NodeId> nodeIdIter;
  std::map<CrateNum, LocalDefId> localIdIter;

  std::map<CrateNum, AST::Crate *> astCrateMappings;
  std::map<CrateNum, HIR::Crate *> hirCrateMappings;

  std::map<DefId, HIR::Item *> defIdMappings;
  std::map<CrateNum, std::map<LocalDefId, HIR::Item *> > localDefIdMappings;
  std::map<CrateNum, std::map<HirId, HIR::Item *> > hirItemMappings;
  std::map<CrateNum, std::map<HirId, HIR::Type *> > hirTypeMappings;
  std::map<CrateNum, std::map<HirId, HIR::Expr *> > hirExprMappings;
  std::map<CrateNum, std::map<HirId, HIR::Stmt *> > hirStmtMappings;
  std::map<CrateNum, std::map<HirId, HIR::FunctionParam *> > hirParamMappings;
  std::map<CrateNum, std::map<HirId, HIR::StructExprField *> >
    hirStructFieldMappings;
  std::map<CrateNum, std::map<HirId, std::pair<HirId, HIR::ImplItem *> > >
    hirImplItemMappings;
  std::map<CrateNum, std::map<HirId, HIR::SelfParam *> > hirSelfParamMappings;
  std::map<HirId, HIR::ImplBlock *> hirImplItemsToImplMappings;
  std::map<CrateNum, std::map<HirId, HIR::ImplBlock *> > hirImplBlockMappings;

  // location info
  std::map<CrateNum, std::map<NodeId, Location> > locations;

  // reverse mappings
  std::map<CrateNum, std::map<NodeId, HirId> > nodeIdToHirMappings;

  // all hirid nodes
  std::map<CrateNum, std::set<HirId> > hirNodesWithinCrate;

  // crate names
  std::map<CrateNum, std::string> crate_names;
};

} // namespace Analysis
} // namespace Rust

#endif // RUST_HIR_MAP_H
