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

#ifndef RUST_HIR_MAP_H
#define RUST_HIR_MAP_H

#include "rust-optional.h"
#include "rust-system.h"
#include "rust-location.h"
#include "rust-mapping-common.h"
#include "rust-canonical-path.h"
#include "rust-ast-full-decls.h"
#include "rust-hir-full-decls.h"
#include "rust-lang-item.h"
#include "rust-privacy-common.h"

namespace Rust {
namespace Analysis {

class NodeMapping
{
public:
  NodeMapping (CrateNum crateNum, NodeId nodeId, HirId hirId,
	       LocalDefId localDefId)
    : crateNum (crateNum), nodeId (nodeId), hirId (hirId),
      localDefId (localDefId)
  {}

  static NodeMapping get_error ();

  CrateNum get_crate_num () const;
  NodeId get_nodeid () const;
  HirId get_hirid () const;
  LocalDefId get_local_defid () const;
  DefId get_defid () const;

  static DefId get_defid (CrateNum crate_num, LocalDefId local_defid);

  std::string as_string () const;

  bool is_equal (const NodeMapping &other) const
  {
    return get_crate_num () == other.get_crate_num ()
	   && get_nodeid () == other.get_nodeid ()
	   && get_hirid () == other.get_hirid ()
	   && get_local_defid () == other.get_local_defid ();
  }

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

  CrateNum get_next_crate_num (const std::string &name);
  void set_current_crate (CrateNum crateNum);
  CrateNum get_current_crate () const;
  bool get_crate_name (CrateNum crate_num, std::string &name) const;
  void set_crate_name (CrateNum crate_num, const std::string &name);
  std::string get_current_crate_name () const;
  bool lookup_crate_name (const std::string &crate_name,
			  CrateNum &resolved_crate_num) const;
  bool crate_num_to_nodeid (const CrateNum &crate_num, NodeId &node_id) const;
  bool node_is_crate (NodeId node_id) const;

  NodeId get_next_node_id ();
  HirId get_next_hir_id () { return get_next_hir_id (get_current_crate ()); }
  HirId get_next_hir_id (CrateNum crateNum);
  LocalDefId get_next_localdef_id ()
  {
    return get_next_localdef_id (get_current_crate ());
  }
  LocalDefId get_next_localdef_id (CrateNum crateNum);

  AST::Crate &get_ast_crate (CrateNum crateNum);
  AST::Crate &get_ast_crate_by_node_id (NodeId id);
  AST::Crate &insert_ast_crate (std::unique_ptr<AST::Crate> &&crate,
				CrateNum crate_num);
  HIR::Crate &insert_hir_crate (std::unique_ptr<HIR::Crate> &&crate);
  HIR::Crate &get_hir_crate (CrateNum crateNum);
  bool is_local_hirid_crate (HirId crateNum);

  void insert_defid_mapping (DefId id, HIR::Item *item);
  HIR::Item *lookup_defid (DefId id);
  void insert_defid_mapping (DefId id, HIR::TraitItem *item);
  HIR::TraitItem *lookup_trait_item_defid (DefId id);

  void insert_local_defid_mapping (CrateNum crateNum, LocalDefId id,
				   HIR::Item *item);
  HIR::Item *lookup_local_defid (CrateNum crateNum, LocalDefId id);

  void insert_hir_item (HIR::Item *item);
  HIR::Item *lookup_hir_item (HirId id);

  void insert_hir_trait_item (HIR::TraitItem *item);
  HIR::TraitItem *lookup_hir_trait_item (HirId id);

  void insert_hir_extern_block (HIR::ExternBlock *block);
  HIR::ExternBlock *lookup_hir_extern_block (HirId id);

  void insert_hir_extern_item (HIR::ExternalItem *item, HirId parent_block);
  HIR::ExternalItem *lookup_hir_extern_item (HirId id, HirId *parent_block);

  void insert_hir_impl_block (HIR::ImplBlock *item);
  HIR::ImplBlock *lookup_hir_impl_block (HirId id);
  bool lookup_impl_block_type (HirId id, HIR::ImplBlock **impl_block);

  void insert_module (HIR::Module *module);
  HIR::Module *lookup_module (HirId id);

  void insert_hir_implitem (HirId parent_impl_id, HIR::ImplItem *item);
  HIR::ImplItem *lookup_hir_implitem (HirId id, HirId *parent_impl_id);

  void insert_hir_expr (HIR::Expr *expr);
  HIR::Expr *lookup_hir_expr (HirId id);

  void insert_hir_path_expr_seg (HIR::PathExprSegment *expr);
  HIR::PathExprSegment *lookup_hir_path_expr_seg (HirId id);

  void insert_hir_generic_param (HIR::GenericParam *expr);
  HIR::GenericParam *lookup_hir_generic_param (HirId id);

  void insert_hir_type (HIR::Type *type);
  HIR::Type *lookup_hir_type (HirId id);

  void insert_hir_stmt (HIR::Stmt *stmt);
  HIR::Stmt *lookup_hir_stmt (HirId id);

  void insert_hir_param (HIR::FunctionParam *type);
  HIR::FunctionParam *lookup_hir_param (HirId id);

  void insert_hir_self_param (HIR::SelfParam *type);
  HIR::SelfParam *lookup_hir_self_param (HirId id);

  void insert_hir_struct_field (HIR::StructExprField *type);
  HIR::StructExprField *lookup_hir_struct_field (HirId id);

  void insert_hir_pattern (HIR::Pattern *pattern);
  HIR::Pattern *lookup_hir_pattern (HirId id);

  void walk_local_defids_for_crate (CrateNum crateNum,
				    std::function<bool (HIR::Item *)> cb);

  void insert_node_to_hir (NodeId id, HirId ref);
  bool lookup_node_to_hir (NodeId id, HirId *ref);
  bool lookup_hir_to_node (HirId id, NodeId *ref);

  void insert_location (HirId id, Location locus);
  Location lookup_location (HirId id);

  bool resolve_nodeid_to_stmt (NodeId id, HIR::Stmt **stmt);

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

  void iterate_trait_items (
    std::function<bool (HIR::TraitItem *item, HIR::Trait *)> cb);

  bool is_impl_item (HirId id)
  {
    HirId parent_impl_block_id = UNKNOWN_HIRID;
    return lookup_hir_implitem (id, &parent_impl_block_id) != nullptr;
  }

  void insert_trait_item_mapping (HirId trait_item_id, HIR::Trait *trait)
  {
    rust_assert (hirTraitItemsToTraitMappings.find (trait_item_id)
		 == hirTraitItemsToTraitMappings.end ());
    hirTraitItemsToTraitMappings[trait_item_id] = trait;
  }

  HIR::Trait *lookup_trait_item_mapping (HirId trait_item_id)
  {
    auto lookup = hirTraitItemsToTraitMappings.find (trait_item_id);
    rust_assert (lookup != hirTraitItemsToTraitMappings.end ());
    return lookup->second;
  }

  void insert_canonical_path (NodeId id, const Resolver::CanonicalPath path)
  {
    const Resolver::CanonicalPath *p = nullptr;
    if (lookup_canonical_path (id, &p))
      {
	// if we have already stored a canonical path this is ok so long as
	// this new path is equal or is smaller that the existing one but in
	// that case we ignore it.
	if (p->is_equal (path))
	  return;
	else
	  {
	    rust_assert (p->size () >= path.size ());
	    return;
	  }
      }

    paths.emplace (id, std::move (path));
  }

  bool lookup_canonical_path (NodeId id, const Resolver::CanonicalPath **path)
  {
    auto it = paths.find (id);
    if (it == paths.end ())
      return false;

    *path = &it->second;
    return true;
  }

  void insert_lang_item (RustLangItem::ItemType item_type, DefId id)
  {
    auto it = lang_item_mappings.find (item_type);
    rust_assert (it == lang_item_mappings.end ());

    lang_item_mappings[item_type] = id;
  }

  bool lookup_lang_item (RustLangItem::ItemType item_type, DefId *id)
  {
    auto it = lang_item_mappings.find (item_type);
    if (it == lang_item_mappings.end ())
      return false;

    *id = it->second;
    return true;
  }

  void insert_macro_def (AST::MacroRulesDefinition *macro);

  bool lookup_macro_def (NodeId id, AST::MacroRulesDefinition **def);

  void insert_macro_invocation (AST::MacroInvocation &invoc,
				AST::MacroRulesDefinition *def);
  bool lookup_macro_invocation (AST::MacroInvocation &invoc,
				AST::MacroRulesDefinition **def);

  void insert_visibility (NodeId id, Privacy::ModuleVisibility visibility);
  bool lookup_visibility (NodeId id, Privacy::ModuleVisibility &def);

  void insert_module_child (NodeId module, NodeId child);
  Optional<std::vector<NodeId> &> lookup_module_children (NodeId module);

  void insert_module_child_item (NodeId module, Resolver::CanonicalPath item);
  Optional<std::vector<Resolver::CanonicalPath> &>
  lookup_module_chidren_items (NodeId module);
  Optional<Resolver::CanonicalPath &>
  lookup_module_child (NodeId module, const std::string &item_name);

  void insert_child_item_to_parent_module_mapping (NodeId child_item,
						   NodeId parent_module);
  Optional<NodeId> lookup_parent_module (NodeId child_item);
  bool node_is_module (NodeId query);

  void insert_ast_item (AST::Item *item);
  bool lookup_ast_item (NodeId id, AST::Item **result);

  HIR::ImplBlock *lookup_builtin_marker ();

private:
  Mappings ();

  CrateNum crateNumItr;
  CrateNum currentCrateNum;
  HirId hirIdIter;
  NodeId nodeIdIter;
  std::map<CrateNum, LocalDefId> localIdIter;
  HIR::ImplBlock *builtinMarker;

  std::map<NodeId, CrateNum> crate_node_to_crate_num;
  std::map<CrateNum, AST::Crate *> ast_crate_mappings;
  std::map<CrateNum, HIR::Crate *> hir_crate_mappings;
  std::map<DefId, HIR::Item *> defIdMappings;
  std::map<DefId, HIR::TraitItem *> defIdTraitItemMappings;
  std::map<CrateNum, std::map<LocalDefId, HIR::Item *>> localDefIdMappings;

  std::map<HirId, HIR::Module *> hirModuleMappings;
  std::map<HirId, HIR::Item *> hirItemMappings;
  std::map<HirId, HIR::Type *> hirTypeMappings;
  std::map<HirId, HIR::Expr *> hirExprMappings;
  std::map<HirId, HIR::Stmt *> hirStmtMappings;
  std::map<HirId, HIR::FunctionParam *> hirParamMappings;
  std::map<HirId, HIR::StructExprField *> hirStructFieldMappings;
  std::map<HirId, std::pair<HirId, HIR::ImplItem *>> hirImplItemMappings;
  std::map<HirId, HIR::SelfParam *> hirSelfParamMappings;
  std::map<HirId, HIR::ImplBlock *> hirImplItemsToImplMappings;
  std::map<HirId, HIR::ImplBlock *> hirImplBlockMappings;
  std::map<HirId, HIR::ImplBlock *> hirImplBlockTypeMappings;
  std::map<HirId, HIR::TraitItem *> hirTraitItemMappings;
  std::map<HirId, HIR::ExternBlock *> hirExternBlockMappings;
  std::map<HirId, std::pair<HIR::ExternalItem *, HirId>> hirExternItemMappings;
  std::map<HirId, HIR::PathExprSegment *> hirPathSegMappings;
  std::map<HirId, HIR::GenericParam *> hirGenericParamMappings;
  std::map<HirId, HIR::Trait *> hirTraitItemsToTraitMappings;
  std::map<HirId, HIR::Pattern *> hirPatternMappings;
  std::map<RustLangItem::ItemType, DefId> lang_item_mappings;
  std::map<NodeId, const Resolver::CanonicalPath> paths;
  std::map<NodeId, Location> locations;
  std::map<NodeId, HirId> nodeIdToHirMappings;
  std::map<HirId, NodeId> hirIdToNodeMappings;

  // all hirid nodes
  std::map<CrateNum, std::set<HirId>> hirNodesWithinCrate;

  // macros
  std::map<NodeId, AST::MacroRulesDefinition *> macroMappings;
  std::map<NodeId, AST::MacroRulesDefinition *> macroInvocations;

  // crate names
  std::map<CrateNum, std::string> crate_names;

  // Low level visibility map for each DefId
  std::map<NodeId, Privacy::ModuleVisibility> visibility_map;

  // Module tree maps

  // Maps each module's node id to a list of its children
  std::map<NodeId, std::vector<NodeId>> module_child_map;
  std::map<NodeId, std::vector<Resolver::CanonicalPath>> module_child_items;
  std::map<NodeId, NodeId> child_to_parent_module_map;

  // AST mappings
  std::map<NodeId, AST::Item *> ast_item_mappings;
};

} // namespace Analysis
} // namespace Rust

#endif // RUST_HIR_MAP_H
