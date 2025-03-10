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

#ifndef RUST_HIR_MAP_H
#define RUST_HIR_MAP_H

#include "optional.h"
#include "rust-system.h"
#include "rust-location.h"
#include "rust-mapping-common.h"
#include "rust-canonical-path.h"
#include "rust-ast-full-decls.h"
#include "rust-hir-full-decls.h"
#include "rust-lang-item.h"
#include "rust-privacy-common.h"
#include "libproc_macro_internal/proc_macro.h"
#include "rust-proc-macro.h"
#include "optional.h"

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
  static Mappings &get ();
  ~Mappings ();

  CrateNum get_next_crate_num (const std::string &name);
  void set_current_crate (CrateNum crateNum);
  CrateNum get_current_crate () const;
  tl::optional<const std::string &> get_crate_name (CrateNum crate_num) const;

  tl::optional<CrateNum> lookup_crate_num (NodeId node_id) const;
  void set_crate_name (CrateNum crate_num, const std::string &name);
  const std::string &get_current_crate_name () const;
  tl::optional<CrateNum>
  lookup_crate_name (const std::string &crate_name) const;
  tl::optional<NodeId> crate_num_to_nodeid (const CrateNum &crate_num) const;
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
  tl::optional<HIR::Item *> lookup_defid (DefId id);
  void insert_defid_mapping (DefId id, HIR::TraitItem *item);
  tl::optional<HIR::TraitItem *> lookup_trait_item_defid (DefId id);

  void insert_local_defid_mapping (CrateNum crateNum, LocalDefId id,
				   HIR::Item *item);
  tl::optional<HIR::Item *> lookup_local_defid (CrateNum crateNum,
						LocalDefId id);

  void insert_hir_item (HIR::Item *item);
  tl::optional<HIR::Item *> lookup_hir_item (HirId id);

  void insert_hir_enumitem (HIR::Enum *parent, HIR::EnumItem *item);
  std::pair<HIR::Enum *, HIR::EnumItem *> lookup_hir_enumitem (HirId id);

  void insert_hir_trait_item (HIR::TraitItem *item);
  tl::optional<HIR::TraitItem *> lookup_hir_trait_item (HirId id);

  void insert_hir_extern_block (HIR::ExternBlock *block);
  tl::optional<HIR::ExternBlock *> lookup_hir_extern_block (HirId id);

  void insert_hir_extern_item (HIR::ExternalItem *item, HirId parent_block);

  // std::pair<hir_extern_item, parent hirid>
  tl::optional<std::pair<HIR::ExternalItem *, HirId>>
  lookup_hir_extern_item (HirId id);

  void insert_hir_impl_block (HIR::ImplBlock *item);
  tl::optional<HIR::ImplBlock *> lookup_hir_impl_block (HirId id);
  tl::optional<HIR::ImplBlock *> lookup_impl_block_type (HirId id);

  void insert_module (HIR::Module *module);
  tl::optional<HIR::Module *> lookup_module (HirId id);

  void insert_hir_implitem (HirId parent_impl_id, HIR::ImplItem *item);
  // Optional<ImpItem, ParentImpl Hir id>
  tl::optional<std::pair<HIR::ImplItem *, HirId>>
  lookup_hir_implitem (HirId id);

  void insert_hir_expr (HIR::Expr *expr);
  tl::optional<HIR::Expr *> lookup_hir_expr (HirId id);

  void insert_hir_path_expr_seg (HIR::PathExprSegment *expr);
  tl::optional<HIR::PathExprSegment *> lookup_hir_path_expr_seg (HirId id);

  void insert_hir_generic_param (HIR::GenericParam *expr);
  tl::optional<HIR::GenericParam *> lookup_hir_generic_param (HirId id);

  void insert_hir_type (HIR::Type *type);
  tl::optional<HIR::Type *> lookup_hir_type (HirId id);

  void insert_hir_stmt (HIR::Stmt *stmt);
  tl::optional<HIR::Stmt *> lookup_hir_stmt (HirId id);

  void insert_hir_param (HIR::FunctionParam *type);
  tl::optional<HIR::FunctionParam *> lookup_hir_param (HirId id);

  void insert_hir_self_param (HIR::SelfParam *type);
  tl::optional<HIR::SelfParam *> lookup_hir_self_param (HirId id);

  void insert_hir_struct_field (HIR::StructExprField *type);
  tl::optional<HIR::StructExprField *> lookup_hir_struct_field (HirId id);

  void insert_hir_pattern (HIR::Pattern *pattern);
  tl::optional<HIR::Pattern *> lookup_hir_pattern (HirId id);

  void walk_local_defids_for_crate (CrateNum crateNum,
				    std::function<bool (HIR::Item *)> cb);

  void insert_node_to_hir (NodeId id, HirId ref);
  tl::optional<HirId> lookup_node_to_hir (NodeId id);
  tl::optional<NodeId> lookup_hir_to_node (HirId id);

  void insert_location (HirId id, location_t locus);
  location_t lookup_location (HirId id);

  tl::optional<HIR::Stmt *> resolve_nodeid_to_stmt (NodeId id);

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

  bool is_impl_item (HirId id) { return lookup_hir_implitem (id).has_value (); }

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
    if (auto p = lookup_canonical_path (id))
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

  tl::optional<const Resolver::CanonicalPath &>
  lookup_canonical_path (NodeId id)
  {
    auto it = paths.find (id);
    if (it == paths.end ())
      return tl::nullopt;

    return it->second;
  }

  void insert_lang_item (LangItem::Kind item_type, DefId id);
  tl::optional<DefId &> lookup_lang_item (LangItem::Kind item_type);

  void insert_lang_item_node (LangItem::Kind item_type, NodeId node_id);
  tl::optional<NodeId &> lookup_lang_item_node (LangItem::Kind item_type);
  NodeId get_lang_item_node (LangItem::Kind item_type);

  // This will fatal_error when this lang item does not exist
  DefId get_lang_item (LangItem::Kind item_type, location_t locus);

  void insert_macro_def (AST::MacroRulesDefinition *macro);

  tl::optional<AST::MacroRulesDefinition *> lookup_macro_def (NodeId id);
  tl::optional<CrateNum> lookup_macro_def_crate (NodeId id);

  void insert_macro_invocation (AST::MacroInvocation &invoc,
				AST::MacroRulesDefinition *def);
  tl::optional<AST::MacroRulesDefinition *>
  lookup_macro_invocation (AST::MacroInvocation &invoc);

  void insert_exported_macro (AST::MacroRulesDefinition &def);
  std::vector<NodeId> &get_exported_macros ();

  void insert_derive_proc_macros (CrateNum num,
				  std::vector<CustomDeriveProcMacro> macros);
  void insert_bang_proc_macros (CrateNum num,
				std::vector<BangProcMacro> macros);
  void insert_attribute_proc_macros (CrateNum num,
				     std::vector<AttributeProcMacro> macros);

  tl::optional<std::vector<CustomDeriveProcMacro> &>
  lookup_derive_proc_macros (CrateNum num);
  tl::optional<std::vector<BangProcMacro> &>
  lookup_bang_proc_macros (CrateNum num);
  tl::optional<std::vector<AttributeProcMacro> &>
  lookup_attribute_proc_macros (CrateNum num);

  void insert_derive_proc_macro_def (CustomDeriveProcMacro macro);
  void insert_bang_proc_macro_def (BangProcMacro macro);
  void insert_attribute_proc_macro_def (AttributeProcMacro macro);

  tl::optional<CustomDeriveProcMacro &>
  lookup_derive_proc_macro_def (NodeId id);
  tl::optional<BangProcMacro &> lookup_bang_proc_macro_def (NodeId id);
  tl::optional<AttributeProcMacro &>
  lookup_attribute_proc_macro_def (NodeId id);

  tl::optional<CustomDeriveProcMacro &>
  lookup_derive_proc_macro_invocation (AST::SimplePath &invoc);
  tl::optional<BangProcMacro &>
  lookup_bang_proc_macro_invocation (AST::MacroInvocation &invoc_id);
  tl::optional<AttributeProcMacro &>
  lookup_attribute_proc_macro_invocation (AST::SimplePath &invoc);
  void insert_derive_proc_macro_invocation (AST::SimplePath &invoc,
					    CustomDeriveProcMacro def);
  void insert_bang_proc_macro_invocation (AST::MacroInvocation &invoc,
					  BangProcMacro def);
  void insert_attribute_proc_macro_invocation (AST::SimplePath &invoc,
					       AttributeProcMacro def);

  void insert_visibility (NodeId id, Privacy::ModuleVisibility visibility);
  tl::optional<Privacy::ModuleVisibility &> lookup_visibility (NodeId id);

  void insert_ast_module (AST::Module *);
  tl::optional<AST::Module *> lookup_ast_module (NodeId id);
  void insert_module_child (NodeId module, NodeId child);
  tl::optional<std::vector<NodeId> &> lookup_module_children (NodeId module);

  void insert_module_child_item (NodeId module, Resolver::CanonicalPath item);
  tl::optional<std::vector<Resolver::CanonicalPath> &>
  lookup_module_chidren_items (NodeId module);
  tl::optional<Resolver::CanonicalPath &>
  lookup_module_child (NodeId module, const std::string &item_name);

  void insert_child_item_to_parent_module_mapping (NodeId child_item,
						   NodeId parent_module);
  tl::optional<NodeId> lookup_parent_module (NodeId child_item);
  bool node_is_module (NodeId query);

  void insert_ast_item (AST::Item *item);
  tl::optional<AST::Item *> lookup_ast_item (NodeId id);

  HIR::ImplBlock *lookup_builtin_marker ();

  tl::optional<HIR::TraitItem *>
  lookup_trait_item_lang_item (LangItem::Kind item, location_t locus);

  void insert_auto_trait (HIR::Trait *trait);
  std::vector<HIR::Trait *> &get_auto_traits ();
  void add_capture (NodeId closure, NodeId definition);
  tl::optional<std::vector<NodeId>> lookup_captures (NodeId closure);

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
  std::map<HirId, std::pair<HIR::Enum *, HIR::EnumItem *>> hirEnumItemMappings;
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

  // FIXME: Add documentation
  std::vector<HIR::Trait *> auto_traits;

  // We need to have two maps here, as lang-items need to be used for both AST
  // passes and HIR passes. Thus those two maps are created at different times.
  std::map<LangItem::Kind, DefId> lang_item_mappings;
  std::map<LangItem::Kind, NodeId> lang_item_nodes;

  std::map<NodeId, Resolver::CanonicalPath> paths;
  std::map<NodeId, location_t> locations;
  std::map<NodeId, HirId> nodeIdToHirMappings;
  std::map<HirId, NodeId> hirIdToNodeMappings;

  // all hirid nodes
  std::map<CrateNum, std::set<HirId>> hirNodesWithinCrate;

  // MBE macros
  std::map<NodeId, std::pair<AST::MacroRulesDefinition *, CrateNum>>
    macroMappings;
  std::map<NodeId, AST::MacroRulesDefinition *> macroInvocations;
  std::vector<NodeId> exportedMacros;

  // Procedural macros
  std::map<CrateNum, std::vector<CustomDeriveProcMacro>>
    procmacrosDeriveMappings;
  std::map<CrateNum, std::vector<BangProcMacro>> procmacrosBangMappings;
  std::map<CrateNum, std::vector<AttributeProcMacro>>
    procmacrosAttributeMappings;

  std::map<NodeId, CustomDeriveProcMacro> procmacroDeriveMappings;
  std::map<NodeId, BangProcMacro> procmacroBangMappings;
  std::map<NodeId, AttributeProcMacro> procmacroAttributeMappings;
  std::map<NodeId, CustomDeriveProcMacro> procmacroDeriveInvocations;
  std::map<NodeId, BangProcMacro> procmacroBangInvocations;
  std::map<NodeId, AttributeProcMacro> procmacroAttributeInvocations;

  // crate names
  std::map<CrateNum, std::string> crate_names;

  // Low level visibility map for each DefId
  std::map<NodeId, Privacy::ModuleVisibility> visibility_map;

  // Module tree maps

  // Maps each module's node id to a list of its children
  std::map<NodeId, std::vector<NodeId>> module_child_map;
  std::map<NodeId, std::vector<Resolver::CanonicalPath>> module_child_items;
  std::map<NodeId, NodeId> child_to_parent_module_map;
  std::map<NodeId, AST::Module *> modules;

  // AST mappings
  std::map<NodeId, AST::Item *> ast_item_mappings;

  // Closure AST NodeId -> vector of Definition node ids
  std::unordered_map<NodeId, std::vector<NodeId>> captures;
};

} // namespace Analysis
} // namespace Rust

#endif // RUST_HIR_MAP_H
