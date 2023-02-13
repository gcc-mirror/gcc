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

#include "rust-hir-map.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-hir-full.h"
#include "rust-macro-builtins.h"
#include "rust-mapping-common.h"

namespace Rust {
namespace Analysis {

NodeMapping
NodeMapping::get_error ()
{
  return NodeMapping (UNKNOWN_CREATENUM, UNKNOWN_NODEID, UNKNOWN_HIRID,
		      UNKNOWN_LOCAL_DEFID);
}

CrateNum
NodeMapping::get_crate_num () const
{
  return crateNum;
}

NodeId
NodeMapping::get_nodeid () const
{
  return nodeId;
}

HirId
NodeMapping::get_hirid () const
{
  return hirId;
}

LocalDefId
NodeMapping::get_local_defid () const
{
  return localDefId;
}

DefId
NodeMapping::get_defid () const
{
  return get_defid (get_crate_num (), get_local_defid ());
}

DefId
NodeMapping::get_defid (CrateNum crate_num, LocalDefId local_defid)
{
  return DefId{crate_num, local_defid};
}

std::string
NodeMapping::as_string () const
{
  std::ostringstream ss;
  ss << "["
     << "C: " << get_crate_num ();
  if (get_nodeid () != UNKNOWN_NODEID)
    ss << " Nid: " << get_nodeid ();

  if (get_hirid () != UNKNOWN_HIRID)
    ss << " Hid: " << get_hirid ();

  if (get_local_defid () != UNKNOWN_LOCAL_DEFID)
    ss << " Lid: " << get_local_defid ();

  ss << "]";
  return ss.str ();
}

// Mappings Class now
static const HirId kDefaultNodeIdBegin = 1;
static const HirId kDefaultHirIdBegin = 1;
static const HirId kDefaultCrateNumBegin = 0;

Mappings::Mappings ()
  : crateNumItr (kDefaultCrateNumBegin), currentCrateNum (UNKNOWN_CREATENUM),
    hirIdIter (kDefaultHirIdBegin), nodeIdIter (kDefaultNodeIdBegin)
{
  Analysis::NodeMapping node (0, 0, 0, 0);
  builtinMarker
    = new HIR::ImplBlock (node, {}, {}, nullptr, nullptr, HIR::WhereClause ({}),
			  Positive,
			  HIR::Visibility (HIR::Visibility::VisType::PUBLIC),
			  {}, {}, Location ());
}

Mappings::~Mappings () { delete builtinMarker; }

Mappings *
Mappings::get ()
{
  static std::unique_ptr<Mappings> instance;
  if (!instance)
    instance = std::unique_ptr<Mappings> (new Mappings ());

  return instance.get ();
}

CrateNum
Mappings::get_next_crate_num (const std::string &name)
{
  auto id = crateNumItr;
  crateNumItr++;
  set_crate_name (id, name);
  return id;
}

void
Mappings::set_current_crate (CrateNum crateNum)
{
  currentCrateNum = crateNum;
}

CrateNum
Mappings::get_current_crate () const
{
  return currentCrateNum;
}

bool
Mappings::get_crate_name (CrateNum crate_num, std::string &name) const
{
  auto it = crate_names.find (crate_num);
  if (it == crate_names.end ())
    return false;

  name.assign (it->second);
  return true;
}

void
Mappings::set_crate_name (CrateNum crate_num, const std::string &name)
{
  crate_names[crate_num] = name;
}

std::string
Mappings::get_current_crate_name () const
{
  std::string name;
  bool ok = get_crate_name (get_current_crate (), name);
  rust_assert (ok);
  return name;
}

bool
Mappings::lookup_crate_name (const std::string &crate_name,
			     CrateNum &resolved_crate_num) const
{
  for (const auto &it : crate_names)
    {
      if (it.second.compare (crate_name) == 0)
	{
	  resolved_crate_num = it.first;
	  return true;
	}
    }
  return false;
}

bool
Mappings::crate_num_to_nodeid (const CrateNum &crate_num, NodeId &node_id) const
{
  auto it = ast_crate_mappings.find (crate_num);
  if (it == ast_crate_mappings.end ())
    return false;

  node_id = it->second->get_node_id ();
  return true;
}

bool
Mappings::node_is_crate (NodeId node_id) const
{
  for (const auto &it : ast_crate_mappings)
    {
      NodeId crate_node_id = it.second->get_node_id ();
      if (crate_node_id == node_id)
	return true;
    }
  return false;
}

NodeId
Mappings::get_next_node_id ()
{
  auto it = nodeIdIter;
  nodeIdIter++;
  return it;
}

HirId
Mappings::get_next_hir_id (CrateNum crateNum)
{
  auto id = hirIdIter;
  hirIdIter++;

  auto it = hirNodesWithinCrate.find (crateNum);
  if (it == hirNodesWithinCrate.end ())
    {
      hirNodesWithinCrate.insert ({crateNum, {}});
    }

  hirNodesWithinCrate[crateNum].insert (id);
  return id;
}

LocalDefId
Mappings::get_next_localdef_id (CrateNum crateNum)
{
  auto it = localIdIter.find (crateNum);
  if (it == localIdIter.end ())
    {
      localIdIter.insert ({crateNum, 1});
    }

  it = localIdIter.find (crateNum);
  rust_assert (it != localIdIter.end ());

  LocalDefId id = it->second;
  localIdIter[crateNum] = id + 1;
  return id;
}

AST::Crate &
Mappings::get_ast_crate (CrateNum crateNum)
{
  auto it = ast_crate_mappings.find (crateNum);
  rust_assert (it != ast_crate_mappings.end ());
  return *it->second;
}

AST::Crate &
Mappings::get_ast_crate_by_node_id (NodeId id)
{
  auto i = crate_node_to_crate_num.find (id);
  rust_assert (i != crate_node_to_crate_num.end ());

  CrateNum crateNum = i->second;
  auto it = ast_crate_mappings.find (crateNum);
  rust_assert (it != ast_crate_mappings.end ());
  return *it->second;
}

AST::Crate &
Mappings::insert_ast_crate (std::unique_ptr<AST::Crate> &&crate,
			    CrateNum crate_num)
{
  auto it = ast_crate_mappings.find (crate_num);
  rust_assert (it == ast_crate_mappings.end ());

  // store it
  ast_crate_mappings.insert ({crate_num, crate.release ()});

  // return the reference to it
  it = ast_crate_mappings.find (crate_num);
  rust_assert (it != ast_crate_mappings.end ());
  return *it->second;
}

HIR::Crate &
Mappings::get_hir_crate (CrateNum crateNum)
{
  auto it = hir_crate_mappings.find (crateNum);
  rust_assert (it != hir_crate_mappings.end ());
  return *it->second;
}

bool
Mappings::is_local_hirid_crate (HirId crateNum)
{
  for (const auto &it : hir_crate_mappings)
    {
      const auto &crate = it.second;
      if (crate->get_mappings ().get_hirid () == crateNum)
	return true;
    }
  return false;
}

HIR::Crate &
Mappings::insert_hir_crate (std::unique_ptr<HIR::Crate> &&crate)
{
  CrateNum crateNum = crate->get_mappings ().get_crate_num ();
  auto it = hir_crate_mappings.find (crateNum);
  rust_assert (it == hir_crate_mappings.end ());

  insert_node_to_hir (crate->get_mappings ().get_nodeid (),
		      crate->get_mappings ().get_hirid ());
  hir_crate_mappings.insert ({crateNum, crate.release ()});

  it = hir_crate_mappings.find (crateNum);
  rust_assert (it != hir_crate_mappings.end ());
  return *it->second;
}

void
Mappings::insert_defid_mapping (DefId id, HIR::Item *item)
{
  CrateNum crate_num = id.crateNum;
  LocalDefId local_def_id = id.localDefId;

  rust_assert (lookup_defid (id) == nullptr);
  rust_assert (lookup_local_defid (crate_num, local_def_id) == nullptr);
  rust_assert (lookup_trait_item_defid (id) == nullptr);

  defIdMappings[id] = item;
  insert_local_defid_mapping (crate_num, local_def_id, item);
}

HIR::Item *
Mappings::lookup_defid (DefId id)
{
  auto it = defIdMappings.find (id);
  if (it == defIdMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_defid_mapping (DefId id, HIR::TraitItem *item)
{
  CrateNum crate_num = id.crateNum;
  LocalDefId local_def_id = id.localDefId;

  rust_assert (lookup_defid (id) == nullptr);
  rust_assert (lookup_local_defid (crate_num, local_def_id) == nullptr);
  rust_assert (lookup_trait_item_defid (id) == nullptr);

  defIdTraitItemMappings[id] = item;
}

HIR::TraitItem *
Mappings::lookup_trait_item_defid (DefId id)
{
  auto it = defIdTraitItemMappings.find (id);
  if (it == defIdTraitItemMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_item (HIR::Item *item)
{
  auto id = item->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_item (id) == nullptr);

  hirItemMappings[id] = item;
  insert_node_to_hir (item->get_mappings ().get_nodeid (), id);
}

HIR::Item *
Mappings::lookup_hir_item (HirId id)
{
  auto it = hirItemMappings.find (id);
  if (it == hirItemMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_trait_item (HIR::TraitItem *item)
{
  auto id = item->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_trait_item (id) == nullptr);

  hirTraitItemMappings[id] = item;
  insert_node_to_hir (item->get_mappings ().get_nodeid (), id);
}

HIR::TraitItem *
Mappings::lookup_hir_trait_item (HirId id)
{
  auto it = hirTraitItemMappings.find (id);
  if (it == hirTraitItemMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_extern_block (HIR::ExternBlock *block)
{
  auto id = block->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_extern_block (id) == nullptr);

  hirExternBlockMappings[id] = block;
  insert_node_to_hir (block->get_mappings ().get_nodeid (), id);
}

HIR::ExternBlock *
Mappings::lookup_hir_extern_block (HirId id)
{
  auto it = hirExternBlockMappings.find (id);
  if (it == hirExternBlockMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_extern_item (HIR::ExternalItem *item, HirId parent_block)
{
  auto id = item->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_extern_item (id, nullptr) == nullptr);

  hirExternItemMappings[id] = {item, parent_block};
  insert_node_to_hir (item->get_mappings ().get_nodeid (), id);
}

HIR::ExternalItem *
Mappings::lookup_hir_extern_item (HirId id, HirId *parent_block)
{
  auto it = hirExternItemMappings.find (id);
  if (it == hirExternItemMappings.end ())
    return nullptr;

  *parent_block = it->second.second;

  return it->second.first;
}

void
Mappings::insert_hir_impl_block (HIR::ImplBlock *item)
{
  auto id = item->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_impl_block (id) == nullptr);

  HirId impl_type_id = item->get_type ()->get_mappings ().get_hirid ();
  hirImplBlockMappings[id] = item;
  hirImplBlockTypeMappings[impl_type_id] = item;
  insert_node_to_hir (item->get_mappings ().get_nodeid (), id);
}

HIR::ImplBlock *
Mappings::lookup_hir_impl_block (HirId id)
{
  auto it = hirImplBlockMappings.find (id);
  if (it == hirImplBlockMappings.end ())
    return nullptr;

  return it->second;
}

bool
Mappings::lookup_impl_block_type (HirId id, HIR::ImplBlock **impl_block)
{
  auto it = hirImplBlockTypeMappings.find (id);
  if (it == hirImplBlockTypeMappings.end ())
    return false;

  *impl_block = it->second;
  return true;
}

void
Mappings::insert_module (HIR::Module *module)
{
  auto id = module->get_mappings ().get_hirid ();
  rust_assert (lookup_module (id) == nullptr);

  hirModuleMappings[id] = module;
  insert_node_to_hir (module->get_mappings ().get_nodeid (), id);
}

HIR::Module *
Mappings::lookup_module (HirId id)
{
  auto it = hirModuleMappings.find (id);
  if (it == hirModuleMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_implitem (HirId parent_impl_id, HIR::ImplItem *item)
{
  auto id = item->get_impl_mappings ().get_hirid ();
  rust_assert (lookup_hir_implitem (id, nullptr) == nullptr);

  hirImplItemMappings[id]
    = std::pair<HirId, HIR::ImplItem *> (parent_impl_id, item);
  insert_node_to_hir (item->get_impl_mappings ().get_nodeid (), id);
}

HIR::ImplItem *
Mappings::lookup_hir_implitem (HirId id, HirId *parent_impl_id)
{
  auto it = hirImplItemMappings.find (id);
  if (it == hirImplItemMappings.end ())
    return nullptr;

  std::pair<HirId, HIR::ImplItem *> &ref = it->second;
  if (parent_impl_id != nullptr)
    *parent_impl_id = ref.first;

  return ref.second;
}

void
Mappings::insert_hir_expr (HIR::Expr *expr)
{
  auto id = expr->get_mappings ().get_hirid ();
  hirExprMappings[id] = expr;

  insert_node_to_hir (expr->get_mappings ().get_nodeid (), id);
  insert_location (id, expr->get_locus ());
}

HIR::Expr *
Mappings::lookup_hir_expr (HirId id)
{
  auto it = hirExprMappings.find (id);
  if (it == hirExprMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_path_expr_seg (HIR::PathExprSegment *expr)
{
  auto id = expr->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_path_expr_seg (id) == nullptr);

  hirPathSegMappings[id] = expr;
  insert_node_to_hir (expr->get_mappings ().get_nodeid (), id);
  insert_location (id, expr->get_locus ());
}

HIR::PathExprSegment *
Mappings::lookup_hir_path_expr_seg (HirId id)
{
  auto it = hirPathSegMappings.find (id);
  if (it == hirPathSegMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_generic_param (HIR::GenericParam *param)
{
  auto id = param->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_generic_param (id) == nullptr);

  hirGenericParamMappings[id] = param;
  insert_node_to_hir (param->get_mappings ().get_nodeid (), id);
  insert_location (id, param->get_locus ());
}

HIR::GenericParam *
Mappings::lookup_hir_generic_param (HirId id)
{
  auto it = hirGenericParamMappings.find (id);
  if (it == hirGenericParamMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_type (HIR::Type *type)
{
  auto id = type->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_type (id) == nullptr);

  hirTypeMappings[id] = type;
  insert_node_to_hir (type->get_mappings ().get_nodeid (), id);
}

HIR::Type *
Mappings::lookup_hir_type (HirId id)
{
  auto it = hirTypeMappings.find (id);
  if (it == hirTypeMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_stmt (HIR::Stmt *stmt)
{
  auto id = stmt->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_stmt (id) == nullptr);

  hirStmtMappings[id] = stmt;
  insert_node_to_hir (stmt->get_mappings ().get_nodeid (), id);
}

HIR::Stmt *
Mappings::lookup_hir_stmt (HirId id)
{
  auto it = hirStmtMappings.find (id);
  if (it == hirStmtMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_param (HIR::FunctionParam *param)
{
  auto id = param->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_param (id) == nullptr);

  hirParamMappings[id] = param;
  insert_node_to_hir (param->get_mappings ().get_nodeid (), id);
}

HIR::FunctionParam *
Mappings::lookup_hir_param (HirId id)
{
  auto it = hirParamMappings.find (id);
  if (it == hirParamMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_self_param (HIR::SelfParam *param)
{
  auto id = param->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_self_param (id) == nullptr);

  hirSelfParamMappings[id] = param;
  insert_node_to_hir (param->get_mappings ().get_nodeid (), id);
}

HIR::SelfParam *
Mappings::lookup_hir_self_param (HirId id)
{
  auto it = hirSelfParamMappings.find (id);
  if (it == hirSelfParamMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_struct_field (HIR::StructExprField *field)
{
  auto id = field->get_mappings ().get_hirid ();
  rust_assert (lookup_hir_struct_field (id) == nullptr);

  hirStructFieldMappings[id] = field;
  insert_node_to_hir (field->get_mappings ().get_nodeid (), id);
}

HIR::StructExprField *
Mappings::lookup_hir_struct_field (HirId id)
{
  auto it = hirStructFieldMappings.find (id);
  if (it == hirStructFieldMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_pattern (HIR::Pattern *pattern)
{
  auto id = pattern->get_pattern_mappings ().get_hirid ();
  rust_assert (lookup_hir_pattern (id) == nullptr);

  hirPatternMappings[id] = pattern;
  insert_node_to_hir (pattern->get_pattern_mappings ().get_nodeid (), id);
}

HIR::Pattern *
Mappings::lookup_hir_pattern (HirId id)
{
  auto it = hirPatternMappings.find (id);
  if (it == hirPatternMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_local_defid_mapping (CrateNum crateNum, LocalDefId id,
				      HIR::Item *item)
{
  rust_assert (lookup_local_defid (crateNum, id) == nullptr);
  localDefIdMappings[crateNum][id] = item;
}

HIR::Item *
Mappings::lookup_local_defid (CrateNum crateNum, LocalDefId id)
{
  auto it = localDefIdMappings.find (crateNum);
  if (it == localDefIdMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::walk_local_defids_for_crate (CrateNum crateNum,
				       std::function<bool (HIR::Item *)> cb)
{
  auto it = localDefIdMappings.find (crateNum);
  if (it == localDefIdMappings.end ())
    return;

  for (auto iy = it->second.begin (); iy != it->second.end (); iy++)
    {
      if (!cb (iy->second))
	return;
    }
}

void
Mappings::insert_node_to_hir (NodeId id, HirId ref)
{
  nodeIdToHirMappings[id] = ref;
  hirIdToNodeMappings[ref] = id;
}

bool
Mappings::lookup_node_to_hir (NodeId id, HirId *ref)
{
  auto it = nodeIdToHirMappings.find (id);
  if (it == nodeIdToHirMappings.end ())
    return false;

  *ref = it->second;
  return true;
}

bool
Mappings::lookup_hir_to_node (HirId id, NodeId *ref)
{
  auto it = hirIdToNodeMappings.find (id);
  if (it == hirIdToNodeMappings.end ())
    return false;

  *ref = it->second;
  return true;
}

void
Mappings::insert_location (HirId id, Location locus)
{
  locations[id] = locus;
}

Location
Mappings::lookup_location (HirId id)
{
  auto it = locations.find (id);
  if (it == locations.end ())
    return Location ();

  return it->second;
}

bool
Mappings::resolve_nodeid_to_stmt (NodeId id, HIR::Stmt **stmt)
{
  auto it = nodeIdToHirMappings.find (id);
  if (it == nodeIdToHirMappings.end ())
    return false;

  HirId resolved = it->second;
  auto resolved_stmt = lookup_hir_stmt (resolved);
  *stmt = resolved_stmt;
  return resolved_stmt != nullptr;
}

void
Mappings::iterate_impl_items (
  std::function<bool (HirId, HIR::ImplItem *, HIR::ImplBlock *)> cb)
{
  for (auto it = hirImplItemMappings.begin (); it != hirImplItemMappings.end ();
       it++)
    {
      auto id = it->first;
      auto impl_item = it->second.second;
      auto impl
	= lookup_associated_impl (impl_item->get_impl_mappings ().get_hirid ());
      if (!cb (id, impl_item, impl))
	return;
    }
}

void
Mappings::iterate_impl_blocks (std::function<bool (HirId, HIR::ImplBlock *)> cb)
{
  for (auto it = hirImplBlockMappings.begin ();
       it != hirImplBlockMappings.end (); it++)
    {
      HirId id = it->first;
      HIR::ImplBlock *impl_block = it->second;
      if (!cb (id, impl_block))
	return;
    }
}

void
Mappings::iterate_trait_items (
  std::function<bool (HIR::TraitItem *, HIR::Trait *)> cb)
{
  for (auto it = hirTraitItemMappings.begin ();
       it != hirTraitItemMappings.end (); it++)
    {
      HirId trait_item_id = it->first;
      HIR::TraitItem *trait_item = it->second;
      HIR::Trait *trait = lookup_trait_item_mapping (trait_item_id);

      if (!cb (trait_item, trait))
	return;
    }
}

void
Mappings::insert_macro_def (AST::MacroRulesDefinition *macro)
{
  static std::map<
    std::string, std::function<AST::Fragment (Location, AST::MacroInvocData &)>>
    builtin_macros = {
      {"assert", MacroBuiltin::assert_handler},
      {"file", MacroBuiltin::file_handler},
      {"line", MacroBuiltin::line_handler},
      {"column", MacroBuiltin::column_handler},
      {"include_bytes", MacroBuiltin::include_bytes_handler},
      {"include_str", MacroBuiltin::include_str_handler},
      {"compile_error", MacroBuiltin::compile_error_handler},
      {"concat", MacroBuiltin::concat_handler},
      {"env", MacroBuiltin::env_handler},
      {"cfg", MacroBuiltin::cfg_handler},
      {"include", MacroBuiltin::include_handler},
    };

  auto outer_attrs = macro->get_outer_attrs ();
  bool should_be_builtin
    = std::any_of (outer_attrs.begin (), outer_attrs.end (),
		   [] (AST::Attribute attr) {
		     return attr.get_path () == "rustc_builtin_macro";
		   });
  if (should_be_builtin)
    {
      auto builtin = builtin_macros.find (macro->get_rule_name ());
      if (builtin != builtin_macros.end ())
	macro->set_builtin_transcriber (builtin->second);
      else
	rust_error_at (macro->get_locus (),
		       "cannot find a built-in macro with name %qs",
		       macro->get_rule_name ().c_str ());
    }

  auto it = macroMappings.find (macro->get_node_id ());
  rust_assert (it == macroMappings.end ());

  macroMappings[macro->get_node_id ()] = macro;
}

bool
Mappings::lookup_macro_def (NodeId id, AST::MacroRulesDefinition **def)
{
  auto it = macroMappings.find (id);
  if (it == macroMappings.end ())
    return false;

  *def = it->second;
  return true;
}

void
Mappings::insert_macro_invocation (AST::MacroInvocation &invoc,
				   AST::MacroRulesDefinition *def)
{
  auto it = macroInvocations.find (invoc.get_macro_node_id ());
  rust_assert (it == macroInvocations.end ());

  macroInvocations[invoc.get_macro_node_id ()] = def;
}

bool
Mappings::lookup_macro_invocation (AST::MacroInvocation &invoc,
				   AST::MacroRulesDefinition **def)
{
  auto it = macroInvocations.find (invoc.get_macro_node_id ());
  if (it == macroInvocations.end ())
    return false;

  *def = it->second;
  return true;
}

void
Mappings::insert_visibility (NodeId id, Privacy::ModuleVisibility visibility)
{
  visibility_map.insert ({id, visibility});
}

bool
Mappings::lookup_visibility (NodeId id, Privacy::ModuleVisibility &def)
{
  auto it = visibility_map.find (id);
  if (it == visibility_map.end ())
    return false;

  def = it->second;
  return true;
}

void
Mappings::insert_module_child (NodeId module, NodeId child)
{
  auto it = module_child_map.find (module);
  if (it == module_child_map.end ())
    module_child_map.insert ({module, {child}});
  else
    it->second.emplace_back (child);
}

Optional<std::vector<NodeId> &>
Mappings::lookup_module_children (NodeId module)
{
  auto it = module_child_map.find (module);
  if (it == module_child_map.end ())
    return Optional<std::vector<NodeId> &>::none ();

  return Optional<std::vector<NodeId> &>::some (it->second);
}

void
Mappings::insert_module_child_item (NodeId module,
				    Resolver::CanonicalPath child)
{
  rust_assert (!child.is_empty ());
  rust_assert (child.get_node_id () != UNKNOWN_NODEID);

  auto it = module_child_items.find (module);
  if (it == module_child_items.end ())
    module_child_items.insert ({module, {child}});
  else
    it->second.emplace_back (child);
}

Optional<std::vector<Resolver::CanonicalPath> &>
Mappings::lookup_module_chidren_items (NodeId module)
{
  auto it = module_child_items.find (module);
  if (it == module_child_items.end ())
    return Optional<std::vector<Resolver::CanonicalPath> &>::none ();

  return Optional<std::vector<Resolver::CanonicalPath> &>::some (it->second);
}

Optional<Resolver::CanonicalPath &>
Mappings::lookup_module_child (NodeId module, const std::string &item_name)
{
  Optional<std::vector<Resolver::CanonicalPath> &> children
    = lookup_module_chidren_items (module);
  if (children.is_none ())
    return Optional<Resolver::CanonicalPath &>::none ();

  // lookup the children to match the name if we can
  for (auto &child : children.get ())
    {
      const std::string &raw_identifier = child.get ();
      bool found = raw_identifier.compare (item_name) == 0;
      if (found)
	return Optional<Resolver::CanonicalPath &>::some (child);
    }
  return Optional<Resolver::CanonicalPath &>::none ();
}

void
Mappings::insert_child_item_to_parent_module_mapping (NodeId child_item,
						      NodeId parent_module)
{
  child_to_parent_module_map.insert ({child_item, parent_module});
}

Optional<NodeId>
Mappings::lookup_parent_module (NodeId child_item)
{
  auto it = child_to_parent_module_map.find (child_item);
  if (it == child_to_parent_module_map.end ())
    return Optional<NodeId>::none ();

  return Optional<NodeId>::some (it->second);
}

bool
Mappings::node_is_module (NodeId query)
{
  return module_child_items.find (query) != module_child_items.end ();
}

void
Mappings::insert_ast_item (AST::Item *item)
{
  auto it = ast_item_mappings.find (item->get_node_id ());
  rust_assert (it == ast_item_mappings.end ());

  ast_item_mappings[item->get_node_id ()] = item;
}

bool
Mappings::lookup_ast_item (NodeId id, AST::Item **result)
{
  auto it = ast_item_mappings.find (id);
  if (it == ast_item_mappings.end ())
    return false;

  *result = it->second;
  return true;
}

HIR::ImplBlock *
Mappings::lookup_builtin_marker ()
{
  return builtinMarker;
}

} // namespace Analysis
} // namespace Rust
