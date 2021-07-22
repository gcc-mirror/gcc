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

#include "rust-hir-map.h"
#include "rust-ast-full.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Analysis {

NodeMapping::NodeMapping (CrateNum crateNum, NodeId nodeId, HirId hirId,
			  LocalDefId localDefId)
  : crateNum (crateNum), nodeId (nodeId), hirId (hirId), localDefId (localDefId)
{}

NodeMapping::~NodeMapping () {}

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
  return ((uint64_t) crate_num << 32) | local_defid;
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

Mappings::Mappings () {}

Mappings::~Mappings () {}

Mappings *
Mappings::get ()
{
  static std::unique_ptr<Mappings> instance;
  if (!instance)
    instance = std::unique_ptr<Mappings> (new Mappings ());

  return instance.get ();
}

CrateNum
Mappings::get_next_crate_num ()
{
  return crateNumItr++;
}

void
Mappings::set_current_crate (CrateNum crateNum)
{
  currentCrateNum = crateNum;
}

CrateNum
Mappings::setup_crate_mappings (std::string crate_name)
{
  CrateNum crate_num = get_next_crate_num ();

  hirIdIter[crate_num] = UNKNOWN_HIRID;
  nodeIdIter[crate_num] = UNKNOWN_NODEID;
  localIdIter[crate_num] = UNKNOWN_LOCAL_DEFID;
  nodeIdToHirMappings[crate_num] = {};
  locations[crate_num] = {};
  crate_names[crate_num] = crate_name;

  return crate_num;
}

CrateNum
Mappings::get_current_crate () const
{
  return currentCrateNum;
}

NodeId
Mappings::get_next_node_id (CrateNum crateNum)
{
  auto it = nodeIdIter.find (crateNum);
  rust_assert (it != nodeIdIter.end ());

  auto id = it->second + 1;
  nodeIdIter[crateNum] = id;
  return id;
}

HirId
Mappings::get_next_hir_id (CrateNum crateNum)
{
  auto it = hirIdIter.find (crateNum);
  rust_assert (it != hirIdIter.end ());

  auto id = it->second + 1;
  hirIdIter[crateNum] = id;
  hirNodesWithinCrate[crateNum].insert (id);
  return id;
}

LocalDefId
Mappings::get_next_localdef_id (CrateNum crateNum)
{
  auto it = localIdIter.find (crateNum);
  rust_assert (it != localIdIter.end ());

  auto id = it->second + 1;
  localIdIter[crateNum] = id;
  return id;
}

AST::Crate *
Mappings::get_ast_crate (CrateNum crateNum)
{
  auto it = astCrateMappings.find (crateNum);
  if (it == astCrateMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_ast_crate (AST::Crate *crate)
{
  CrateNum crateNum = get_current_crate ();
  rust_assert (get_ast_crate (crateNum) == nullptr);

  astCrateMappings[crateNum] = crate;
}

HIR::Crate *
Mappings::get_hir_crate (CrateNum crateNum)
{
  auto it = hirCrateMappings.find (crateNum);
  if (it == hirCrateMappings.end ())
    return nullptr;

  return it->second;
}

void
Mappings::insert_hir_crate (HIR::Crate *crate)
{
  CrateNum crateNum = crate->get_mappings ().get_crate_num ();
  rust_assert (get_hir_crate (crateNum) == nullptr);

  hirCrateMappings[crateNum] = crate;
}

void
Mappings::insert_defid_mapping (DefId id, HIR::Item *item)
{
  CrateNum crate_num = (id & DEF_ID_CRATE_MASK) >> 32;
  LocalDefId local_def_id = id & DEF_ID_LOCAL_DEF_MASK;

  rust_assert (lookup_defid (id) == nullptr);
  rust_assert (lookup_local_defid (crate_num, local_def_id) == nullptr);

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
Mappings::insert_hir_item (CrateNum crateNum, HirId id, HIR::Item *item)
{
  rust_assert (lookup_hir_item (crateNum, id) == nullptr);

  hirItemMappings[crateNum][id] = item;
  nodeIdToHirMappings[crateNum][item->get_mappings ().get_nodeid ()] = id;
}

HIR::Item *
Mappings::lookup_hir_item (CrateNum crateNum, HirId id)
{
  auto it = hirItemMappings.find (crateNum);
  if (it == hirItemMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_trait_item (CrateNum crateNum, HirId id,
				 HIR::TraitItem *item)
{
  rust_assert (lookup_hir_item (crateNum, id) == nullptr);

  hirTraitItemMappings[crateNum][id] = item;
  nodeIdToHirMappings[crateNum][item->get_mappings ().get_nodeid ()] = id;
}

HIR::TraitItem *
Mappings::lookup_hir_trait_item (CrateNum crateNum, HirId id)
{
  auto it = hirTraitItemMappings.find (crateNum);
  if (it == hirTraitItemMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_impl_block (CrateNum crateNum, HirId id,
				 HIR::ImplBlock *item)
{
  rust_assert (lookup_hir_impl_block (crateNum, id) == nullptr);

  hirImplBlockMappings[crateNum][id] = item;
  nodeIdToHirMappings[crateNum][item->get_mappings ().get_nodeid ()] = id;
}

HIR::ImplBlock *
Mappings::lookup_hir_impl_block (CrateNum crateNum, HirId id)
{
  auto it = hirImplBlockMappings.find (crateNum);
  if (it == hirImplBlockMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_implitem (CrateNum crateNum, HirId id,
			       HirId parent_impl_id, HIR::ImplItem *item)
{
  rust_assert (lookup_hir_implitem (crateNum, id, nullptr) == nullptr);
  hirImplItemMappings[crateNum][id]
    = std::pair<HirId, HIR::ImplItem *> (parent_impl_id, item);
  nodeIdToHirMappings[crateNum][item->get_impl_mappings ().get_nodeid ()] = id;
}

HIR::ImplItem *
Mappings::lookup_hir_implitem (CrateNum crateNum, HirId id,
			       HirId *parent_impl_id)
{
  auto it = hirImplItemMappings.find (crateNum);
  if (it == hirImplItemMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  std::pair<HirId, HIR::ImplItem *> &ref = iy->second;
  if (parent_impl_id != nullptr)
    *parent_impl_id = ref.first;

  return ref.second;
}

void
Mappings::insert_hir_expr (CrateNum crateNum, HirId id, HIR::Expr *expr)
{
  hirExprMappings[crateNum][id] = expr;
  nodeIdToHirMappings[crateNum][expr->get_mappings ().get_nodeid ()] = id;
  insert_location (crateNum, id, expr->get_locus_slow ());
}

HIR::Expr *
Mappings::lookup_hir_expr (CrateNum crateNum, HirId id)
{
  auto it = hirExprMappings.find (crateNum);
  if (it == hirExprMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_type (CrateNum crateNum, HirId id, HIR::Type *type)
{
  rust_assert (lookup_hir_type (crateNum, id) == nullptr);

  hirTypeMappings[crateNum][id] = type;
  nodeIdToHirMappings[crateNum][type->get_mappings ().get_nodeid ()] = id;
}

HIR::Type *
Mappings::lookup_hir_type (CrateNum crateNum, HirId id)
{
  auto it = hirTypeMappings.find (crateNum);
  if (it == hirTypeMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_stmt (CrateNum crateNum, HirId id, HIR::Stmt *type)
{
  rust_assert (lookup_hir_stmt (crateNum, id) == nullptr);

  hirStmtMappings[crateNum][id] = type;
  nodeIdToHirMappings[crateNum][type->get_mappings ().get_nodeid ()] = id;
}

HIR::Stmt *
Mappings::lookup_hir_stmt (CrateNum crateNum, HirId id)
{
  auto it = hirStmtMappings.find (crateNum);
  if (it == hirStmtMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_param (CrateNum crateNum, HirId id,
			    HIR::FunctionParam *param)
{
  rust_assert (lookup_hir_stmt (crateNum, id) == nullptr);

  hirParamMappings[crateNum][id] = param;
  nodeIdToHirMappings[crateNum][param->get_mappings ().get_nodeid ()] = id;
}

HIR::FunctionParam *
Mappings::lookup_hir_param (CrateNum crateNum, HirId id)
{
  auto it = hirParamMappings.find (crateNum);
  if (it == hirParamMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_self_param (CrateNum crateNum, HirId id,
				 HIR::SelfParam *param)
{
  rust_assert (lookup_hir_stmt (crateNum, id) == nullptr);

  hirSelfParamMappings[crateNum][id] = param;
  nodeIdToHirMappings[crateNum][param->get_mappings ().get_nodeid ()] = id;
}

HIR::SelfParam *
Mappings::lookup_hir_self_param (CrateNum crateNum, HirId id)
{
  auto it = hirSelfParamMappings.find (crateNum);
  if (it == hirSelfParamMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
}

void
Mappings::insert_hir_struct_field (CrateNum crateNum, HirId id,
				   HIR::StructExprField *field)
{
  rust_assert (lookup_hir_stmt (crateNum, id) == nullptr);

  hirStructFieldMappings[crateNum][id] = field;
  nodeIdToHirMappings[crateNum][field->get_mappings ().get_nodeid ()] = id;
}

HIR::StructExprField *
Mappings::lookup_hir_struct_field (CrateNum crateNum, HirId id)
{
  auto it = hirStructFieldMappings.find (crateNum);
  if (it == hirStructFieldMappings.end ())
    return nullptr;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return nullptr;

  return iy->second;
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
Mappings::insert_node_to_hir (CrateNum crate, NodeId id, HirId ref)
{
  nodeIdToHirMappings[crate][id] = ref;
}

bool
Mappings::lookup_node_to_hir (CrateNum crate, NodeId id, HirId *ref)
{
  auto it = nodeIdToHirMappings.find (crate);
  if (it == nodeIdToHirMappings.end ())
    return false;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return false;

  *ref = iy->second;
  return true;
}

void
Mappings::insert_location (CrateNum crate, HirId id, Location locus)
{
  locations[crate][id] = locus;
}

Location
Mappings::lookup_location (CrateNum crate, HirId id)
{
  auto it = locations.find (crate);
  if (it == locations.end ())
    return Location ();

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return Location ();

  return iy->second;
}

bool
Mappings::resolve_nodeid_to_stmt (CrateNum crate, NodeId id, HIR::Stmt **stmt)
{
  auto it = nodeIdToHirMappings.find (crate);
  if (it == nodeIdToHirMappings.end ())
    return false;

  auto iy = it->second.find (id);
  if (iy == it->second.end ())
    return false;

  HirId resolved = iy->second;
  auto resolved_stmt = lookup_hir_stmt (crate, resolved);
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
      for (auto iy = it->second.begin (); iy != it->second.end (); iy++)
	{
	  auto id = iy->first;
	  auto impl_item = iy->second.second;
	  auto impl = lookup_associated_impl (
	    impl_item->get_impl_mappings ().get_hirid ());
	  if (!cb (id, impl_item, impl))
	    return;
	}
    }
}

void
Mappings::iterate_impl_blocks (std::function<bool (HirId, HIR::ImplBlock *)> cb)
{
  for (auto it = hirImplBlockMappings.begin ();
       it != hirImplBlockMappings.end (); it++)
    {
      for (auto iy = it->second.begin (); iy != it->second.end (); iy++)
	{
	  HirId id = iy->first;
	  HIR::ImplBlock *impl_block = iy->second;
	  if (!cb (id, impl_block))
	    return;
	}
    }
}

} // namespace Analysis
} // namespace Rust
