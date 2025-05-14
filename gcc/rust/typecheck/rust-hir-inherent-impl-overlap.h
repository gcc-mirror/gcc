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

#ifndef RUST_HIR_INHERENT_IMPL_ITEM_OVERLAP_H
#define RUST_HIR_INHERENT_IMPL_ITEM_OVERLAP_H

#include "rust-hir-type-check-base.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

class OverlappingImplItemPass : public TypeCheckBase
{
public:
  static void go ()
  {
    OverlappingImplItemPass pass;

    // generate mappings
    pass.mappings.iterate_impl_items (
      [&] (HirId id, HIR::ImplItem *impl_item, HIR::ImplBlock *impl) -> bool {
	// ignoring trait-impls might need thought later on
	if (impl->has_trait_ref ())
	  return true;

	pass.process_impl_item (id, impl_item, impl);
	return true;
      });

    pass.scan ();
  }

  void process_impl_item (HirId id, HIR::ImplItem *impl_item,
			  HIR::ImplBlock *impl)
  {
    // lets make a mapping of impl-item Self type to (impl-item,name):
    // {
    //   impl-type -> [ (item, name), ... ]
    // }

    HirId impl_type_id = impl->get_type ().get_mappings ().get_hirid ();
    TyTy::BaseType *impl_type = nullptr;
    bool ok = query_type (impl_type_id, &impl_type);
    if (!ok)
      return;

    std::string impl_item_name = impl_item->get_impl_item_name ();
    std::pair<HIR::ImplItem *, std::string> elem (impl_item, impl_item_name);
    impl_mappings[impl_type].insert (std::move (elem));
  }

  void scan ()
  {
    // we can now brute force the map looking for can_eq on each of the
    // impl_items_types to look for possible colliding impl blocks;
    for (auto it = impl_mappings.begin (); it != impl_mappings.end (); it++)
      {
	TyTy::BaseType *query = it->first;

	for (auto iy = impl_mappings.begin (); iy != impl_mappings.end (); iy++)
	  {
	    TyTy::BaseType *candidate = iy->first;
	    if (query == candidate)
	      continue;

	    if (query->can_eq (candidate, false))
	      {
		// we might be in the case that we have:
		//
		// *const T vs *const [T]
		//
		// so lets use an equality check when the
		// candidates are both generic to be sure we dont emit a false
		// positive

		bool a = query->is_concrete ();
		bool b = candidate->is_concrete ();
		bool both_generic = !a && !b;
		if (both_generic)
		  {
		    if (!query->is_equal (*candidate))
		      continue;
		  }

		possible_collision (it->second, iy->second);
	      }
	  }
      }
  }

  void possible_collision (
    std::set<std::pair<HIR::ImplItem *, std::string> > query,
    std::set<std::pair<HIR::ImplItem *, std::string> > candidate)
  {
    for (auto &q : query)
      {
	HIR::ImplItem *query_impl_item = q.first;
	std::string query_impl_item_name = q.second;

	for (auto &c : candidate)
	  {
	    HIR::ImplItem *candidate_impl_item = c.first;
	    std::string candidate_impl_item_name = c.second;

	    if (query_impl_item_name.compare (candidate_impl_item_name) == 0)
	      collision_detected (query_impl_item, candidate_impl_item,
				  candidate_impl_item_name);
	  }
      }
  }

  void collision_detected (HIR::ImplItem *query, HIR::ImplItem *dup,
			   const std::string &name)
  {
    rich_location r (line_table, dup->get_locus ());
    std::string msg = "duplicate definitions for " + name;
    r.add_fixit_replace (query->get_locus (), msg.c_str ());
    r.add_range (query->get_locus ());
    rust_error_at (r, ErrorCode::E0592, "duplicate definitions with name %qs",
		   name.c_str ());
  }

private:
  OverlappingImplItemPass () : TypeCheckBase () {}

  std::map<TyTy::BaseType *,
	   std::set<std::pair<HIR::ImplItem *, std::string> > >
    impl_mappings;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_INHERENT_IMPL_ITEM_OVERLAP_H
