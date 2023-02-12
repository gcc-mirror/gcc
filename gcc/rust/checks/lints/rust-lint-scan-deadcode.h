// Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_SCAN_DEADCODE
#define RUST_HIR_SCAN_DEADCODE

#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-lint-marklive.h"
#include "rust-name-resolver.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Analysis {

// Scan item symbols and warn the symbol if it is not in the live_symbols set.
// There are three kinds of item we should handle in this pass.
// 1. Function item
// 2. The function item in the impl block without trait
// 3. StructStruct, e.g., `Struct Foo{one: 1, two: 2}`. Furthermore, the unused
//    struct fields will be warned too.
// 4. TupleStruct, e.g., `Struct Foo(i32, i32)`
class ScanDeadcode : public MarkLiveBase
{
  using Rust::Analysis::MarkLiveBase::visit;

public:
  static void Scan (HIR::Crate &crate)
  {
    std::set<HirId> live_symbols = Analysis::MarkLive::Analysis (crate);
    ScanDeadcode sdc (live_symbols);
    for (auto it = crate.items.begin (); it != crate.items.end (); it++)
      {
	it->get ()->accept_vis (sdc);
      }
  };

  void visit (HIR::Function &function) override
  {
    HirId hirId = function.get_mappings ().get_hirid ();
    if (should_warn (hirId) && !function.get_visibility ().is_public ())
      {
	if (mappings->is_impl_item (hirId))
	  {
	    HIR::ImplBlock *implBlock
	      = mappings->lookup_associated_impl (hirId);
	    if (!implBlock->has_trait_ref ())
	      {
		rust_warning_at (function.get_locus (), 0,
				 "associated function is never used: %<%s%>",
				 function.get_function_name ().c_str ());
	      }
	  }
	else
	  {
	    rust_warning_at (function.get_locus (), 0,
			     "function is never used: %<%s%>",
			     function.get_function_name ().c_str ());
	  }
      }
  }

  void visit (HIR::StructStruct &stct) override
  {
    HirId hirId = stct.get_mappings ().get_hirid ();
    if (should_warn (hirId) && !stct.get_visibility ().is_public ())
      {
	bool name_starts_underscore = stct.get_identifier ().at (0) == '_';
	if (!name_starts_underscore)
	  rust_warning_at (stct.get_locus (), 0,
			   "struct is never constructed: %<%s%>",
			   stct.get_identifier ().c_str ());
      }
    else
      {
	// only warn the unused fields when in unwarned struct.
	for (auto &field : stct.get_fields ())
	  {
	    HirId field_hir_id = field.get_mappings ().get_hirid ();
	    if (should_warn (field_hir_id)
		&& !field.get_visibility ().is_public ())
	      {
		rust_warning_at (field.get_locus (), 0,
				 "field is never read: %<%s%>",
				 field.get_field_name ().c_str ());
	      }
	  }
      }
  }

  void visit (HIR::TupleStruct &stct) override
  {
    // only warn tuple struct unconstructed, and ignoring unused field
    HirId hirId = stct.get_mappings ().get_hirid ();
    if (should_warn (hirId) && !stct.get_visibility ().is_public ())
      {
	rust_warning_at (stct.get_locus (), 0,
			 "struct is never constructed: %<%s%>",
			 stct.get_identifier ().c_str ());
      }
  }

  void visit (HIR::ImplBlock &blc) override
  {
    if (blc.has_impl_items ())
      {
	for (auto &implItem : blc.get_impl_items ())
	  {
	    implItem->accept_vis (*this);
	  }
      }
  }

  void visit (HIR::Module &mod) override
  {
    for (auto &item : mod.get_items ())
      item->accept_vis (*this);
  }

private:
  std::set<HirId> live_symbols;
  Resolver::Resolver *resolver;
  Analysis::Mappings *mappings;

  ScanDeadcode (std::set<HirId> &live_symbols)
    : live_symbols (live_symbols), resolver (Resolver::Resolver::get ()),
      mappings (Analysis::Mappings::get ()){};

  bool should_warn (HirId hirId)
  {
    // TODO: There are more condition to check if should warn, i.e visibility,
    // attributes.
    return live_symbols.find (hirId) == live_symbols.end ();
  }
};

} // namespace Analysis
} // namespace Rust

#endif
