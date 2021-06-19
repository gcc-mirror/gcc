// Copyright (C) 2021 Free Software Foundation, Inc.

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

#include <set>
#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"
#include "rust-lint-marklive-base.h"
#include "rust-name-resolver.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Analysis {

class ScanDeadcode : public MarkLiveBase
{
  using Rust::Analysis::MarkLiveBase::visit;

public:
  static void Scan (HIR::Crate &crate, std::set<HirId> live_symbols)
  {
    ScanDeadcode sdc (live_symbols);
    for (auto it = crate.items.begin (); it != crate.items.end (); it++)
      {
	it->get ()->accept_vis (sdc);
      }
  };

  void visit (HIR::Function &function) override
  {
    HirId hirId = function.get_mappings ().get_hirid ();
    if (live_symbols.find (hirId) == live_symbols.end ())
      {
	rust_warning_at (function.get_locus (), 0,
			 "function is never used: %<%s%>",
			 function.get_function_name ().c_str ());
	return;
      }
  }

private:
  std::set<HirId> live_symbols;
  // std::set<HirId> dead_codes;

  ScanDeadcode (std::set<HirId> &live_symbols) : live_symbols (live_symbols){};
};

} // namespace Analysis
} // namespace Rust

#endif