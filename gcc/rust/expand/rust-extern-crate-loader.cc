// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-extern-crate-loader.h"
#include "rust-session-manager.h"

namespace Rust {
ExternCrateLoaderVisitor::ExternCrateLoaderVisitor (
  std::vector<Session::LoadedCrate> &loaded_crate)
  : loaded_crates (loaded_crate)
{}

void
ExternCrateLoaderVisitor::go (AST::Crate &crate)
{
  visit (crate);
}

void
ExternCrateLoaderVisitor::visit (AST::ExternCrate &extern_crate)
{
  if (extern_crate.references_self ())
    return;

  Session &session = Session::get_instance ();
  auto crate = session.load_extern_crate (extern_crate.get_referenced_crate (),
					  extern_crate.get_locus ());
  if (crate)
    loaded_crates.push_back (std::move (crate.value ()));
}

} // namespace Rust
