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

#ifndef RUST_EXTERN_CRATE_LOADER_H
#define RUST_EXTERN_CRATE_LOADER_H

#include "rust-ast-visitor.h"
#include "rust-session-manager.h"

namespace Rust {

class ExternCrateLoaderVisitor : public AST::DefaultASTVisitor
{
  std::vector<Session::LoadedCrate> &loaded_crates;

public:
  using DefaultASTVisitor::visit;
  void go (AST::Crate &crate);

  void visit (AST::ExternCrate &extern_crate) override;

  ExternCrateLoaderVisitor (std::vector<Session::LoadedCrate> &loaded_crates);
};

} // namespace Rust

#endif /* ! RUST_EXTERN_CRATE_LOADER */
