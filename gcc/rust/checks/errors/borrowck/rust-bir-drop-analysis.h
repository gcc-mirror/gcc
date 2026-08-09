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

#ifndef RUST_BIR_DROP_ANALYSIS_H
#define RUST_BIR_DROP_ANALYSIS_H

#include "rust-bir.h"

namespace Rust {
namespace BIR {

/*
  Classifies scheduled whole-local BIR Drop statements according to
  whether their place is initialized at the drop point.

  This analysis tracks initialization state across the BIR control-flow graph.
*/
class DropAnalysis
{
public:
  static DropAnalysis &get ();

  void clear ();
  void analyze (Function &function);

  bool is_definitely_dead (HirId id) const;

private:
  std::set<HirId> definitely_dead;
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_DROP_ANALYSIS_H
