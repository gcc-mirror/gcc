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

#ifndef RUST_HIR_METHOD_RESOLVE_H
#define RUST_HIR_METHOD_RESOLVE_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"
#include "rust-hir-path-probe.h"
#include "rust-substitution-mapper.h"

namespace Rust {
namespace Resolver {

class MethodResolution : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static std::vector<PathProbeCandidate>
  Probe (std::vector<PathProbeCandidate> &path_candidates)
  {
    MethodResolution probe;
    for (auto &c : path_candidates)
      probe.process_candidate (c);

    return probe.candidates;
  }

  void process_candidate (PathProbeCandidate &candidate)
  {
    is_method_flag = false;
    candidate.impl_item->accept_vis (*this);

    if (is_method_flag)
      candidates.push_back (candidate);
  }

  void visit (HIR::Method &method) override { is_method_flag = true; }

private:
  MethodResolution () : TypeCheckBase () {}

  bool is_method_flag;
  std::vector<PathProbeCandidate> candidates;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_METHOD_RESOLVE_H
