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

#ifndef RUST_HIR_TYPE_CHECK_UTIL_H
#define RUST_HIR_TYPE_CHECK_UTIL_H

#include "rust-system.h"
#include "rust-hir-visitor.h"

namespace Rust {
namespace Resolver {

class ImplTypeIterator : public HIR::HIRFullVisitorBase
{
  using HIR::HIRFullVisitorBase::visit;

public:
  ImplTypeIterator (HIR::ImplBlock &impl,
		    std::function<void (HIR::TypeAlias &alias)> cb)
    : impl (impl), cb (cb)
  {}

  void go ();

  void visit (HIR::TypeAlias &alias) override;

private:
  HIR::ImplBlock &impl;
  std::function<void (HIR::TypeAlias &alias)> cb;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_UTIL_H
