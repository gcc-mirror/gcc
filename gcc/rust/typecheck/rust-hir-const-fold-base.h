// Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_CONST_FOLD_BASE_H
#define RUST_HIR_CONST_FOLD_BASE_H

#include "rust-diagnostics.h"
#include "rust-hir-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"
#include "rust-name-resolver.h"
#include "rust-hir-const-fold-ctx.h"

namespace Rust {
namespace ConstFold {

// base class to allow derivatives to overload as needed
class ConstFoldBase : public HIR::HIRFullVisitorBase
{
public:
  virtual ~ConstFoldBase () {}

protected:
  ConstFoldBase ()
    : mappings (Analysis::Mappings::get ()),
      resolver (Resolver::Resolver::get ()),
      tyctx (Resolver::TypeCheckContext::get ()), ctx (Context::get ())
  {}

  Analysis::Mappings *mappings;
  Resolver::Resolver *resolver;
  Resolver::TypeCheckContext *tyctx;
  Context *ctx;
};

} // namespace ConstFold
} // namespace Rust

#endif // RUST_HIR_CONST_FOLD_BASE_H
