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

#include "rust-immutable-name-resolution-context.h"

namespace Rust {
namespace Resolver2_0 {

static ImmutableNameResolutionContext *instance = nullptr;

const ImmutableNameResolutionContext &
ImmutableNameResolutionContext::init (const NameResolutionContext &ctx)
{
  rust_assert (!instance);

  instance = new ImmutableNameResolutionContext (ctx);

  return *instance;
}

const ImmutableNameResolutionContext &
ImmutableNameResolutionContext::get ()
{
  rust_assert (instance);

  return *instance;
}

const NameResolutionContext &
ImmutableNameResolutionContext::resolver () const
{
  return ctx;
}

ImmutableNameResolutionContext::ImmutableNameResolutionContext (
  const NameResolutionContext &ctx)
  : ctx (ctx)
{}

} // namespace Resolver2_0
} // namespace Rust
