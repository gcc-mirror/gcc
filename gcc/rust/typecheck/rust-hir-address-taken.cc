// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#include "rust-hir-address-taken.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Resolver {

AddressTakenContext *
AddressTakenContext::get ()
{
  static AddressTakenContext *instance;
  if (instance == nullptr)
    instance = new AddressTakenContext ();

  return instance;
}

AddressTakenContext::~AddressTakenContext () {}

bool
AddressTakenContext::lookup_addess_taken (HirId id, bool *address_taken) const
{
  const auto &it = ctx.find (id);
  if (it == ctx.end ())
    return false;

  *address_taken = it->second;
  return true;
}

void
AddressTakenContext::insert_address_taken (HirId id, bool address_taken)
{
  const auto &it = ctx.find (id);
  if (it != ctx.end ())
    {
      // assert that we never change a true result to a negative
      if (it->second == true)
	{
	  rust_assert (address_taken != false);
	}
    }

  ctx[id] = address_taken;
}

} // namespace Resolver
} // namespace Rust
