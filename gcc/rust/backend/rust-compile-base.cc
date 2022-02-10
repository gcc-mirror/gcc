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

#include "rust-compile-base.h"
#include "stringpool.h"

namespace Rust {
namespace Compile {

void
HIRCompileBase::setup_attributes_on_fndecl (
  tree fndecl, bool is_main_entry_point, bool has_visibility,
  const HIR::FunctionQualifiers &qualifiers, const AST::AttrVec &attrs)
{
  // if its the main fn or pub visibility mark its as DECL_PUBLIC
  // please see https://github.com/Rust-GCC/gccrs/pull/137
  if (is_main_entry_point || has_visibility)
    {
      TREE_PUBLIC (fndecl) = 1;
    }

  // is it a const fn
  if (qualifiers.is_const ())
    {
      TREE_READONLY (fndecl) = 1;
    }

  // is it inline?
  for (const auto &attr : attrs)
    {
      bool is_inline = attr.get_path ().as_string ().compare ("inline") == 0;
      if (is_inline)
	{
	  DECL_DECLARED_INLINE_P (fndecl) = 1;

	  // do we want to force inline regardless of optimisation level?
	  // https://gcc.gnu.org/onlinedocs/gcc/Inline.html
	  //
	  // /* Add attribute "always_inline": */
	  // DECL_ATTRIBUTES (fndecl)
	  //   = tree_cons (get_identifier ("always_inline"), NULL,
	  //       	 DECL_ATTRIBUTES (fndecl));
	}
    }
}

void
HIRCompileBase::setup_abi_options (tree fndecl, ABI abi)
{
  switch (abi)
    {
    case Rust::ABI::RUST:
    case Rust::ABI::INTRINSIC:
    case Rust::ABI::C:
    case Rust::ABI::CDECL:
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (get_identifier ("cdecl"), NULL, DECL_ATTRIBUTES (fndecl));
      break;

    case Rust::ABI::STDCALL:
      DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("stdcall"), NULL,
					    DECL_ATTRIBUTES (fndecl));
      break;

    case Rust::ABI::FASTCALL:
      DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("fastcall"), NULL,
					    DECL_ATTRIBUTES (fndecl));

      break;

    default:
      break;
    }
}

} // namespace Compile
} // namespace Rust
