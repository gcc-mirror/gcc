// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-ice-finalizer.h"

namespace Rust {
namespace Resolver {

void ATTRIBUTE_NORETURN
funny_ice_text_finalizer (diagnostic_text_output_format &text_output,
			  const diagnostic_info *diagnostic,
			  diagnostic_t diag_kind)
{
  gcc_assert (diag_kind == DK_ICE_NOBT);
  default_diagnostic_text_finalizer (text_output, diagnostic, diag_kind);
  fnotice (stderr, "You have broken GCC Rust. This is a feature.\n");
  exit (ICE_EXIT_CODE);
}

} // namespace Resolver
} // namespace Rust
