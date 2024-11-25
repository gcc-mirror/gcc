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

#ifndef RUST_ICE_FINALIZER_H
#define RUST_ICE_FINALIZER_H

#include "rust-linemap.h"
#include "diagnostic.h"

namespace Rust {
namespace Resolver {

/* The "break rust" Easter egg.

   Backstory: once upon a time, there used to be a bug in rustc: it would ICE
   during typechecking on a 'break' with an expression outside of a loop.  The
   issue has been reported [0] and fixed [1], but in recognition of this, as a
   special Easter egg, "break rust" was made to intentionally cause an ICE.

   [0]: https://github.com/rust-lang/rust/issues/43162
   [1]: https://github.com/rust-lang/rust/pull/43745

   This was made in a way that does not break valid programs: namely, it only
   happens when the 'break' is outside of a loop (so invalid anyway).

   GCC Rust supports this essential feature as well, but in a slightly
   different way.  Instead of delaying the error until type checking, we emit
   it here in the resolution phase.  We, too, only do this to programs that
   are already invalid: we only emit our funny ICE if the name "rust" (which
   must be immediately inside a break-with-a-value expression) fails to
   resolve.  Note that "break (rust)" does not trigger our ICE, only using
   "break rust" directly does, and only if there's no "rust" in scope.  We do
   this in the same way regardless of whether the "break" is outside of a loop
   or inside one.

   As a GNU extension, we also support "break gcc", much to the same effect,
   subject to the same rules.  */

/* The finalizer for our funny ICE.  This prints a custom message instead of
   the default bug reporting instructions, as there is no bug to report.  */

void ATTRIBUTE_NORETURN
funny_ice_text_finalizer (diagnostic_text_output_format &text_output,
			  const diagnostic_info *diagnostic,
			  diagnostic_t diag_kind);

} // namespace Resolver
} // namespace Rust

#endif /* ! RUST_ICE_FINALIZER_H */
