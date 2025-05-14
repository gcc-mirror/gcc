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

#ifndef RUST_TOKEN_CONVERTER_H
#define RUST_TOKEN_CONVERTER_H

#include "rust-system.h"
#include "rust-token.h"
#include "libproc_macro_internal/proc_macro.h"

namespace Rust {

ProcMacro::TokenStream
convert (const std::vector<const_TokenPtr> &tokens);

std::vector<const_TokenPtr>
convert (const ProcMacro::TokenStream &ts);

ProcMacro::Literal
convert_literal (const_TokenPtr lit);

} // namespace Rust

#endif /* ! RUST_TOKEN_CONVERTER_H */
