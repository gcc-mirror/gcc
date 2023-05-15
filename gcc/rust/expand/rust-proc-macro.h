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

#ifndef RUST_PROC_MACRO_H
#define RUST_PROC_MACRO_H

#include "libproc_macro/proc_macro.h"

namespace Rust {

/**
 * Load a procedural macro library and return a pointer to it's entrypoint.
 *
 * @param The path to the shared object file to load.
 */
const std::vector<ProcMacro::Procmacro>
load_macros (std::string path);

} // namespace Rust

#endif /* ! RUST_PROC_MACRO_H */
