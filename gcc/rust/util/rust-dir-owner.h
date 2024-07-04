// Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

// Handles non-mod-rs and mod-rs file differentiation

#ifndef RUST_DIR_OWNER
#define RUST_DIR_OWNER

#include "rust-system.h"

namespace Rust {

// extracts the owned subdirectory name from a file name
bool
get_file_subdir (const std::string &filename, std::string &subdir);

} // namespace Rust

#endif // RUST_DIR_OWNER
