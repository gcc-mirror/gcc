// Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#include "rust-system.h"
#include "rust-dir-owner.h"

namespace Rust {

// extracts the owned subdirectory name from a file name
bool
get_file_subdir (const std::string &filename, std::string &subdir)
{
  // directory owning filenames
  if (filename == "mod.rs" || filename == "lib.rs" || filename == "main.rs")
    return false;

  // files not ending in ".rs" are directory owners
  if (filename.size () < 3 || filename.compare (filename.size () - 3, 3, ".rs"))
    return false;

  subdir = filename.substr (0, filename.size () - 3);
  return true;
}

} // namespace Rust
