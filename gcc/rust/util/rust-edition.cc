// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-edition.h"
#include "rust-session-manager.h"

namespace Rust {

Edition
get_rust_edition ()
{
  switch (Session::get_instance ().options.get_edition ())
    {
    case CompileOptions::Edition::E2015:
      return Edition::E2015;
    case CompileOptions::Edition::E2018:
      return Edition::E2018;
    case CompileOptions::Edition::E2021:
      return Edition::E2021;
    default:
      rust_unreachable ();
    }
}

} // namespace Rust
