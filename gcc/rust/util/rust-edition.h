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

#ifndef RUST_EDITION_H
#define RUST_EDITION_H

/*
 * This header exists to avoid including rust-session-manager.h
 * when we only need information on the selected rust edition
 */

namespace Rust {

enum class Edition
{
  E2015,
  E2018,
  E2021
};

Edition
get_rust_edition ();

} // namespace Rust

#endif // !RUST_EDITION_H
