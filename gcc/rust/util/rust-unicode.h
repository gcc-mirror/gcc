// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_UNICODE_H
#define RUST_UNICODE_H

#include "rust-system.h"

namespace Rust {

// TODO: add function nfc_normalize

bool
is_alphabetic (uint32_t codepoint);

bool
is_numeric (uint32_t codepoint);

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
rust_utf8_normalize_test ();

void
rust_utf8_property_test ();

} // namespace selftest

#endif // CHECKING_P

#endif
