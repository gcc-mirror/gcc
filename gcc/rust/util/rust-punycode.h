// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#ifndef RUST_PUNYCODE_H
#define RUST_PUNYCODE_H

#include "rust-unicode.h"
#include "optional.h"

namespace Rust {

/* Encode a string as punycode. Returns a string if encoding is successful.
 * Returns nullopt otherwise. Note that a returned string contains only ASCII
 * characters and does not start with `xn--`. */
tl::optional<std::string>
encode_punycode (const Utf8String &src);

} // namespace Rust

#if CHECKING_P

namespace selftest {

void
rust_punycode_encode_test ();

} // namespace selftest

#endif // CHECKING_P

#endif
