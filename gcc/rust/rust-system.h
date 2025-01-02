// rust-system.h -- Rust frontend inclusion of gcc header files   -*- C++ -*-
// Copyright (C) 2009-2025 Free Software Foundation, Inc.

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

#ifndef RUST_SYSTEM_H
#define RUST_SYSTEM_H

#define INCLUDE_ALGORITHM
#include "config.h"

/* Define this so that inttypes.h defines the PRI?64 macros even
   when compiling with a C++ compiler.  Define it here so in the
   event inttypes.h gets pulled in by another header it is already
   defined.  */
#define __STDC_FORMAT_MACROS

// These must be included before the #poison declarations in system.h.

#include <string>
#include <list>
#include <map>
#include <set>
#include <vector>
#include <stack>
#include <sstream>
#include <string>
#include <deque>
#include <functional>
#include <memory>
#include <utility>
#include <fstream>
#include <array>
#include <algorithm>
#include <limits>
#include <numeric>

// Rust frontend requires C++11 minimum, so will have unordered_map and set
#include <unordered_map>
#include <unordered_set>

/* We don't really need iostream, but some versions of gmp.h include
 * it when compiled with C++, which means that we need to include it
 * before the macro magic of safe-ctype.h, which is included by
 * system.h. */
#include <iostream>
#include <iomanip>

#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"

#include "diagnostic-core.h" /* For error_at and friends.  */
#include "intl.h"	     /* For _().  */

#define RUST_ATTRIBUTE_NORETURN ATTRIBUTE_NORETURN

// File separator to use based on whether or not the OS we're working with is
// DOS-based
#if defined(HAVE_DOS_BASED_FILE_SYSTEM)
constexpr static const char *file_separator = "\\";
#else
constexpr static const char *file_separator = "/";
#endif /* HAVE_DOS_BASED_FILE_SYSTEM */

// When using gcc, rust_assert is just gcc_assert.
#define rust_assert(EXPR) gcc_assert (EXPR)

/**
 * rust_unreachable is just a fancy abort which causes an internal compiler
 * error. This macro is not equivalent to `__builtin_unreachable` and does not
 * indicate optimizations for the compiler
 */
#define rust_unreachable() (fancy_abort (__FILE__, __LINE__, __FUNCTION__))

extern void
rust_preserve_from_gc (tree t);

extern const char *
rust_localize_identifier (const char *ident);

#endif // !defined(RUST_SYSTEM_H)
