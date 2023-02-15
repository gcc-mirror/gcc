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

// rust-diagnostics.h -- interface to diagnostic reporting   -*- C++ -*-

#ifndef RUST_DIAGNOSTICS_H
#define RUST_DIAGNOSTICS_H

#include "rust-linemap.h"

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)
#define RUST_ATTRIBUTE_GCC_DIAG(m, n)                                          \
  __attribute__ ((__format__ (__gcc_tdiag__, m, n)))                           \
    __attribute__ ((__nonnull__ (m)))
#else
#define RUST_ATTRIBUTE_GCC_DIAG(m, n)
#endif

// These declarations define the interface through which the frontend
// reports errors and warnings. These functions accept printf-like
// format specifiers (e.g. %d, %f, %s, etc), with the following additional
// extensions:
//
//  1.  'q' qualifier may be applied to a specifier to add quoting, e.g.
//      %qd produces a quoted decimal output, %qs a quoted string output.
//      [This extension is supported only with single-character format
//      specifiers].
//
//  2.  %m specifier outputs value of "strerror(errno)" at time of call.
//
//  3.  %< outputs an opening quote, %> a closing quote.
//
// All other format specifiers are as defined by 'sprintf'. The final resulting
// message is then sent to the back end via rust_be_error_at/rust_be_warning_at.

// clang-format off
// simple location
extern void
rust_internal_error_at (const Location, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_error_at (const Location, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
extern void
rust_warning_at (const Location, int opt, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
extern void
rust_fatal_error (const Location, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_inform (const Location, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);

// rich locations
extern void
rust_error_at (const RichLocation &, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
// clang-format on

// These interfaces provide a way for the front end to ask for
// the open/close quote characters it should use when formatting
// diagnostics (warnings, errors).
extern const char *
rust_open_quote ();
extern const char *
rust_close_quote ();

// These interfaces are used by utilities above to pass warnings and
// errors (once format specifiers have been expanded) to the back end,
// and to determine quoting style. Avoid calling these routines directly;
// instead use the equivalent routines above. The back end is required to
// implement these routines.

// clang-format off
extern void
rust_be_internal_error_at (const Location, const std::string &errmsg)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_be_error_at (const Location, const std::string &errmsg);
extern void
rust_be_error_at (const RichLocation &, const std::string &errmsg);
extern void
rust_be_warning_at (const Location, int opt, const std::string &warningmsg);
extern void
rust_be_fatal_error (const Location, const std::string &errmsg)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_be_inform (const Location, const std::string &infomsg);
extern void
rust_be_get_quotechars (const char **open_quote, const char **close_quote);
extern bool
rust_be_debug_p (void);
// clang-format on

namespace Rust {
/* A structure used to represent an error. Useful for enabling
 * errors to be ignored, e.g. if backtracking. */
struct Error
{
  enum class Kind
  {
    Hint,
    Err,
    FatalErr,
  };

  Kind kind;
  Location locus;
  std::string message;
  // TODO: store more stuff? e.g. node id?

  Error (Kind kind, Location locus, std::string message)
    : kind (kind), locus (locus), message (std::move (message))
  {
    message.shrink_to_fit ();
  }

  Error (Location locus, std::string message)
  {
    Error (Kind::Err, locus, std::move (message));
  }

  static Error Hint (Location locus, std::string message)
  {
    return Error (Kind::Hint, locus, std::move (message));
  }

  static Error Fatal (Location locus, std::string message)
  {
    return Error (Kind::FatalErr, locus, std::move (message));
  }

  // TODO: the attribute part might be incorrect
  Error (Location locus, const char *fmt,
	 ...) /*RUST_ATTRIBUTE_GCC_DIAG (2, 3)*/ RUST_ATTRIBUTE_GCC_DIAG (3, 4);

  /**
   * printf-like overload of Error::Hint
   */
  static Error Hint (Location locus, const char *fmt, ...)
    RUST_ATTRIBUTE_GCC_DIAG (2, 3);

  /**
   * printf-like overload of Error::Fatal
   */
  static Error Fatal (Location locus, const char *fmt, ...)
    RUST_ATTRIBUTE_GCC_DIAG (2, 3);

  void emit () const
  {
    switch (kind)
      {
      case Kind::Hint:
	rust_inform (locus, "%s", message.c_str ());
	break;
      case Kind::Err:
	rust_error_at (locus, "%s", message.c_str ());
	break;
      case Kind::FatalErr:
	rust_fatal_error (locus, "%s", message.c_str ());
	break;
      }
  }
};
} // namespace Rust

// rust_debug uses normal printf formatting, not GCC diagnostic formatting.
#define rust_debug(...) rust_debug_loc (Location (), __VA_ARGS__)

// rust_sorry_at wraps GCC diagnostic "sorry_at" to accept "Location" instead of
// "location_t"
#define rust_sorry_at(location, ...)                                           \
  sorry_at (location.gcc_location (), __VA_ARGS__)

void
rust_debug_loc (const Location location, const char *fmt,
		...) ATTRIBUTE_PRINTF_2;

#endif // !defined(RUST_DIAGNOSTICS_H)
