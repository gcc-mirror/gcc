// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "util/optional.h"

// This macro is used to specify the position of format string & it's
// arguments within the function's paramter list.
// 'm' specifies the position of the format string parameter.
// 'n' specifies the position of the first argument for the format string.
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

// simple location

// https://gist.github.com/MahadMuhammad/8c9d5fc88ea18d8c520937a8071d4185

// We want E0005 to be mapped to the value `5` - this way, we can easily format
// it in `make_description`. We also want to keep the value "5" only once when
// defining the error code in rust-error-codes.def, so not have ERROR(E0005, 5)
// as that is error prone. If we just use `0005` as the discriminant for the
// `E0005` enum variant, then we are actually creating octal values (!) as `0`
// is the C/C++ octal prefix. So this does not work for `E0009` for example,
// since 0009 is not a valid octal literal.
// We can circumvent this by doing a little bit of constant folding in the
// discriminant expression. So for ERROR(E0009), this macro expands to the
// following variant:
//
// E0009 = (10009 - 10000)
//
// which gets folded to the result of the substraction... 9. A valid decimal
// literal which corresponds to E0009.
#define ERROR(N) E##N = (1##N - 10000)
enum class ErrorCode : unsigned int
{
#include "rust-error-codes.def"
};
#undef ERROR

// clang-format off
extern void
rust_internal_error_at (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_error_at (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
extern void
rust_error_at (const location_t, const ErrorCode, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
extern void
rust_warning_at (const location_t, int opt, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
extern void
rust_fatal_error (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_inform (const location_t, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);

// rich locations
extern void
rust_error_at (const rich_location &, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
extern void
rust_error_at (const rich_location &, const ErrorCode, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
extern void /* similiar to other frontends */
rust_error_at(rich_location *richloc,const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (2, 3);
extern void /* similiar to other frontends */
rust_error_at(rich_location *richloc, const ErrorCode, const char *fmt, ...)
  RUST_ATTRIBUTE_GCC_DIAG (3, 4);
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
rust_be_internal_error_at (const location_t, const std::string &errmsg)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_be_error_at (const location_t, const std::string &errmsg);
extern void
rust_be_error_at (const location_t, const ErrorCode,
		  const std::string &errmsg);
extern void
rust_be_error_at (const rich_location &, const std::string &errmsg);
extern void
rust_be_error_at (const rich_location &, const ErrorCode,
		  const std::string &errmsg);
extern void /* similiar to other frontends */
rust_be_error_at (rich_location *richloc, const std::string &errmsg);
extern void /* similiar to other frontends */
rust_be_error_at (rich_location *richloc, const ErrorCode,
      const std::string &errmsg);
extern void
rust_be_warning_at (const location_t, int opt, const std::string &warningmsg);
extern void
rust_be_fatal_error (const location_t, const std::string &errmsg)
  RUST_ATTRIBUTE_NORETURN;
extern void
rust_be_inform (const location_t, const std::string &infomsg);
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
  location_t locus;
  rich_location *richlocus = nullptr;
  ErrorCode errorcode;
  std::string message;
  bool is_errorcode = false;

  // simple location
  Error (Kind kind, location_t locus, std::string message)
    : kind (kind), locus (locus), message (std::move (message))
  {
    message.shrink_to_fit ();
  }
  // simple location + error code
  Error (Kind kind, location_t locus, ErrorCode code, std::string message)
    : kind (kind), locus (locus), errorcode (std::move (code)),
      message (std::move (message))
  {
    is_errorcode = true;
    message.shrink_to_fit ();
  }
  // rich location
  Error (Kind kind, rich_location *richlocus, std::string message)
    : kind (kind), richlocus (richlocus), message (std::move (message))
  {
    message.shrink_to_fit ();
  }
  // rich location + error code
  Error (Kind kind, rich_location *richlocus, ErrorCode code,
	 std::string message)
    : kind (kind), richlocus (richlocus), errorcode (std::move (code)),
      message (std::move (message))
  {
    is_errorcode = true;
    message.shrink_to_fit ();
  }
  // simple location
  Error (location_t locus, std::string message)
  {
    Error (Kind::Err, locus, std::move (message));
  }
  // simple location + error code
  Error (location_t locus, ErrorCode code, std::string message)
  {
    Error (Kind::Err, locus, std::move (code), std::move (message));
  }
  // rich location
  Error (rich_location *richlocus, std::string message)
  {
    Error (Kind::Err, richlocus, std::move (message));
  }
  // rich location + error code
  Error (rich_location *richlocus, ErrorCode code, std::string message)
  {
    Error (Kind::Err, richlocus, std::move (code), std::move (message));
  }

  static Error Hint (location_t locus, std::string message)
  {
    return Error (Kind::Hint, locus, std::move (message));
  }

  static Error Fatal (location_t locus, std::string message)
  {
    return Error (Kind::FatalErr, locus, std::move (message));
  }

  // TODO: the attribute part might be incorrect
  // simple location
  Error (location_t locus, const char *fmt,
	 ...) /*RUST_ATTRIBUTE_GCC_DIAG (2, 3)*/ RUST_ATTRIBUTE_GCC_DIAG (3, 4);

  // simple location + error code
  Error (location_t locus, ErrorCode code, const char *fmt,
	 ...) /*RUST_ATTRIBUTE_GCC_DIAG (3, 4)*/ RUST_ATTRIBUTE_GCC_DIAG (4, 5);

  // rich location
  Error (rich_location *richlocus, const char *fmt,
	 ...) /*RUST_ATTRIBUTE_GCC_DIAG (2, 3)*/ RUST_ATTRIBUTE_GCC_DIAG (3, 4);

  // rich location + error code
  Error (rich_location *richlocus, ErrorCode code, const char *fmt,
	 ...) /*RUST_ATTRIBUTE_GCC_DIAG (3, 4)*/ RUST_ATTRIBUTE_GCC_DIAG (4, 5);

  /**
   * printf-like overload of Error::Hint
   */
  static Error Hint (location_t locus, const char *fmt, ...)
    RUST_ATTRIBUTE_GCC_DIAG (2, 3);

  /**
   * printf-like overload of Error::Fatal
   */
  static Error Fatal (location_t locus, const char *fmt, ...)
    RUST_ATTRIBUTE_GCC_DIAG (2, 3);

  void emit () const
  {
    switch (kind)
      {
      case Kind::Hint:
	rust_inform (locus, "%s", message.c_str ());
	break;
      case Kind::Err:
	if (is_errorcode)
	  {
	    if (richlocus == nullptr)
	      rust_error_at (locus, errorcode, "%s", message.c_str ());
	    else
	      rust_error_at (*richlocus, errorcode, "%s", message.c_str ());
	  }
	else
	  {
	    if (richlocus == nullptr)
	      rust_error_at (locus, "%s", message.c_str ());
	    else
	      rust_error_at (*richlocus, "%s", message.c_str ());
	  }
	break;
      case Kind::FatalErr:
	rust_fatal_error (locus, "%s", message.c_str ());
	break;
      }
  }
};
} // namespace Rust

// rust_debug uses normal printf formatting, not GCC diagnostic formatting.
#define rust_debug(...) rust_debug_loc (UNDEF_LOCATION, __VA_ARGS__)

#define rust_sorry_at(location, ...) sorry_at (location, __VA_ARGS__)

void
rust_debug_loc (const location_t location, const char *fmt,
		...) ATTRIBUTE_PRINTF_2;

#endif // !defined(RUST_DIAGNOSTICS_H)
