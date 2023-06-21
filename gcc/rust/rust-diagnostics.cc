// rust-diagnostics.cc -- GCC implementation of rust diagnostics interface.
// Copyright (C) 2016-2023 Free Software Foundation, Inc.
// Contributed by Than McIntosh, Google.

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

#include "rust-system.h"
#include "rust-diagnostics.h"

static std::string
mformat_value ()
{
  return std::string (xstrerror (errno));
}

// Rewrite a format string to expand any extensions not
// supported by sprintf(). See comments in rust-diagnostics.h
// for list of supported format specifiers.

static std::string
expand_format (const char *fmt)
{
  std::stringstream ss;
  for (const char *c = fmt; *c; ++c)
    {
      if (*c != '%')
	{
	  ss << *c;
	  continue;
	}
      c++;
      switch (*c)
	{
	  case '\0': {
	    // malformed format string
	    rust_unreachable ();
	  }
	  case '%': {
	    ss << "%";
	    break;
	  }
	  case 'm': {
	    ss << mformat_value ();
	    break;
	  }
	  case '<': {
	    ss << rust_open_quote ();
	    break;
	  }
	  case '>': {
	    ss << rust_close_quote ();
	    break;
	  }
	  case 'q': {
	    ss << rust_open_quote ();
	    c++;
	    if (*c == 'm')
	      {
		ss << mformat_value ();
	      }
	    else
	      {
		ss << "%" << *c;
	      }
	    ss << rust_close_quote ();
	    break;
	  }
	  default: {
	    ss << "%" << *c;
	  }
	}
    }
  return ss.str ();
}

// Expand message format specifiers, using a combination of
// expand_format above to handle extensions (ex: %m, %q) and vasprintf()
// to handle regular printf-style formatting. A pragma is being used here to
// suppress this warning:
//
//   warning: function ‘std::__cxx11::string expand_message(const char*,
//   __va_list_tag*)’ might be a candidate for ‘gnu_printf’ format attribute
//   [-Wsuggest-attribute=format]
//
// What appears to be happening here is that the checker is deciding that
// because of the call to vasprintf() (which has attribute gnu_printf), the
// calling function must need to have attribute gnu_printf as well, even
// though there is already an attribute declaration for it.

static std::string
expand_message (const char *fmt, va_list ap) RUST_ATTRIBUTE_GCC_DIAG (1, 0);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-attribute=format"

static std::string
expand_message (const char *fmt, va_list ap)
{
  char *mbuf = 0;
  std::string expanded_fmt = expand_format (fmt);
  int nwr = vasprintf (&mbuf, expanded_fmt.c_str (), ap);
  if (nwr == -1)
    {
      // memory allocation failed
      rust_be_error_at (Linemap::unknown_location (),
			"memory allocation failed in vasprintf");
      rust_assert (0);
    }
  std::string rval = std::string (mbuf);
  free (mbuf);
  return rval;
}

#pragma GCC diagnostic pop

static const char *cached_open_quote = NULL;
static const char *cached_close_quote = NULL;

const char *
rust_open_quote ()
{
  if (cached_open_quote == NULL)
    rust_be_get_quotechars (&cached_open_quote, &cached_close_quote);
  return cached_open_quote;
}

const char *
rust_close_quote ()
{
  if (cached_close_quote == NULL)
    rust_be_get_quotechars (&cached_open_quote, &cached_close_quote);
  return cached_close_quote;
}

void
rust_internal_error_at (const Location location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_internal_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_error_at (const Location location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_warning_at (const Location location, int opt, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_warning_at (location, opt, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_fatal_error (const Location location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_fatal_error (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_inform (const Location location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_inform (location, expand_message (fmt, ap));
  va_end (ap);
}

// Rich Locations
void
rust_error_at (const RichLocation &location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_debug_loc (const Location location, const char *fmt, ...)
{
  if (!rust_be_debug_p ())
    return;

  va_list ap;

  va_start (ap, fmt);
  char *mbuf = NULL;
  int nwr = vasprintf (&mbuf, fmt, ap);
  va_end (ap);
  if (nwr == -1)
    {
      rust_be_error_at (Linemap::unknown_location (),
			"memory allocation failed in vasprintf");
      rust_assert (0);
    }
  std::string rval = std::string (mbuf);
  free (mbuf);
  rust_be_inform (location, rval);
}

namespace Rust {

/**
 * This function takes ownership of `args` and calls `va_end` on it
 */
static Error
va_constructor (Error::Kind kind, Location locus, const char *fmt, va_list args)
  RUST_ATTRIBUTE_GCC_DIAG (3, 0);

static Error
va_constructor (Error::Kind kind, Location locus, const char *fmt, va_list args)
{
  std::string message = expand_message (fmt, args);
  message.shrink_to_fit ();
  va_end (args);

  return Error (kind, locus, message);
}

Error::Error (const Location location, const char *fmt, ...)
  : kind (Kind::Err), locus (location)
{
  va_list ap;
  va_start (ap, fmt);

  *this = va_constructor (Kind::Err, location, fmt, ap);
}

Error
Error::Hint (const Location location, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);

  return va_constructor (Kind::Hint, location, fmt, ap);
}

Error
Error::Fatal (const Location location, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);

  return va_constructor (Kind::FatalErr, location, fmt, ap);
}

} // namespace Rust
