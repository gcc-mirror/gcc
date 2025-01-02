// rust-diagnostics.cc -- GCC implementation of rust diagnostics interface.
// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#include "options.h"
#include "diagnostic-metadata.h"

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
      rust_be_error_at (UNKNOWN_LOCATION,
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

void
rust_be_get_quotechars (const char **open_qu, const char **close_qu)
{
  *open_qu = open_quote;
  *close_qu = close_quote;
}

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
rust_be_internal_error_at (const location_t location, const std::string &errmsg)
{
  std::string loc_str = Linemap::location_to_string (location);
  if (loc_str.empty ())
    internal_error ("%s", errmsg.c_str ());
  else
    internal_error ("at %s, %s", loc_str.c_str (), errmsg.c_str ());
}

void
rust_internal_error_at (const location_t location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_internal_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_error_at (const location_t location, const std::string &errmsg)
{
  error_at (location, "%s", errmsg.c_str ());
}

void
rust_error_at (const location_t location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

class rust_error_code_rule : public diagnostic_metadata::rule
{
public:
  rust_error_code_rule (const ErrorCode code) : m_code (code) {}

  void format_error_code (char *buffer) const
  {
    // we can use the `u` format specifier because the `ErrorCode` enum class
    // "inherits" from `unsigned int` - add a static assertion to make sure
    // that's the case before we do the formatting
    static_assert (
      std::is_same<std::underlying_type<ErrorCode>::type, unsigned int>::value,
      "invalid format specifier for ErrorCode's underlying type");

    snprintf (buffer, 6, "E%04u",
	      (std::underlying_type<ErrorCode>::type) m_code);
  }

  char *make_description () const final override
  {
    // 'E' + 4 characters + \0
    char *buffer = static_cast<char *> (xcalloc (6, sizeof (char)));

    format_error_code (buffer);

    return buffer;
  }

  char *make_url () const final override
  {
    char buffer[6] = {0};
    format_error_code (buffer);

    return concat ("https://doc.rust-lang.org/error-index.html#", buffer, NULL);
  }

private:
  const ErrorCode m_code;
};

void
rust_be_error_at (const location_t location, const ErrorCode code,
		  const std::string &errmsg)
{
  rich_location gcc_loc (line_table, location);
  diagnostic_metadata m;
  rust_error_code_rule rule (code);
  m.add_rule (rule);
  error_meta (&gcc_loc, m, "%s", errmsg.c_str ());
}

void
rust_error_at (const location_t location, const ErrorCode code, const char *fmt,
	       ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (location, code, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_error_at (const rich_location &location, const ErrorCode code,
		  const std::string &errmsg)
{
  /* TODO: 'error_at' would like a non-'const' 'rich_location *'.  */
  rich_location &gcc_loc = const_cast<rich_location &> (location);
  diagnostic_metadata m;
  rust_error_code_rule rule (code);
  m.add_rule (rule);
  error_meta (&gcc_loc, m, "%s", errmsg.c_str ());
}

void
rust_error_at (const rich_location &location, const ErrorCode code,
	       const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (location, code, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_error_at (rich_location *richloc, const ErrorCode code,
		  const std::string &errmsg)
{
  diagnostic_metadata m;
  rust_error_code_rule rule (code);
  m.add_rule (rule);
  error_meta (richloc, m, "%s", errmsg.c_str ());
}

void
rust_error_at (rich_location *richloc, const ErrorCode code, const char *fmt,
	       ...)
{
  /* TODO: Refactoring diagnostics to this overload */
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (richloc, code, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_warning_at (const location_t location, int opt,
		    const std::string &warningmsg)
{
  warning_at (location, opt, "%s", warningmsg.c_str ());
}

void
rust_warning_at (const location_t location, int opt, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_warning_at (location, opt, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_fatal_error (const location_t location, const std::string &fatalmsg)
{
  fatal_error (location, "%s", fatalmsg.c_str ());
}

void
rust_fatal_error (const location_t location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_fatal_error (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_inform (const location_t location, const std::string &infomsg)
{
  inform (location, "%s", infomsg.c_str ());
}

void
rust_inform (const location_t location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_inform (location, expand_message (fmt, ap));
  va_end (ap);
}

// Rich Locations
void
rust_be_error_at (const rich_location &location, const std::string &errmsg)
{
  /* TODO: 'error_at' would like a non-'const' 'rich_location *'.  */
  rich_location &gcc_loc = const_cast<rich_location &> (location);
  error_at (&gcc_loc, "%s", errmsg.c_str ());
}

void
rust_error_at (const rich_location &location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

void
rust_be_error_at (rich_location *richloc, const std::string &errmsg)
{
  error_at (richloc, "%s", errmsg.c_str ());
}

void
rust_error_at (rich_location *richloc, const char *fmt, ...)
{
  /* TODO: Refactoring diagnostics to this overload */
  va_list ap;

  va_start (ap, fmt);
  rust_be_error_at (richloc, expand_message (fmt, ap));
  va_end (ap);
}

bool
rust_be_debug_p (void)
{
  return !!flag_rust_debug;
}

void
rust_debug_loc (const location_t location, const char *fmt, ...)
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
      rust_be_error_at (UNKNOWN_LOCATION,
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

// simple location
static Error
va_constructor (Error::Kind kind, location_t locus, const char *fmt,
		va_list args) RUST_ATTRIBUTE_GCC_DIAG (3, 0);

// simple location + error code
static Error
va_constructor (Error::Kind kind, location_t locus, const ErrorCode code,
		const char *fmt, va_list args) RUST_ATTRIBUTE_GCC_DIAG (4, 0);

// rich location
static Error
va_constructor (Error::Kind kind, rich_location *r_locus, const char *fmt,
		va_list args) RUST_ATTRIBUTE_GCC_DIAG (3, 0);

// rich location + error code
static Error
va_constructor (Error::Kind kind, rich_location *r_locus, const ErrorCode code,
		const char *fmt, va_list args) RUST_ATTRIBUTE_GCC_DIAG (4, 0);

// simple location
static Error
va_constructor (Error::Kind kind, location_t locus, const char *fmt,
		va_list args)
{
  std::string message = expand_message (fmt, args);
  message.shrink_to_fit ();
  va_end (args);

  return Error (kind, locus, message);
}

// simple location + error code
static Error
va_constructor (Error::Kind kind, location_t locus, const ErrorCode code,
		const char *fmt, va_list args)
{
  std::string message = expand_message (fmt, args);
  message.shrink_to_fit ();
  va_end (args);

  return Error (kind, locus, code, message);
}

// rich location
static Error
va_constructor (Error::Kind kind, rich_location *r_locus, const char *fmt,
		va_list args)
{
  std::string message = expand_message (fmt, args);
  message.shrink_to_fit ();
  va_end (args);

  return Error (kind, r_locus, message);
}

// rich location + error code
static Error
va_constructor (Error::Kind kind, rich_location *r_locus, const ErrorCode code,
		const char *fmt, va_list args)
{
  std::string message = expand_message (fmt, args);
  message.shrink_to_fit ();
  va_end (args);

  return Error (kind, r_locus, code, message);
}

// simple location
Error::Error (const location_t location, const char *fmt, ...)
  : kind (Kind::Err), locus (location)
{
  va_list ap;
  va_start (ap, fmt);

  *this = va_constructor (Kind::Err, location, fmt, ap);
}

// simple location + error code
Error::Error (const location_t location, const ErrorCode code, const char *fmt,
	      ...)
  : kind (Kind::Err), locus (location), errorcode (code)
{
  va_list ap;
  va_start (ap, fmt);

  *this = va_constructor (Kind::Err, location, code, fmt, ap);
}

// rich location
Error::Error (rich_location *r_locus, const char *fmt, ...)
  : kind (Kind::Err), richlocus (r_locus)
{
  va_list ap;
  va_start (ap, fmt);

  *this = va_constructor (Kind::Err, r_locus, fmt, ap);
}

// rich location + error code
Error::Error (rich_location *r_locus, const ErrorCode code, const char *fmt,
	      ...)
  : kind (Kind::Err), richlocus (r_locus), errorcode (code)
{
  va_list ap;
  va_start (ap, fmt);

  *this = va_constructor (Kind::Err, r_locus, code, fmt, ap);
}

Error
Error::Hint (const location_t location, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);

  return va_constructor (Kind::Hint, location, fmt, ap);
}

Error
Error::Fatal (const location_t location, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);

  return va_constructor (Kind::FatalErr, location, fmt, ap);
}

} // namespace Rust
