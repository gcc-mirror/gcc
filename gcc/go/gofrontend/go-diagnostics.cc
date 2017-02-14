// go-diagnostics.cc -- Go error/warning diagnostics utilities.

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-diagnostics.h"

static std::string
mformat_value()
{
  return std::string(xstrerror(errno));
}

// Rewrite a format string to expand any extensions not
// supported by sprintf(). See comments in go-diagnostics.h
// for list of supported format specifiers.

static std::string
expand_format(const char* fmt)
{
  std::stringstream ss;
  for (const char* c = fmt; *c; ++c)
    {
      if (*c != '%')
        {
          ss << *c;
          continue;
        }
      c++;
      switch (*c)
        {
          case '\0':
            {
              // malformed format string
              go_unreachable();
            }
          case '%':
            {
              ss << "%";
              break;
            }
          case 'm':
            {
              ss << mformat_value();
              break;
            }
          case '<':
            {
              ss << go_open_quote();
              break;
            }
          case '>':
            {
              ss << go_close_quote();
              break;
            }
          case 'q':
            {
              ss << go_open_quote();
              c++;
              if (*c == 'm')
                {
                  ss << mformat_value();
                }
              else
                {
                  ss << "%" << *c;
                }
              ss << go_close_quote();
              break;
            }
          default:
            {
              ss << "%" << *c;
            }
        }
    }
  return ss.str();
}

// Expand message format specifiers, using a combination of
// expand_format above to handle extensions (ex: %m, %q) and vasprintf()
// to handle regular printf-style formatting. A pragma is being used here to
// suppress this warning:
//
//   warning: function ‘std::__cxx11::string expand_message(const char*, __va_list_tag*)’ might be a candidate for ‘gnu_printf’ format attribute [-Wsuggest-attribute=format]
//
// What appears to be happening here is that the checker is deciding that
// because of the call to vasprintf() (which has attribute gnu_printf), the
// calling function must need to have attribute gnu_printf as well, even
// though there is already an attribute declaration for it.

static std::string
expand_message(const char* fmt, va_list ap) GO_ATTRIBUTE_GCC_DIAG(1,0);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-attribute=format"

static std::string
expand_message(const char* fmt, va_list ap)
{
  char* mbuf = 0;
  std::string expanded_fmt = expand_format(fmt);
  int nwr = vasprintf(&mbuf, expanded_fmt.c_str(), ap);
  if (nwr == -1)
    {
      // memory allocation failed
      go_be_error_at(Linemap::unknown_location(),
                     "memory allocation failed in vasprintf");
      go_assert(0);
    }
  std::string rval = std::string(mbuf);
  free(mbuf);
  return rval;
}

#pragma GCC diagnostic pop

static const char* cached_open_quote = NULL;
static const char* cached_close_quote = NULL;

const char*
go_open_quote()
{
  if (cached_open_quote == NULL)
    go_be_get_quotechars(&cached_open_quote, &cached_close_quote);
  return cached_open_quote;
}

const char*
go_close_quote()
{
  if (cached_close_quote == NULL)
    go_be_get_quotechars(&cached_open_quote, &cached_close_quote);
  return cached_close_quote;
}

void
go_error_at(const Location location, const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  go_be_error_at(location, expand_message(fmt, ap));
  va_end(ap);
}

void
go_warning_at(const Location location, int opt, const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  go_be_warning_at(location, opt, expand_message(fmt, ap));
  va_end(ap);
}

void
go_fatal_error(const Location location, const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  go_be_fatal_error(location, expand_message(fmt, ap));
  va_end(ap);
}

void
go_inform(const Location location, const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  go_be_inform(location, expand_message(fmt, ap));
  va_end(ap);
}
