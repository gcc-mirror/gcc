// go-encode-id.cc -- Go identifier and packagepath encoding/decoding hooks

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "gogo.h"
#include "go-location.h"
#include "go-linemap.h"
#include "go-encode-id.h"
#include "lex.h"

// Return whether the character c is OK to use in the assembler.  We
// only permit ASCII alphanumeric characters, underscore, and dot.

static bool
char_needs_encoding(char c)
{
  switch (c)
    {
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '_': case '.':
      return false;
    default:
      return true;
    }
}

// Return whether the identifier needs to be translated because it
// contains non-ASCII characters.

bool
go_id_needs_encoding(const std::string& str)
{
  for (std::string::const_iterator p = str.begin();
       p != str.end();
       ++p)
    if (char_needs_encoding(*p))
      return true;
  return false;
}

// Pull the next UTF-8 character out of P and store it in *PC.  Return
// the number of bytes read.

static size_t
fetch_utf8_char(const char* p, unsigned int* pc)
{
  unsigned char c = *p;
  if ((c & 0x80) == 0)
    {
      *pc = c;
      return 1;
    }
  size_t len = 0;
  while ((c & 0x80) != 0)
    {
      ++len;
      c <<= 1;
    }
  unsigned int rc = *p & ((1 << (7 - len)) - 1);
  for (size_t i = 1; i < len; i++)
    {
      unsigned int u = p[i];
      rc <<= 6;
      rc |= u & 0x3f;
    }
  *pc = rc;
  return len;
}

// Encode an identifier using assembler-friendly characters. The encoding is
// described in detail near the end of the long comment at the start of
// names.cc. Short version: translate all non-ASCII-alphanumeric characters into
// ..uXXXX or ..UXXXXXXXX, translate ASCII non-alphanumerics into ".zXX".

std::string
go_encode_id(const std::string &id)
{
  if (Lex::is_invalid_identifier(id))
    {
      go_assert(saw_errors());
      return id;
    }

  // The encoding is only unambiguous if the input string does not
  // contain ..z, ..u or ..U.
  go_assert(id.find("..z") == std::string::npos);
  go_assert(id.find("..u") == std::string::npos);
  go_assert(id.find("..U") == std::string::npos);

  std::string ret;
  const char* p = id.c_str();
  const char* pend = p + id.length();

  // A leading ".0" is a space introduced before a mangled type name
  // that starts with a 'u' or 'U', to avoid confusion with the
  // mangling used here.  We don't need a leading ".0", and we don't
  // want symbols that start with '.', so remove it.
  if (p[0] == '.' && p[1] == '0')
    p += 2;

  while (p < pend)
    {
      unsigned int c;
      size_t len = fetch_utf8_char(p, &c);
      if (len == 1 && !char_needs_encoding(c))
	{
	  ret += c;
	}
      else
	{
	  char buf[16];
          if (len == 1)
            snprintf(buf, sizeof buf, "..z%02x", c);
	  else if (c < 0x10000)
	    snprintf(buf, sizeof buf, "..u%04x", c);
	  else
	    snprintf(buf, sizeof buf, "..U%08x", c);

	  // We don't want a symbol to start with '.', so add a prefix
	  // if needed.
	  if (ret.empty())
	    ret += '_';

	  ret += buf;
	}
      p += len;
    }
  return ret;
}

// Convert a hex digit string to a unicode codepoint. No checking
// to insure that the hex digit is meaningful.

static unsigned
hex_digits_to_unicode_codepoint(const char *digits, unsigned ndig)
{
  unsigned result = 0;
  for (unsigned i = 0; i < ndig; ++i) {
    result <<= 4;
    result |= Lex::hex_val(digits[i]);
  }
  return result;
}

// Decode/demangle a mangled string produced by go_encode_id(). Returns
// empty string if demangling process fails in some way.  At the moment
// this routine is unused; there is an equivalent routine in the runtime
// used for demangling symbols appearing in stack traces.

std::string
go_decode_id(const std::string &encoded)
{
  std::string ret;
  const char* p = encoded.c_str();
  const char* pend = p + encoded.length();
  const Location loc = Linemap::predeclared_location();

  // Special case for initial "_", in case it was introduced
  // as a way to prevent encoded symbol starting with ".".
  if (*p == '_' && (strncmp(p+1, "..u", 3) == 0 || strncmp(p+1, "..U", 3) == 0))
    p++;

  while (p < pend)
    {
      if (strncmp(p, "..z", 3) == 0)
        {
          const char* digits = p+3;
          if (strlen(digits) < 2)
            return "";
          unsigned rune = hex_digits_to_unicode_codepoint(digits, 2);
          Lex::append_char(rune, true, &ret, loc);
          p += 5;
        }
      else if (strncmp(p, "..u", 3) == 0)
        {
          const char* digits = p+3;
          if (strlen(digits) < 4)
            return "";
          unsigned rune = hex_digits_to_unicode_codepoint(digits, 4);
          Lex::append_char(rune, true, &ret, loc);
          p += 7;
        }
      else if (strncmp(p, "..U", 3) == 0)
        {
          const char* digits = p+3;
          if (strlen(digits) < 8)
            return "";
          unsigned rune = hex_digits_to_unicode_codepoint(digits, 8);
          Lex::append_char(rune, true, &ret, loc);
          p += 11;
        }
      else
        {
          ret += *p;
          p += 1;
        }
    }

  return ret;
}

std::string
go_selectively_encode_id(const std::string &id)
{
  if (go_id_needs_encoding(id))
    return go_encode_id(id);
  return std::string();
}

// Encode a struct field tag.  This is only used when we need to
// create a type descriptor for an anonymous struct type with field
// tags.  This mangling is applied before go_encode_id.  We skip
// alphanumerics and underscore, replace every other single byte
// character with .xNN, and leave larger UTF-8 characters for
// go_encode_id.

std::string
go_mangle_struct_tag(const std::string& tag)
{
  std::string ret;
  const char* p = tag.c_str();
  const char* pend = p + tag.length();
  while (p < pend)
    {
      unsigned int c;
      size_t len = fetch_utf8_char(p, &c);
      if (len > 1)
	ret.append(p, len);
      else if (!char_needs_encoding(c) && c != '.')
	ret += c;
      else
	{
	  char buf[16];
	  snprintf(buf, sizeof buf, ".x%02x", c);
	  ret += buf;
	}
      p += len;
    }
  return ret;
}

// Encode a package path.

std::string
go_mangle_pkgpath(const std::string& pkgpath)
{
  std::string s = pkgpath;
  for (size_t i = s.find('.');
       i != std::string::npos;
       i = s.find('.', i + 1))
    s.replace(i, 1, ".x2e"); // 0x2e is the ASCII encoding for '.'
  return s;
}
