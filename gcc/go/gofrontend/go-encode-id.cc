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

// Return whether the character c can appear in a name that we are
// encoding.  We only permit ASCII alphanumeric characters.

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

// Map from characters to the underscore encoding for them.

class Special_char_code
{
 public:
  Special_char_code();

  // Return the simple underscore encoding for C, or 0 if none.
  char
  code_for(unsigned int c) const
  {
    if (c <= 127)
      return this->codes_[c];
    return 0;
  }

 private:
  // Encodings for characters.
  char codes_[128];
};

// Construct the underscore encoding map.

Special_char_code::Special_char_code()
{
  memset(this->codes_, 0, sizeof this->codes_);
  this->codes_['_'] = '_';
  this->codes_['.'] = '0';
  this->codes_['/'] = '1';
  this->codes_['*'] = '2';
  this->codes_[','] = '3';
  this->codes_['{'] = '4';
  this->codes_['}'] = '5';
  this->codes_['['] = '6';
  this->codes_[']'] = '7';
  this->codes_['('] = '8';
  this->codes_[')'] = '9';
  this->codes_['"'] = 'a';
  this->codes_[' '] = 'b';
  this->codes_[';'] = 'c';
}

// The singleton Special_char_code.

static const Special_char_code special_char_code;

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

// Encode an identifier using assembler-friendly characters.  The
// encoding is described in detail near the end of the long comment at
// the start of names.cc.

std::string
go_encode_id(const std::string &id)
{
  if (Lex::is_invalid_identifier(id))
    {
      go_assert(saw_errors());
      return id;
    }

  std::string ret;
  const char* p = id.c_str();
  const char* pend = p + id.length();

  // We encode a leading digit, to ensure that no identifier starts
  // with a digit.
  if (pend > p && p[0] >= '0' && p[0] <= '9')
    {
      char buf[8];
      snprintf(buf, sizeof buf, "_x%02x", p[0]);
      ret.append(buf);
      ++p;
    }

  while (p < pend)
    {
      unsigned int c;
      size_t len = fetch_utf8_char(p, &c);
      if (len == 1)
	{
	  if (!char_needs_encoding(c))
	    ret.push_back(c);
	  else
	    {
	      char code = special_char_code.code_for(c);
	      if (code != 0)
		{
		  ret.push_back('_');
		  ret.push_back(code);
		}
	      else
		{
		  char buf[16];
		  snprintf(buf, sizeof buf, "_x%02x", c);
		  ret.append(buf);
		}
	    }
	}
      else
	{
	  char buf[16];
	  if (c < 0x10000)
	    snprintf(buf, sizeof buf, "_u%04x", c);
	  else
	    snprintf(buf, sizeof buf, "_U%08x", c);
	  ret.append(buf);
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

  while (p < pend)
    {
      if (*p != '_' || p + 1 == pend)
	{
	  ret.push_back(*p);
	  p++;
	  continue;
	}

      switch (p[1])
	{
	case '_':
	  ret.push_back('_');
	  p += 2;
	  break;
	case '0':
	  ret.push_back('.');
	  p += 2;
	  break;
	case '1':
	  ret.push_back('/');
	  p += 2;
	  break;
	case '2':
	  ret.push_back('*');
	  p += 2;
	  break;
	case '3':
	  ret.push_back(',');
	  p += 2;
	  break;
	case '4':
	  ret.push_back('{');
	  p += 2;
	  break;
	case '5':
	  ret.push_back('}');
	  p += 2;
	  break;
	case '6':
	  ret.push_back('[');
	  p += 2;
	  break;
	case '7':
	  ret.push_back(']');
	  p += 2;
	  break;
	case '8':
	  ret.push_back('(');
	  p += 2;
	  break;
	case '9':
	  ret.push_back(')');
	  p += 2;
	  break;
	case 'a':
	  ret.push_back('"');
	  p += 2;
	  break;
	case 'b':
	  ret.push_back(' ');
	  p += 2;
	  break;
	case 'c':
	  ret.push_back(';');
	  p += 2;
	  break;
        case 'x':
	  {
	    const char* digits = p + 2;
	    if (strlen(digits) < 2)
	      return "";
	    unsigned int rune = hex_digits_to_unicode_codepoint(digits, 2);
	    Lex::append_char(rune, true, &ret, loc);
	    p += 4;
	  }
	  break;
	case 'u':
	  {
	    const char* digits = p + 2;
	    if (strlen(digits) < 4)
	      return "";
	    unsigned int rune = hex_digits_to_unicode_codepoint(digits, 4);
	    Lex::append_char(rune, true, &ret, loc);
	    p += 6;
	  }
	  break;
	case 'U':
	  {
	    const char* digits = p + 2;
	    if (strlen(digits) < 8)
	      return "";
	    unsigned int rune = hex_digits_to_unicode_codepoint(digits, 8);
	    Lex::append_char(rune, true, &ret, loc);
	    p += 10;
	  }
	  break;
	default:
	  return "";
	}
    }

  return ret;
}

// Encode a struct field tag.  This is only used when we need to
// create a type descriptor for an anonymous struct type with field
// tags.  Underscore encoding will be applied to the returned string.
// The tag will appear between curly braces, so that is all we have to
// avoid.

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
      else if (c != '{' && c != '}' && c != '\\')
	ret.push_back(c);
      else
	{
	  ret.push_back('\\');
	  ret.push_back(c);
	}
      p += len;
    }
  return ret;
}
