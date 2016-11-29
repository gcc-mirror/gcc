// go-encode-id.cc -- Go identifier encoding hooks

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-location.h"
#include "go-linemap.h"
#include "go-encode-id.h"

// Return whether the character c is OK to use in the assembler.

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
    case '_': case '.': case '$': case '/':
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

// Encode an identifier using ASCII characters.

std::string
go_encode_id(const std::string &id)
{
  std::string ret;
  const char* p = id.c_str();
  const char* pend = p + id.length();
  while (p < pend)
    {
      unsigned int c;
      size_t len = fetch_utf8_char(p, &c);
      if (len == 1 && !char_needs_encoding(c))
        ret += c;
      else
        {
          ret += "$U";
          char buf[30];
          snprintf(buf, sizeof buf, "%x", c);
          ret += buf;
          ret += "$";
        }
      p += len;
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
