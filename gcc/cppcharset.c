/* CPP Library - charsets
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

   Broken out of c-lex.c Apr 2003, adding valid C99 UCN ranges.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "cpplib.h"
#include "cpphash.h"

static int ucn_valid_in_identifier PARAMS ((cpp_reader *, cppchar_t));

/* [lex.charset]: The character designated by the universal character
   name \UNNNNNNNN is that character whose character short name in
   ISO/IEC 10646 is NNNNNNNN; the character designated by the
   universal character name \uNNNN is that character whose character
   short name in ISO/IEC 10646 is 0000NNNN.  If the hexadecimal value
   for a universal character name is less than 0x20 or in the range
   0x7F-0x9F (inclusive), or if the universal character name
   designates a character in the basic source character set, then the
   program is ill-formed.

   *PSTR must be preceded by "\u" or "\U"; it is assumed that the
   buffer end is delimited by a non-hex digit.  Returns zero if UCNs
   are not part of the relevant standard, or if the string beginning
   at *PSTR doesn't syntactically match the form 'NNNN' or 'NNNNNNNN'.

   Otherwise the non-zero value of the UCN, whether valid or invalid,
   is returned.  Diagnostics are emitted for invalid values.  PSTR
   is updated to point one beyond the UCN, or to the syntactically
   invalid character.

   IDENTIFIER_POS is 0 when not in an identifier, 1 for the start of
   an identifier, or 2 otherwise.
*/

cppchar_t
_cpp_valid_ucn (pfile, pstr, identifier_pos)
     cpp_reader *pfile;
     const uchar **pstr;
     int identifier_pos;
{
  cppchar_t result, c;
  unsigned int length;
  const uchar *str = *pstr;
  const uchar *base = str - 2;

  /* Only attempt to interpret a UCS for C++ and C99.  */
  if (!CPP_OPTION (pfile, cplusplus) && !CPP_OPTION (pfile, c99))
    return 0;

  /* We don't accept UCNs for an EBCDIC target.  */
  if (CPP_OPTION (pfile, EBCDIC))
    return 0;

  if (str[-1] == 'u')
    length = 4;
  else if (str[-1] == 'U')
    length = 8;
  else
    abort();

  result = 0;
  do
    {
      c = *str;
      if (!ISXDIGIT (c))
	break;
      str++;
      result = (result << 4) + hex_value (c);
    }
  while (--length);

  *pstr = str;
  if (length)
    /* We'll error when we try it out as the start of an identifier.  */
    cpp_error (pfile, DL_ERROR, "incomplete universal character name %.*s",
	       (int) (str - base), base);
  /* The standard permits $, @ and ` to be specified as UCNs.  We use
     hex escapes so that this also works with EBCDIC hosts.  */
  else if ((result < 0xa0
	    && (result != 0x24 && result != 0x40 && result != 0x60))
	   || (result & 0x80000000)
	   || (result >= 0xD800 && result <= 0xDFFF))
    {
      cpp_error (pfile, DL_ERROR, "%.*s is not a valid universal character",
		 (int) (str - base), base);
    }
  else if (identifier_pos)
    {
      int validity = ucn_valid_in_identifier (pfile, result);

      if (validity == 0)
	cpp_error (pfile, DL_ERROR,
		   "universal character %.*s is not valid in an identifier",
		   (int) (str - base), base);
      else if (validity == 2 && identifier_pos == 1)
	cpp_error (pfile, DL_ERROR,
   "universal character %.*s is not valid at the start of an identifier",
		   (int) (str - base), base);
    }

  if (result == 0)
    result = 1;

  return result;
}

/* Returns 1 if C is valid in an identifier, 2 if C is valid except at
   the start of an identifier, and 0 if C is not valid in an
   identifier.  We assume C has already gone through the checks of
   _cpp_valid_ucn.  */
static int
ucn_valid_in_identifier (pfile, c)
     cpp_reader *pfile;
     cppchar_t c;
{
  /* None of the valid chars are outside the Basic Multilingual Plane (the
     low 16 bits).  */
  if (c > 0xffff)
    return 0;

  if (CPP_OPTION (pfile, c99) || !CPP_PEDANTIC (pfile))
    {
      /* Latin.  */
      if (c == 0x0aa || c == 0x00ba || c == 0x207f || c == 0x1e9b)
	return 1;

      /* Greek.  */
      if (c == 0x0386)
	return 1;

      /* Cyrillic.  */
      if (c == 0x040c)
	return 1;

      /* Hebrew.  */
      if ((c >= 0x05b0 && c <= 0x05b9)
	  || (c >= 0x05bb && c <= 0x005bd)
	  || c == 0x05bf
	  || (c >= 0x05c1 && c <= 0x05c2))
	return 1;

      /* Arabic.  */
      if ((c >= 0x06d0 && c <= 0x06dc)
	  || c == 0x06e8
	  || (c >= 0x06ea && c <= 0x06ed))
	return 1;

      /* Devanagari */
      if ((c >= 0x0901 && c <= 0x0903)
	  || (c >= 0x093e && c <= 0x094d)
	  || (c >= 0x0950 && c <= 0x0952)
	  || c == 0x0963)
	return 1;

      /* Bengali */
      if ((c >= 0x0981 && c <= 0x0983)
	  || (c >= 0x09be && c <= 0x09c4)
	  || (c >= 0x09c7 && c <= 0x09c8)
	  || (c >= 0x09cb && c <= 0x09cd)
	  || (c >= 0x09e2 && c <= 0x09e3))
	return 1;

      /* Gurmukhi */
      if (c == 0x0a02
	  || (c >= 0x0a3e && c <= 0x0a42)
	  || (c >= 0x0a47 && c <= 0x0a48)
	  || (c >= 0x0a4b && c <= 0x0a4d)
	  || (c == 0x0a74))
	return 1;
      
      /* Gujarati */
      if ((c >= 0x0a81 && c <= 0x0a83)
	  || (c >= 0x0abd && c <= 0x0ac5)
	  || (c >= 0x0ac7 && c <= 0x0ac9)
	  || (c >= 0x0acb && c <= 0x0acd)
	  || (c == 0x0ad0))
	return 1;

      /* Oriya */
      if ((c >= 0x0b01 && c <= 0x0b03)
	  || (c >= 0x0b3e && c <= 0x0b43)
	  || (c >= 0x0b47 && c <= 0x0b48)
	  || (c >= 0x0b4b && c <= 0x0b4d))
	return 1;

      /* Tamil */
      if ((c >= 0x0b82 && c <= 0x0b83)
	  || (c >= 0x0bbe && c <= 0x0bc2)
	  || (c >= 0x0bc6 && c <= 0x0bc8)
	  || (c >= 0x0bc8 && c <= 0x0bcd))
	return 1;

      /* Telugu */
      if ((c >= 0x0c01 && c <= 0x0c03)
	  || (c >= 0x0c3e && c <= 0x0c44)
	  || (c >= 0x0c46 && c <= 0x0c48)	
	  || (c >= 0x0c4a && c <= 0x0c4d))
	return 1;

      /* Kannada */
      if ((c >= 0x0c82 && c <= 0x0c83)
	  || (c >= 0x0cbe && c <= 0x0cc4)
	  || (c >= 0x0cc6 && c <= 0x0cc8)
	  || (c >= 0x0cca && c <= 0x0ccd)
	  || c == 0x0cde)
	return 1;

      /* Malayalam */
      if ((c >= 0x0d02 && c <= 0x0d03)
	  || (c >= 0x0d3e && c <= 0x0d43)
	  || (c >= 0x0d46 && c <= 0x0d48)
	  || (c >= 0x0d4a && c <= 0x0d4d))
	return 1;

      /* Thai */
      if ((c >= 0x0e01 && c <= 0x0e3a)
	  || (c >= 0x0e40 && c <= 0x0e5b))
	return 1;

      /* Lao */
      if ((c >= 0x0ead && c <= 0x0eae)
	  || (c >= 0x0eb0 && c <= 0x0eb9)
	  || (c >= 0x0ebb && c <= 0x0ebd)
	  || (c >= 0x0ec0 && c <= 0x0ec4)
	  || c == 0x0ec6
	  || (c >= 0x0ec8 && c <= 0x0ecd)
	  || (c >= 0x0edc && c <= 0x0ed))
	return 1;

      /* Tibetan.  */
      if (c == 0x0f00
	  || (c >= 0x0f18 && c <= 0x0f19)
	  || c == 0x0f35
	  || c == 0x0f37
	  || c == 0x0f39
	  || (c >= 0x0f3e && c <= 0x0f47)
	  || (c >= 0x0f49 && c <= 0x0f69)
	  || (c >= 0x0f71 && c <= 0x0f84)
	  || (c >= 0x0f86 && c <= 0x0f8b)
	  || (c >= 0x0f90 && c <= 0x0f95)
	  || c == 0x0f97
	  || (c >= 0x0f99 && c <= 0x0fad)
	  || (c >= 0x0fb1 && c <= 0x0fb7)
	  || c == 0x0fb9)
	return 1;

      /* Katakana */
      if ((c >= 0x30a1 && c <= 0x30f6)
	  || (c >= 0x30fb && c <= 0x30fc))
	return 1;

      /* CJK Unified Ideographs.  */
      if (c >= 0x4e00 && c <= 0x9fa5)
	return 1;

      /* Hangul.  */
      if (c >= 0xac00 && c <= 0xd7a3)
	return 1;

      /* Digits.  */
      if ((c >= 0x0660 && c <= 0x0669)
	  || (c >= 0x06f0 && c <= 0x06f9)
	  || (c >= 0x0966 && c <= 0x096f)
	  || (c >= 0x09e6 && c <= 0x09ef)
	  || (c >= 0x0a66 && c <= 0x0a6f)
	  || (c >= 0x0ae6 && c <= 0x0aef)
	  || (c >= 0x0b66 && c <= 0x0b6f)
	  || (c >= 0x0be7 && c <= 0x0bef)
	  || (c >= 0x0c66 && c <= 0x0c6f)
	  || (c >= 0x0ce6 && c <= 0x0cef)
	  || (c >= 0x0d66 && c <= 0x0d6f)
	  || (c >= 0x0e50 && c <= 0x0e59)
	  || (c >= 0x0ed0 && c <= 0x0ed9)
	  || (c >= 0x0f20 && c <= 0x0f33))
	return 2;

      /* Special characters.  */
      if (c == 0x00b5
	  || c == 0x00b7
	  || (c >= 0x02b0 && c <= 0x02b8)
	  || c == 0x02bb
	  || (c >= 0x02bd && c <= 0x02c1)
	  || (c >= 0x02d0 && c <= 0x02d1)
	  || (c >= 0x02e0 && c <= 0x02e4)
	  || c == 0x037a
	  || c == 0x0559
	  || c == 0x093d
	  || c == 0x0b3d
	  || c == 0x1fbe
	  || (c >= 0x203f && c <= 0x2040)
	  || c == 0x2102
	  || c == 0x2107
	  || (c >= 0x210a && c <= 0x2113)
	  || c == 0x2115
	  || (c >= 0x2118 && c <= 0x211d)
	  || c == 0x2124
	  || c == 0x2126
	  || c == 0x2128
	  || (c >= 0x212a && c <= 0x2131)
	  || (c >= 0x2133 && c <= 0x2138)
	  || (c >= 0x2160 && c <= 0x2182)
	  || (c >= 0x3005 && c <= 0x3007)
	  || (c >= 0x3021 && c <= 0x3029))
	return 1;	  
    }
  
  if (CPP_OPTION (pfile, cplusplus) || !CPP_PEDANTIC (pfile))
    {
      /* Greek.  */
      if (c == 0x0384)
	return 1;

      /* Cyrillic.  */
      if (c == 0x040d)
	return 1;

      /* Hebrew.  */
      if (c >= 0x05f3 && c <= 0x05f4)
	return 1;

      /* Lao.  */
      if ((c >= 0x0ead && c <= 0x0eb0)
	  || (c == 0x0eb2)
	  || (c == 0x0eb3)
	  || (c == 0x0ebd)
	  || (c >= 0x0ec0 && c <= 0x0ec4)
	  || (c == 0x0ec6))
	return 1;

      /* Hiragana */
      if (c == 0x3094
	  || (c >= 0x309d && c <= 0x309e))
	return 1;

      /* Katakana */
      if ((c >= 0x30a1 && c <= 0x30fe))
	return 1;

      /* Hangul */
      if ((c >= 0x1100 && c <= 0x1159)
	  || (c >= 0x1161 && c <= 0x11a2)
	  || (c >= 0x11a8 && c <= 0x11f9))
	return 1;

      /* CJK Unified Ideographs */
      if ((c >= 0xf900 && c <= 0xfa2d)
	  || (c >= 0xfb1f && c <= 0xfb36)
	  || (c >= 0xfb38 && c <= 0xfb3c)
	  || (c == 0xfb3e)
	  || (c >= 0xfb40 && c <= 0xfb41)
	  || (c >= 0xfb42 && c <= 0xfb44)
	  || (c >= 0xfb46 && c <= 0xfbb1)
	  || (c >= 0xfbd3 && c <= 0xfd3f)
	  || (c >= 0xfd50 && c <= 0xfd8f)
	  || (c >= 0xfd92 && c <= 0xfdc7)
	  || (c >= 0xfdf0 && c <= 0xfdfb)
	  || (c >= 0xfe70 && c <= 0xfe72)
	  || (c == 0xfe74)
	  || (c >= 0xfe76 && c <= 0xfefc)
	  || (c >= 0xff21 && c <= 0xff3a)
	  || (c >= 0xff41 && c <= 0xff5a)
	  || (c >= 0xff66 && c <= 0xffbe)
	  || (c >= 0xffc2 && c <= 0xffc7)
	  || (c >= 0xffca && c <= 0xffcf)
	  || (c >= 0xffd2 && c <= 0xffd7)
	  || (c >= 0xffda && c <= 0xffdc)
	  || (c >= 0x4e00 && c <= 0x9fa5))
	return 1;
    }

  /* Latin */
  if ((c >= 0x00c0 && c <= 0x00d6)
      || (c >= 0x00d8 && c <= 0x00f6)
      || (c >= 0x00f8 && c <= 0x01f5)
      || (c >= 0x01fa && c <= 0x0217)
      || (c >= 0x0250 && c <= 0x02a8)
      || (c >= 0x1e00 && c <= 0x1e9a)
      || (c >= 0x1ea0 && c <= 0x1ef9))
    return 1;

  /* Greek */
  if ((c >= 0x0388 && c <= 0x038a)
      || (c == 0x038c)
      || (c >= 0x038e && c <= 0x03a1)
      || (c >= 0x03a3 && c <= 0x03ce)
      || (c >= 0x03d0 && c <= 0x03d6)
      || (c == 0x03da)
      || (c == 0x03dc)
      || (c == 0x03de)
      || (c == 0x03e0)
      || (c >= 0x03e2 && c <= 0x03f3)
      || (c >= 0x1f00 && c <= 0x1f15)
      || (c >= 0x1f18 && c <= 0x1f1d)
      || (c >= 0x1f20 && c <= 0x1f45)
      || (c >= 0x1f48 && c <= 0x1f4d)
      || (c >= 0x1f50 && c <= 0x1f57)
      || (c == 0x1f59)
      || (c == 0x1f5b)
      || (c == 0x1f5d)
      || (c >= 0x1f5f && c <= 0x1f7d)
      || (c >= 0x1f80 && c <= 0x1fb4)
      || (c >= 0x1fb6 && c <= 0x1fbc)
      || (c >= 0x1fc2 && c <= 0x1fc4)
      || (c >= 0x1fc6 && c <= 0x1fcc)
      || (c >= 0x1fd0 && c <= 0x1fd3)
      || (c >= 0x1fd6 && c <= 0x1fdb)
      || (c >= 0x1fe0 && c <= 0x1fec)
      || (c >= 0x1ff2 && c <= 0x1ff4)
      || (c >= 0x1ff6 && c <= 0x1ffc))
    return 1;

  /* Cyrillic */
  if ((c >= 0x0401 && c <= 0x040c)
      || (c >= 0x040f && c <= 0x044f)
      || (c >= 0x0451 && c <= 0x045c)
      || (c >= 0x045e && c <= 0x0481)
      || (c >= 0x0490 && c <= 0x04c4)
      || (c >= 0x04c7 && c <= 0x04c8)
      || (c >= 0x04cb && c <= 0x04cc)
      || (c >= 0x04d0 && c <= 0x04eb)
      || (c >= 0x04ee && c <= 0x04f5)
      || (c >= 0x04f8 && c <= 0x04f9))
    return 1;

  /* Armenian */
  if ((c >= 0x0531 && c <= 0x0556)
      || (c >= 0x0561 && c <= 0x0587))
    return 1;

  /* Hebrew */
  if ((c >= 0x05d0 && c <= 0x05ea)
      || (c >= 0x05f0 && c <= 0x05f2))
    return 1;

  /* Arabic */
  if ((c >= 0x0621 && c <= 0x063a)
      || (c >= 0x0640 && c <= 0x0652)
      || (c >= 0x0670 && c <= 0x06b7)
      || (c >= 0x06ba && c <= 0x06be)
      || (c >= 0x06c0 && c <= 0x06ce)
      || (c >= 0x06e5 && c <= 0x06e7))
    return 1;

  /* Devanagari */
  if ((c >= 0x0905 && c <= 0x0939)
      || (c >= 0x0958 && c <= 0x0962))
    return 1;

  /* Bengali */
  if ((c >= 0x0985 && c <= 0x098c)
      || (c >= 0x098f && c <= 0x0990)
      || (c >= 0x0993 && c <= 0x09a8)
      || (c >= 0x09aa && c <= 0x09b0)
      || (c == 0x09b2)
      || (c >= 0x09b6 && c <= 0x09b9)
      || (c >= 0x09dc && c <= 0x09dd)
      || (c >= 0x09df && c <= 0x09e1)
      || (c >= 0x09f0 && c <= 0x09f1))
    return 1;

  /* Gurmukhi */
  if ((c >= 0x0a05 && c <= 0x0a0a)
      || (c >= 0x0a0f && c <= 0x0a10)
      || (c >= 0x0a13 && c <= 0x0a28)
      || (c >= 0x0a2a && c <= 0x0a30)
      || (c >= 0x0a32 && c <= 0x0a33)
      || (c >= 0x0a35 && c <= 0x0a36)
      || (c >= 0x0a38 && c <= 0x0a39)
      || (c >= 0x0a59 && c <= 0x0a5c)
      || (c == 0x0a5e))
    return 1;

  /* Gujarati */
  if ((c >= 0x0a85 && c <= 0x0a8b)
      || (c == 0x0a8d)
      || (c >= 0x0a8f && c <= 0x0a91)
      || (c >= 0x0a93 && c <= 0x0aa8)
      || (c >= 0x0aaa && c <= 0x0ab0)
      || (c >= 0x0ab2 && c <= 0x0ab3)
      || (c >= 0x0ab5 && c <= 0x0ab9)
      || (c == 0x0ae0))
    return 1;

  /* Oriya */
  if ((c >= 0x0b05 && c <= 0x0b0c)
      || (c >= 0x0b0f && c <= 0x0b10)
      || (c >= 0x0b13 && c <= 0x0b28)
      || (c >= 0x0b2a && c <= 0x0b30)
      || (c >= 0x0b32 && c <= 0x0b33)
      || (c >= 0x0b36 && c <= 0x0b39)
      || (c >= 0x0b5c && c <= 0x0b5d)
      || (c >= 0x0b5f && c <= 0x0b61))
    return 1;

  /* Tamil */
  if ((c >= 0x0b85 && c <= 0x0b8a)
      || (c >= 0x0b8e && c <= 0x0b90)
      || (c >= 0x0b92 && c <= 0x0b95)
      || (c >= 0x0b99 && c <= 0x0b9a)
      || (c == 0x0b9c)
      || (c >= 0x0b9e && c <= 0x0b9f)
      || (c >= 0x0ba3 && c <= 0x0ba4)
      || (c >= 0x0ba8 && c <= 0x0baa)
      || (c >= 0x0bae && c <= 0x0bb5)
      || (c >= 0x0bb7 && c <= 0x0bb9))
    return 1;

  /* Telugu */
  if ((c >= 0x0c05 && c <= 0x0c0c)
      || (c >= 0x0c0e && c <= 0x0c10)
      || (c >= 0x0c12 && c <= 0x0c28)
      || (c >= 0x0c2a && c <= 0x0c33)
      || (c >= 0x0c35 && c <= 0x0c39)
      || (c >= 0x0c60 && c <= 0x0c61))
    return 1;

  /* Kannada */
  if ((c >= 0x0c85 && c <= 0x0c8c)
      || (c >= 0x0c8e && c <= 0x0c90)
      || (c >= 0x0c92 && c <= 0x0ca8)
      || (c >= 0x0caa && c <= 0x0cb3)
      || (c >= 0x0cb5 && c <= 0x0cb9)
      || (c >= 0x0ce0 && c <= 0x0ce1))
    return 1;

  /* Malayalam */
  if ((c >= 0x0d05 && c <= 0x0d0c)
      || (c >= 0x0d0e && c <= 0x0d10)
      || (c >= 0x0d12 && c <= 0x0d28)
      || (c >= 0x0d2a && c <= 0x0d39)
      || (c >= 0x0d60 && c <= 0x0d61))
    return 1;

  /* Thai */
  if ((c >= 0x0e01 && c <= 0x0e30)
      || (c >= 0x0e32 && c <= 0x0e33)
      || (c >= 0x0e40 && c <= 0x0e46)
      || (c >= 0x0e4f && c <= 0x0e5b))
    return 1;

  /* Lao */
  if ((c >= 0x0e81 && c <= 0x0e82)
      || (c == 0x0e84)
      || (c == 0x0e87)
      || (c == 0x0e88)
      || (c == 0x0e8a)
      || (c == 0x0e8d)
      || (c >= 0x0e94 && c <= 0x0e97)
      || (c >= 0x0e99 && c <= 0x0e9f)
      || (c >= 0x0ea1 && c <= 0x0ea3)
      || (c == 0x0ea5)
      || (c == 0x0ea7)
      || (c == 0x0eaa)
      || (c == 0x0eab))
    return 1;

  /* Georgian */
  if ((c >= 0x10a0 && c <= 0x10c5)
      || (c >= 0x10d0 && c <= 0x10f6))
    return 1;

  /* Hiragana */
  if ((c >= 0x3041 && c <= 0x3093)
      || (c >= 0x309b && c <= 0x309c))
    return 1;

  /* Bopmofo */
  if ((c >= 0x3105 && c <= 0x312c))
    return 1;

  return 0;
}
