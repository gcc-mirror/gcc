/* Table of UCNs which are valid in identifiers.
   Copyright (C) 2003 Free Software Foundation, Inc.

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

/* Automatically generated from cppucnid.tab, do not edit */

/* This file reproduces the table in ISO/IEC 9899:1999 (C99) Annex
   D, which is itself a reproduction from ISO/IEC TR 10176:1998, and
   the similar table from ISO/IEC 14882:1988 (C++98) Annex E, which is
   a reproduction of ISO/IEC PDTR 10176.  Unfortunately these tables
   are not identical.  */

#ifndef CPPUCNID_H
#define CPPUCNID_H

#define C99 1
#define CXX 2
#define DIG 4

struct ucnrange
{
  unsigned short lo, hi;
  unsigned short flags;
};

static const struct ucnrange ucnranges[] = {
  { 0x00aa, 0x00aa,     C99     },  /* Latin */
  { 0x00b5, 0x00b5,     C99     },  /* Special characters */
  { 0x00b7, 0x00b7,     C99     },
  { 0x00ba, 0x00ba,     C99     },  /* Latin */
  { 0x00c0, 0x00d6, CXX|C99     },
  { 0x00d8, 0x00f6, CXX|C99     },
  { 0x00f8, 0x01f5, CXX|C99     },
  { 0x01fa, 0x0217, CXX|C99     },
  { 0x0250, 0x02a8, CXX|C99     },
  { 0x02b0, 0x02b8,     C99     },  /* Special characters */
  { 0x02bb, 0x02bb,     C99     },
  { 0x02bd, 0x02c1,     C99     },
  { 0x02d0, 0x02d1,     C99     },
  { 0x02e0, 0x02e4,     C99     },
  { 0x037a, 0x037a,     C99     },
  { 0x0384, 0x0384, CXX         },  /* Greek */
  { 0x0386, 0x0386,     C99     },
  { 0x0388, 0x038a, CXX|C99     },
  { 0x038c, 0x038c, CXX|C99     },
  { 0x038e, 0x03a1, CXX|C99     },
  { 0x03a3, 0x03ce, CXX|C99     },
  { 0x03d0, 0x03d6, CXX|C99     },
  { 0x03da, 0x03da, CXX|C99     },
  { 0x03dc, 0x03dc, CXX|C99     },
  { 0x03de, 0x03de, CXX|C99     },
  { 0x03e0, 0x03e0, CXX|C99     },
  { 0x03e2, 0x03f3, CXX|C99     },
  { 0x0401, 0x040c, CXX|C99     },  /* Cyrillic */
  { 0x040d, 0x040d, CXX         },
  { 0x040e, 0x040e,     C99     },
  { 0x040f, 0x044f, CXX|C99     },
  { 0x0451, 0x045c, CXX|C99     },
  { 0x045e, 0x0481, CXX|C99     },
  { 0x0490, 0x04c4, CXX|C99     },
  { 0x04c7, 0x04c8, CXX|C99     },
  { 0x04cb, 0x04cc, CXX|C99     },
  { 0x04d0, 0x04eb, CXX|C99     },
  { 0x04ee, 0x04f5, CXX|C99     },
  { 0x04f8, 0x04f9, CXX|C99     },
  { 0x0531, 0x0556, CXX|C99     },  /* Armenian */
  { 0x0559, 0x0559,     C99     },  /* Special characters */
  { 0x0561, 0x0587, CXX|C99     },  /* Armenian */
  { 0x05b0, 0x05b9,     C99     },  /* Hebrew */
  { 0x05bb, 0x05bd,     C99     },
  { 0x05bf, 0x05bf,     C99     },
  { 0x05c1, 0x05c2,     C99     },
  { 0x05d0, 0x05ea, CXX|C99     },
  { 0x05f0, 0x05f2, CXX|C99     },
  { 0x05f3, 0x05f4, CXX         },
  { 0x0621, 0x063a, CXX|C99     },  /* Arabic */
  { 0x0640, 0x0652, CXX|C99     },
  { 0x0660, 0x0669,     C99|DIG },  /* Digits */
  { 0x0670, 0x06b7, CXX|C99     },  /* Arabic */
  { 0x06ba, 0x06be, CXX|C99     },
  { 0x06c0, 0x06ce, CXX|C99     },
  { 0x06d0, 0x06dc,     C99     },
  { 0x06e5, 0x06e7, CXX|C99     },
  { 0x06e8, 0x06e8,     C99     },
  { 0x06ea, 0x06ed,     C99     },
  { 0x06f0, 0x06f9,     C99|DIG },  /* Digits */
  { 0x0901, 0x0903,     C99     },  /* Devanagari */
  { 0x0905, 0x0939, CXX|C99     },
  { 0x093d, 0x093d,     C99     },  /* Special characters */
  { 0x093e, 0x094d,     C99     },  /* Devanagari */
  { 0x0950, 0x0952,     C99     },
  { 0x0958, 0x0962, CXX|C99     },
  { 0x0963, 0x0963,     C99     },
  { 0x0966, 0x096f,     C99|DIG },  /* Digits */
  { 0x0981, 0x0983,     C99     },  /* Bengali */
  { 0x0985, 0x098c, CXX|C99     },
  { 0x098f, 0x0990, CXX|C99     },
  { 0x0993, 0x09a8, CXX|C99     },
  { 0x09aa, 0x09b0, CXX|C99     },
  { 0x09b2, 0x09b2, CXX|C99     },
  { 0x09b6, 0x09b9, CXX|C99     },
  { 0x09be, 0x09c4,     C99     },
  { 0x09c7, 0x09c8,     C99     },
  { 0x09cb, 0x09cd,     C99     },
  { 0x09dc, 0x09dd, CXX|C99     },
  { 0x09df, 0x09e1, CXX|C99     },
  { 0x09e2, 0x09e3,     C99     },
  { 0x09e6, 0x09ef,     C99|DIG },  /* Digits */
  { 0x09f0, 0x09f1, CXX|C99     },  /* Bengali */
  { 0x0a02, 0x0a02,     C99     },  /* Gurmukhi */
  { 0x0a05, 0x0a0a, CXX|C99     },
  { 0x0a0f, 0x0a10, CXX|C99     },
  { 0x0a13, 0x0a28, CXX|C99     },
  { 0x0a2a, 0x0a30, CXX|C99     },
  { 0x0a32, 0x0a33, CXX|C99     },
  { 0x0a35, 0x0a36, CXX|C99     },
  { 0x0a38, 0x0a39, CXX|C99     },
  { 0x0a3e, 0x0a42,     C99     },
  { 0x0a47, 0x0a48,     C99     },
  { 0x0a4b, 0x0a4d,     C99     },
  { 0x0a59, 0x0a5c, CXX|C99     },
  { 0x0a5e, 0x0a5e, CXX|C99     },
  { 0x0a66, 0x0a6f,     C99|DIG },  /* Digits */
  { 0x0a74, 0x0a74,     C99     },  /* Gurmukhi */
  { 0x0a81, 0x0a83,     C99     },  /* Gujarati */
  { 0x0a85, 0x0a8b, CXX|C99     },
  { 0x0a8d, 0x0a8d, CXX|C99     },
  { 0x0a8f, 0x0a91, CXX|C99     },
  { 0x0a93, 0x0aa8, CXX|C99     },
  { 0x0aaa, 0x0ab0, CXX|C99     },
  { 0x0ab2, 0x0ab3, CXX|C99     },
  { 0x0ab5, 0x0ab9, CXX|C99     },
  { 0x0abd, 0x0ac5,     C99     },
  { 0x0ac7, 0x0ac9,     C99     },
  { 0x0acb, 0x0acd,     C99     },
  { 0x0ad0, 0x0ad0,     C99     },
  { 0x0ae0, 0x0ae0, CXX|C99     },
  { 0x0ae6, 0x0aef,     C99|DIG },  /* Digits */
  { 0x0b01, 0x0b03,     C99     },  /* Oriya */
  { 0x0b05, 0x0b0c, CXX|C99     },
  { 0x0b0f, 0x0b10, CXX|C99     },
  { 0x0b13, 0x0b28, CXX|C99     },
  { 0x0b2a, 0x0b30, CXX|C99     },
  { 0x0b32, 0x0b33, CXX|C99     },
  { 0x0b36, 0x0b39, CXX|C99     },
  { 0x0b3d, 0x0b3d,     C99     },  /* Special characters */
  { 0x0b3e, 0x0b43,     C99     },  /* Oriya */
  { 0x0b47, 0x0b48,     C99     },
  { 0x0b4b, 0x0b4d,     C99     },
  { 0x0b5c, 0x0b5d, CXX|C99     },
  { 0x0b5f, 0x0b61, CXX|C99     },
  { 0x0b66, 0x0b6f,     C99|DIG },  /* Digits */
  { 0x0b82, 0x0b83,     C99     },  /* Tamil */
  { 0x0b85, 0x0b8a, CXX|C99     },
  { 0x0b8e, 0x0b90, CXX|C99     },
  { 0x0b92, 0x0b95, CXX|C99     },
  { 0x0b99, 0x0b9a, CXX|C99     },
  { 0x0b9c, 0x0b9c, CXX|C99     },
  { 0x0b9e, 0x0b9f, CXX|C99     },
  { 0x0ba3, 0x0ba4, CXX|C99     },
  { 0x0ba8, 0x0baa, CXX|C99     },
  { 0x0bae, 0x0bb5, CXX|C99     },
  { 0x0bb7, 0x0bb9, CXX|C99     },
  { 0x0bbe, 0x0bc2,     C99     },
  { 0x0bc6, 0x0bc8,     C99     },
  { 0x0bca, 0x0bcd,     C99     },
  { 0x0be7, 0x0bef,     C99|DIG },  /* Digits */
  { 0x0c01, 0x0c03,     C99     },  /* Telugu */
  { 0x0c05, 0x0c0c, CXX|C99     },
  { 0x0c0e, 0x0c10, CXX|C99     },
  { 0x0c12, 0x0c28, CXX|C99     },
  { 0x0c2a, 0x0c33, CXX|C99     },
  { 0x0c35, 0x0c39, CXX|C99     },
  { 0x0c3e, 0x0c44,     C99     },
  { 0x0c46, 0x0c48,     C99     },
  { 0x0c4a, 0x0c4d,     C99     },
  { 0x0c60, 0x0c61, CXX|C99     },
  { 0x0c66, 0x0c6f,     C99|DIG },  /* Digits */
  { 0x0c82, 0x0c83,     C99     },  /* Kannada */
  { 0x0c85, 0x0c8c, CXX|C99     },
  { 0x0c8e, 0x0c90, CXX|C99     },
  { 0x0c92, 0x0ca8, CXX|C99     },
  { 0x0caa, 0x0cb3, CXX|C99     },
  { 0x0cb5, 0x0cb9, CXX|C99     },
  { 0x0cbe, 0x0cc4,     C99     },
  { 0x0cc6, 0x0cc8,     C99     },
  { 0x0cca, 0x0ccd,     C99     },
  { 0x0cde, 0x0cde,     C99     },
  { 0x0ce0, 0x0ce1, CXX|C99     },
  { 0x0ce6, 0x0cef,     C99|DIG },  /* Digits */
  { 0x0d02, 0x0d03,     C99     },  /* Malayalam */
  { 0x0d05, 0x0d0c, CXX|C99     },
  { 0x0d0e, 0x0d10, CXX|C99     },
  { 0x0d12, 0x0d28, CXX|C99     },
  { 0x0d2a, 0x0d39, CXX|C99     },
  { 0x0d3e, 0x0d43,     C99     },
  { 0x0d46, 0x0d48,     C99     },
  { 0x0d4a, 0x0d4d,     C99     },
  { 0x0d60, 0x0d61, CXX|C99     },
  { 0x0d66, 0x0d6f,     C99|DIG },  /* Digits */
  { 0x0e01, 0x0e30, CXX|C99     },  /* Thai */
  { 0x0e31, 0x0e31,     C99     },
  { 0x0e32, 0x0e33, CXX|C99     },
  { 0x0e34, 0x0e3a,     C99     },
  { 0x0e40, 0x0e46, CXX|C99     },
  { 0x0e47, 0x0e49,     C99     },
  { 0x0e50, 0x0e59, CXX|C99|DIG },  /* Digits */
  { 0x0e5a, 0x0e5b, CXX|C99     },  /* Thai */
  { 0x0e81, 0x0e82, CXX|C99     },  /* Lao */
  { 0x0e84, 0x0e84, CXX|C99     },
  { 0x0e87, 0x0e88, CXX|C99     },
  { 0x0e8a, 0x0e8a, CXX|C99     },
  { 0x0e8d, 0x0e8d, CXX|C99     },
  { 0x0e94, 0x0e97, CXX|C99     },
  { 0x0e99, 0x0e9f, CXX|C99     },
  { 0x0ea1, 0x0ea3, CXX|C99     },
  { 0x0ea5, 0x0ea5, CXX|C99     },
  { 0x0ea7, 0x0ea7, CXX|C99     },
  { 0x0eaa, 0x0eab, CXX|C99     },
  { 0x0ead, 0x0eae, CXX|C99     },
  { 0x0eaf, 0x0eaf, CXX         },
  { 0x0eb0, 0x0eb0, CXX|C99     },
  { 0x0eb1, 0x0eb1,     C99     },
  { 0x0eb2, 0x0eb3, CXX|C99     },
  { 0x0eb4, 0x0eb9,     C99     },
  { 0x0ebb, 0x0ebc,     C99     },
  { 0x0ebd, 0x0ebd, CXX|C99     },
  { 0x0ec0, 0x0ec4, CXX|C99     },
  { 0x0ec6, 0x0ec6, CXX|C99     },
  { 0x0ec8, 0x0ecd,     C99     },
  { 0x0ed0, 0x0ed9,     C99|DIG },  /* Digits */
  { 0x0edc, 0x0edd,     C99     },  /* Lao */
  { 0x0f00, 0x0f00,     C99     },  /* Tibetan */
  { 0x0f18, 0x0f19,     C99     },
  { 0x0f20, 0x0f33,     C99|DIG },  /* Digits */
  { 0x0f35, 0x0f35,     C99     },  /* Tibetan */
  { 0x0f37, 0x0f37,     C99     },
  { 0x0f39, 0x0f39,     C99     },
  { 0x0f3e, 0x0f47,     C99     },
  { 0x0f49, 0x0f69,     C99     },
  { 0x0f71, 0x0f84,     C99     },
  { 0x0f86, 0x0f8b,     C99     },
  { 0x0f90, 0x0f95,     C99     },
  { 0x0f97, 0x0f97,     C99     },
  { 0x0f99, 0x0fad,     C99     },
  { 0x0fb1, 0x0fb7,     C99     },
  { 0x0fb9, 0x0fb9,     C99     },
  { 0x10a0, 0x10c5, CXX|C99     },  /* Georgian */
  { 0x10d0, 0x10f6, CXX|C99     },
  { 0x1100, 0x1159, CXX         },  /* Hangul */
  { 0x1161, 0x11a2, CXX         },
  { 0x11a8, 0x11f9, CXX         },
  { 0x1e00, 0x1e9a, CXX|C99     },  /* Latin */
  { 0x1e9b, 0x1e9b,     C99     },
  { 0x1ea0, 0x1ef9, CXX|C99     },
  { 0x1f00, 0x1f15, CXX|C99     },  /* Greek */
  { 0x1f18, 0x1f1d, CXX|C99     },
  { 0x1f20, 0x1f45, CXX|C99     },
  { 0x1f48, 0x1f4d, CXX|C99     },
  { 0x1f50, 0x1f57, CXX|C99     },
  { 0x1f59, 0x1f59, CXX|C99     },
  { 0x1f5b, 0x1f5b, CXX|C99     },
  { 0x1f5d, 0x1f5d, CXX|C99     },
  { 0x1f5f, 0x1f7d, CXX|C99     },
  { 0x1f80, 0x1fb4, CXX|C99     },
  { 0x1fb6, 0x1fbc, CXX|C99     },
  { 0x1fbe, 0x1fbe,     C99     },  /* Special characters */
  { 0x1fc2, 0x1fc4, CXX|C99     },  /* Greek */
  { 0x1fc6, 0x1fcc, CXX|C99     },
  { 0x1fd0, 0x1fd3, CXX|C99     },
  { 0x1fd6, 0x1fdb, CXX|C99     },
  { 0x1fe0, 0x1fec, CXX|C99     },
  { 0x1ff2, 0x1ff4, CXX|C99     },
  { 0x1ff6, 0x1ffc, CXX|C99     },
  { 0x203f, 0x2040,     C99     },  /* Special characters */
  { 0x207f, 0x207f,     C99     },  /* Latin */
  { 0x2102, 0x2102,     C99     },  /* Special characters */
  { 0x2107, 0x2107,     C99     },
  { 0x210a, 0x2113,     C99     },
  { 0x2115, 0x2115,     C99     },
  { 0x2118, 0x211d,     C99     },
  { 0x2124, 0x2124,     C99     },
  { 0x2126, 0x2126,     C99     },
  { 0x2128, 0x2128,     C99     },
  { 0x212a, 0x2131,     C99     },
  { 0x2133, 0x2138,     C99     },
  { 0x2160, 0x2182,     C99     },
  { 0x3005, 0x3007,     C99     },
  { 0x3021, 0x3029,     C99     },
  { 0x3041, 0x3093, CXX|C99     },  /* Hiragana */
  { 0x3094, 0x3094, CXX         },
  { 0x309b, 0x309c, CXX|C99     },
  { 0x309d, 0x309e, CXX         },
  { 0x30a1, 0x30f6, CXX|C99     },  /* Katakana */
  { 0x30f7, 0x30fa, CXX         },
  { 0x30fb, 0x30fc, CXX|C99     },
  { 0x30fd, 0x30fe, CXX         },
  { 0x3105, 0x312c, CXX|C99     },  /* Bopomofo */
  { 0x4e00, 0x9fa5, CXX|C99     },  /* CJK Unified Ideographs */
  { 0xac00, 0xd7a3,     C99     },  /* Hangul */
  { 0xf900, 0xfa2d, CXX         },  /* CJK Unified Ideographs */
  { 0xfb1f, 0xfb36, CXX         },
  { 0xfb38, 0xfb3c, CXX         },
  { 0xfb3e, 0xfb3e, CXX         },
  { 0xfb40, 0xfb44, CXX         },
  { 0xfb46, 0xfbb1, CXX         },
  { 0xfbd3, 0xfd3f, CXX         },
  { 0xfd50, 0xfd8f, CXX         },
  { 0xfd92, 0xfdc7, CXX         },
  { 0xfdf0, 0xfdfb, CXX         },
  { 0xfe70, 0xfe72, CXX         },
  { 0xfe74, 0xfe74, CXX         },
  { 0xfe76, 0xfefc, CXX         },
  { 0xff21, 0xff3a, CXX         },
  { 0xff41, 0xff5a, CXX         },
  { 0xff66, 0xffbe, CXX         },
  { 0xffc2, 0xffc7, CXX         },
  { 0xffca, 0xffcf, CXX         },
  { 0xffd2, 0xffd7, CXX         },
  { 0xffda, 0xffdc, CXX         },
};

#endif /* cppucnid.h */
