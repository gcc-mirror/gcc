/* Structure layout test generator.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Compile with gcc -o struct-layout-1_generate{,.c} generate_random{,_r}.c */

/* N.B. -- This program cannot use libiberty as that will not work
   when testing an installed compiler.  */
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
/* We use our own pseudo-random number generator, so that it gives the same
   values on all hosts.  */
#include "generate-random.h"

#if LLONG_MAX != 9223372036854775807LL && __LONG_LONG_MAX__ != 9223372036854775807LL
# error Need 64-bit long long
#endif

typedef unsigned int hashval_t;

enum TYPE
{
  TYPE_INT,
  TYPE_UINT,
  TYPE_CINT,
  TYPE_CUINT,
  TYPE_FLOAT,
  TYPE_CFLOAT,
  TYPE_SENUM,
  TYPE_UENUM,
  TYPE_PTR,
  TYPE_FNPTR,
  TYPE_OTHER
};

struct types
{
  const char *name;
  enum TYPE type;
  unsigned long long int maxval;
  char bitfld;
};

struct types base_types[] = {
/* As we don't know whether char will be signed or not, just limit ourselves
   to unsigned values less than maximum signed char value.  */
{ "char", TYPE_UINT, 127, 'C' },
{ "signed char", TYPE_INT, 127, 'C' },
{ "unsigned char", TYPE_UINT, 255, 'C' },
{ "short int", TYPE_INT, 32767, 'S' },
{ "unsigned short int", TYPE_UINT, 65535, 'S' },
{ "int", TYPE_INT, 2147483647, 'I' },
{ "unsigned int", TYPE_UINT, 4294967295U, 'I' },
{ "long int", TYPE_INT, 9223372036854775807LL, 'L' },
{ "unsigned long int", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "long long int", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "unsigned long long int", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "bool", TYPE_UINT, 1, 'B' },
{ "void *", TYPE_PTR, 0, 0 },
{ "char *", TYPE_PTR, 0, 0 },
{ "int *", TYPE_PTR, 0, 0 },
{ "float", TYPE_FLOAT, 0, 0 },
{ "double", TYPE_FLOAT, 0, 0 },
{ "long double", TYPE_FLOAT, 0, 0 },
#define NTYPES1 18
{ "Tchar", TYPE_UINT, 127, 'C' },
{ "Tschar", TYPE_INT, 127, 'C' },
{ "Tuchar", TYPE_UINT, 255, 'C' },
{ "Tshort", TYPE_INT, 32767, 'S' },
{ "Tushort", TYPE_UINT, 65535, 'S' },
{ "Tint", TYPE_INT, 2147483647, 'I' },
{ "Tuint", TYPE_UINT, 4294967295U, 'I' },
{ "Tlong", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Tulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Tllong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Tullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Tbool", TYPE_UINT, 1, 'B' },
{ "size_t", TYPE_UINT, 18446744073709551615ULL, 0 },
{ "Tptr", TYPE_PTR, 0, 0 },
{ "Tcptr", TYPE_PTR, 0, 0 },
{ "Tiptr", TYPE_PTR, 0, 0 },
{ "Tfnptr", TYPE_FNPTR, 0, 0 },
{ "Tfloat", TYPE_FLOAT, 0, 0 },
{ "Tdouble", TYPE_FLOAT, 0, 0 },
{ "Tldouble", TYPE_FLOAT, 0, 0 },
{ "enum E0", TYPE_UENUM, 0, ' ' },
{ "enum E1", TYPE_UENUM, 1, ' ' },
{ "enum E2", TYPE_SENUM, 3, ' ' },
{ "enum E3", TYPE_SENUM, 127, ' ' },
{ "enum E4", TYPE_UENUM, 255, ' ' },
{ "enum E5", TYPE_SENUM, 32767, ' ' },
{ "enum E6", TYPE_UENUM, 65535, ' ' },
{ "enum E7", TYPE_SENUM, 2147483647, ' ' },
{ "enum E8", TYPE_UENUM, 4294967295U, ' ' },
{ "enum E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "TE0", TYPE_UENUM, 0, ' ' },
{ "TE1", TYPE_UENUM, 1, ' ' },
{ "TE2", TYPE_SENUM, 3, ' ' },
{ "TE3", TYPE_SENUM, 127, ' ' },
{ "TE4", TYPE_UENUM, 255, ' ' },
{ "TE5", TYPE_SENUM, 32767, ' ' },
{ "TE6", TYPE_UENUM, 65535, ' ' },
{ "TE7", TYPE_SENUM, 2147483647, ' ' },
{ "TE8", TYPE_UENUM, 4294967295U, ' ' },
{ "TE9", TYPE_SENUM, 1099511627775LL, ' ' },
/* vector-defs.h typedefs */
{ "qi", TYPE_INT, 127, 0 },
{ "hi", TYPE_INT, 32767, 0 },
{ "si", TYPE_INT, 2147483647, 0 },
{ "di", TYPE_INT, 9223372036854775807LL, 0 },
{ "sf", TYPE_FLOAT, 0, 0 },
{ "df", TYPE_FLOAT, 0, 0 }
#define NTYPES2 (sizeof (base_types) / sizeof (base_types[0]))
};
struct types complex_types[] = {
{ "_Complex char", TYPE_CUINT, 127, 0 },
{ "_Complex signed char", TYPE_CINT, 127, 0 },
{ "_Complex unsigned char", TYPE_CUINT, 255, 0 },
{ "_Complex short int", TYPE_CINT, 32767, 0 },
{ "_Complex unsigned short int", TYPE_CUINT, 65535, 0 },
{ "_Complex int", TYPE_CINT, 2147483647, 0 },
{ "_Complex unsigned int", TYPE_CUINT, 4294967295U, 0 },
{ "_Complex long int", TYPE_CINT, 9223372036854775807LL, 0 },
{ "_Complex unsigned long int", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "_Complex long long int", TYPE_CINT, 9223372036854775807LL, 0 },
{ "_Complex unsigned long long int", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "_Complex float", TYPE_CFLOAT, 0, 0 },
{ "_Complex double", TYPE_CFLOAT, 0, 0 },
{ "_Complex long double", TYPE_CFLOAT, 0, 0 },
{ "Tcchar", TYPE_CUINT, 127, 0 },
{ "Tcschar", TYPE_CINT, 127, 0 },
{ "Tcuchar", TYPE_CUINT, 255, 0 },
{ "Tcshort", TYPE_CINT, 32767, 0 },
{ "Tcushort", TYPE_CUINT, 65535, 0 },
{ "Tcint", TYPE_CINT, 2147483647, 0 },
{ "Tcuint", TYPE_CUINT, 4294967295U, 0 },
{ "Tclong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tculong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tcllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tcullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tcfloat", TYPE_CFLOAT, 0, 0 },
{ "Tcdouble", TYPE_CFLOAT, 0, 0 },
{ "Tcldouble", TYPE_CFLOAT, 0, 0 }
#define NCTYPES2 (sizeof (complex_types) / sizeof (complex_types[0]))
};
struct types vector_types[] = {
/* vector-defs.h typedefs */
{ "v8qi", TYPE_OTHER, 0, 0 },
{ "v16qi", TYPE_OTHER, 0, 0 },
{ "v2hi", TYPE_OTHER, 0, 0 },
{ "v4hi", TYPE_OTHER, 0, 0 },
{ "v8hi", TYPE_OTHER, 0, 0 },
{ "v2si", TYPE_OTHER, 0, 0 },
{ "v4si", TYPE_OTHER, 0, 0 },
{ "v1di", TYPE_OTHER, 0, 0 },
{ "v2di", TYPE_OTHER, 0, 0 },
{ "v2sf", TYPE_OTHER, 0, 0 },
{ "v4sf", TYPE_OTHER, 0, 0 },
{ "v16sf", TYPE_OTHER, 0, 0 },
{ "v2df", TYPE_OTHER, 0, 0 },
{ "u8qi", TYPE_OTHER, 0, 0 },
{ "u16qi", TYPE_OTHER, 0, 0 },
{ "u2hi", TYPE_OTHER, 0, 0 },
{ "u4hi", TYPE_OTHER, 0, 0 },
{ "u8hi", TYPE_OTHER, 0, 0 },
{ "u2si", TYPE_OTHER, 0, 0 },
{ "u4si", TYPE_OTHER, 0, 0 },
{ "u1di", TYPE_OTHER, 0, 0 },
{ "u2di", TYPE_OTHER, 0, 0 },
{ "u2sf", TYPE_OTHER, 0, 0 },
{ "u4sf", TYPE_OTHER, 0, 0 },
{ "u16sf", TYPE_OTHER, 0, 0 },
{ "u2df", TYPE_OTHER, 0, 0 },
{ "__m64", TYPE_OTHER, 0, 0 },
{ "__m128", TYPE_OTHER, 0, 0 }
#define NVTYPES2 (sizeof (vector_types) / sizeof (vector_types[0]))
};
struct types attrib_types[] = {
{ "Talchar", TYPE_UINT, 127, 'C' },
{ "Talschar", TYPE_INT, 127, 'C' },
{ "Taluchar", TYPE_UINT, 255, 'C' },
{ "Talshort", TYPE_INT, 32767, 'S' },
{ "Talushort", TYPE_UINT, 65535, 'S' },
{ "Talint", TYPE_INT, 2147483647, 'I' },
{ "Taluint", TYPE_UINT, 4294967295U, 'I' },
{ "Tallong", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Talulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Talllong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Talullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Talbool", TYPE_UINT, 1, 'B' },
{ "Talptr", TYPE_PTR, 0, 0 },
{ "Talcptr", TYPE_PTR, 0, 0 },
{ "Taliptr", TYPE_PTR, 0, 0 },
{ "Talfloat", TYPE_FLOAT, 0, 0 },
{ "Taldouble", TYPE_FLOAT, 0, 0 },
{ "Talldouble", TYPE_FLOAT, 0, 0 },
{ "TalE0", TYPE_UENUM, 0, ' ' },
{ "TalE1", TYPE_UENUM, 1, ' ' },
{ "TalE2", TYPE_SENUM, 3, ' ' },
{ "TalE3", TYPE_SENUM, 127, ' ' },
{ "TalE4", TYPE_UENUM, 255, ' ' },
{ "TalE5", TYPE_SENUM, 32767, ' ' },
{ "TalE6", TYPE_UENUM, 65535, ' ' },
{ "TalE7", TYPE_SENUM, 2147483647, ' ' },
{ "TalE8", TYPE_UENUM, 4294967295U, ' ' },
{ "TalE9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Tal1char", TYPE_UINT, 127, 'C' },
{ "Tal1schar", TYPE_INT, 127, 'C' },
{ "Tal1uchar", TYPE_UINT, 255, 'C' },
{ "Tal1short", TYPE_INT, 32767, 'S' },
{ "Tal1ushort", TYPE_UINT, 65535, 'S' },
{ "Tal1int", TYPE_INT, 2147483647, 'I' },
{ "Tal1uint", TYPE_UINT, 4294967295U, 'I' },
{ "Tal1long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Tal1ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Tal1llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Tal1ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Tal1bool", TYPE_UINT, 1, 'B' },
{ "Tal1ptr", TYPE_PTR, 0, 0 },
{ "Tal1cptr", TYPE_PTR, 0, 0 },
{ "Tal1iptr", TYPE_PTR, 0, 0 },
{ "Tal1float", TYPE_FLOAT, 0, 0 },
{ "Tal1double", TYPE_FLOAT, 0, 0 },
{ "Tal1ldouble", TYPE_FLOAT, 0, 0 },
{ "Tal1E0", TYPE_UENUM, 0, ' ' },
{ "Tal1E1", TYPE_UENUM, 1, ' ' },
{ "Tal1E2", TYPE_SENUM, 3, ' ' },
{ "Tal1E3", TYPE_SENUM, 127, ' ' },
{ "Tal1E4", TYPE_UENUM, 255, ' ' },
{ "Tal1E5", TYPE_SENUM, 32767, ' ' },
{ "Tal1E6", TYPE_UENUM, 65535, ' ' },
{ "Tal1E7", TYPE_SENUM, 2147483647, ' ' },
{ "Tal1E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Tal1E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Tal2char", TYPE_UINT, 127, 'C' },
{ "Tal2schar", TYPE_INT, 127, 'C' },
{ "Tal2uchar", TYPE_UINT, 255, 'C' },
{ "Tal2short", TYPE_INT, 32767, 'S' },
{ "Tal2ushort", TYPE_UINT, 65535, 'S' },
{ "Tal2int", TYPE_INT, 2147483647, 'I' },
{ "Tal2uint", TYPE_UINT, 4294967295U, 'I' },
{ "Tal2long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Tal2ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Tal2llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Tal2ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Tal2bool", TYPE_UINT, 1, 'B' },
{ "Tal2ptr", TYPE_PTR, 0, 0 },
{ "Tal2cptr", TYPE_PTR, 0, 0 },
{ "Tal2iptr", TYPE_PTR, 0, 0 },
{ "Tal2float", TYPE_FLOAT, 0, 0 },
{ "Tal2double", TYPE_FLOAT, 0, 0 },
{ "Tal2ldouble", TYPE_FLOAT, 0, 0 },
{ "Tal2E0", TYPE_UENUM, 0, ' ' },
{ "Tal2E1", TYPE_UENUM, 1, ' ' },
{ "Tal2E2", TYPE_SENUM, 3, ' ' },
{ "Tal2E3", TYPE_SENUM, 127, ' ' },
{ "Tal2E4", TYPE_UENUM, 255, ' ' },
{ "Tal2E5", TYPE_SENUM, 32767, ' ' },
{ "Tal2E6", TYPE_UENUM, 65535, ' ' },
{ "Tal2E7", TYPE_SENUM, 2147483647, ' ' },
{ "Tal2E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Tal2E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Tal4char", TYPE_UINT, 127, 'C' },
{ "Tal4schar", TYPE_INT, 127, 'C' },
{ "Tal4uchar", TYPE_UINT, 255, 'C' },
{ "Tal4short", TYPE_INT, 32767, 'S' },
{ "Tal4ushort", TYPE_UINT, 65535, 'S' },
{ "Tal4int", TYPE_INT, 2147483647, 'I' },
{ "Tal4uint", TYPE_UINT, 4294967295U, 'I' },
{ "Tal4long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Tal4ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Tal4llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Tal4ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Tal4bool", TYPE_UINT, 1, 'B' },
{ "Tal4ptr", TYPE_PTR, 0, 0 },
{ "Tal4cptr", TYPE_PTR, 0, 0 },
{ "Tal4iptr", TYPE_PTR, 0, 0 },
{ "Tal4float", TYPE_FLOAT, 0, 0 },
{ "Tal4double", TYPE_FLOAT, 0, 0 },
{ "Tal4ldouble", TYPE_FLOAT, 0, 0 },
{ "Tal4E0", TYPE_UENUM, 0, ' ' },
{ "Tal4E1", TYPE_UENUM, 1, ' ' },
{ "Tal4E2", TYPE_SENUM, 3, ' ' },
{ "Tal4E3", TYPE_SENUM, 127, ' ' },
{ "Tal4E4", TYPE_UENUM, 255, ' ' },
{ "Tal4E5", TYPE_SENUM, 32767, ' ' },
{ "Tal4E6", TYPE_UENUM, 65535, ' ' },
{ "Tal4E7", TYPE_SENUM, 2147483647, ' ' },
{ "Tal4E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Tal4E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Tal8char", TYPE_UINT, 127, 'C' },
{ "Tal8schar", TYPE_INT, 127, 'C' },
{ "Tal8uchar", TYPE_UINT, 255, 'C' },
{ "Tal8short", TYPE_INT, 32767, 'S' },
{ "Tal8ushort", TYPE_UINT, 65535, 'S' },
{ "Tal8int", TYPE_INT, 2147483647, 'I' },
{ "Tal8uint", TYPE_UINT, 4294967295U, 'I' },
{ "Tal8long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Tal8ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Tal8llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Tal8ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Tal8bool", TYPE_UINT, 1, 'B' },
{ "Tal8ptr", TYPE_PTR, 0, 0 },
{ "Tal8cptr", TYPE_PTR, 0, 0 },
{ "Tal8iptr", TYPE_PTR, 0, 0 },
{ "Tal8float", TYPE_FLOAT, 0, 0 },
{ "Tal8double", TYPE_FLOAT, 0, 0 },
{ "Tal8ldouble", TYPE_FLOAT, 0, 0 },
{ "Tal8E0", TYPE_UENUM, 0, ' ' },
{ "Tal8E1", TYPE_UENUM, 1, ' ' },
{ "Tal8E2", TYPE_SENUM, 3, ' ' },
{ "Tal8E3", TYPE_SENUM, 127, ' ' },
{ "Tal8E4", TYPE_UENUM, 255, ' ' },
{ "Tal8E5", TYPE_SENUM, 32767, ' ' },
{ "Tal8E6", TYPE_UENUM, 65535, ' ' },
{ "Tal8E7", TYPE_SENUM, 2147483647, ' ' },
{ "Tal8E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Tal8E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Tal16char", TYPE_UINT, 127, 'C' },
{ "Tal16schar", TYPE_INT, 127, 'C' },
{ "Tal16uchar", TYPE_UINT, 255, 'C' },
{ "Tal16short", TYPE_INT, 32767, 'S' },
{ "Tal16ushort", TYPE_UINT, 65535, 'S' },
{ "Tal16int", TYPE_INT, 2147483647, 'I' },
{ "Tal16uint", TYPE_UINT, 4294967295U, 'I' },
{ "Tal16long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Tal16ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Tal16llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Tal16ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Tal16bool", TYPE_UINT, 1, 'B' },
{ "Tal16ptr", TYPE_PTR, 0, 0 },
{ "Tal16cptr", TYPE_PTR, 0, 0 },
{ "Tal16iptr", TYPE_PTR, 0, 0 },
{ "Tal16float", TYPE_FLOAT, 0, 0 },
{ "Tal16double", TYPE_FLOAT, 0, 0 },
{ "Tal16ldouble", TYPE_FLOAT, 0, 0 },
{ "Tal16E0", TYPE_UENUM, 0, ' ' },
{ "Tal16E1", TYPE_UENUM, 1, ' ' },
{ "Tal16E2", TYPE_SENUM, 3, ' ' },
{ "Tal16E3", TYPE_SENUM, 127, ' ' },
{ "Tal16E4", TYPE_UENUM, 255, ' ' },
{ "Tal16E5", TYPE_SENUM, 32767, ' ' },
{ "Tal16E6", TYPE_UENUM, 65535, ' ' },
{ "Tal16E7", TYPE_SENUM, 2147483647, ' ' },
{ "Tal16E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Tal16E9", TYPE_SENUM, 1099511627775LL, ' ' }
#define NATYPES2 (sizeof (attrib_types) / sizeof (attrib_types[0]))
};
struct types complex_attrib_types[] = {
{ "Talcchar", TYPE_CUINT, 127, 0 },
{ "Talcschar", TYPE_CINT, 127, 0 },
{ "Talcuchar", TYPE_CUINT, 255, 0 },
{ "Talcshort", TYPE_CINT, 32767, 0 },
{ "Talcushort", TYPE_CUINT, 65535, 0 },
{ "Talcint", TYPE_CINT, 2147483647, 0 },
{ "Talcuint", TYPE_CUINT, 4294967295U, 0 },
{ "Talclong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talculong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talcllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talcullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talcfloat", TYPE_CFLOAT, 0, 0 },
{ "Talcdouble", TYPE_CFLOAT, 0, 0 },
{ "Talcldouble", TYPE_CFLOAT, 0, 0 },
{ "Tal1cchar", TYPE_CUINT, 127, 0 },
{ "Tal1cschar", TYPE_CINT, 127, 0 },
{ "Tal1cuchar", TYPE_CUINT, 255, 0 },
{ "Tal1cshort", TYPE_CINT, 32767, 0 },
{ "Tal1cushort", TYPE_CUINT, 65535, 0 },
{ "Tal1cint", TYPE_CINT, 2147483647, 0 },
{ "Tal1cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Tal1clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal1culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal1cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal1cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal1cfloat", TYPE_CFLOAT, 0, 0 },
{ "Tal1cdouble", TYPE_CFLOAT, 0, 0 },
{ "Tal1cldouble", TYPE_CFLOAT, 0, 0 },
{ "Tal2cchar", TYPE_CUINT, 127, 0 },
{ "Tal2cschar", TYPE_CINT, 127, 0 },
{ "Tal2cuchar", TYPE_CUINT, 255, 0 },
{ "Tal2cshort", TYPE_CINT, 32767, 0 },
{ "Tal2cushort", TYPE_CUINT, 65535, 0 },
{ "Tal2cint", TYPE_CINT, 2147483647, 0 },
{ "Tal2cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Tal2clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal2culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal2cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal2cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal2cfloat", TYPE_CFLOAT, 0, 0 },
{ "Tal2cdouble", TYPE_CFLOAT, 0, 0 },
{ "Tal2cldouble", TYPE_CFLOAT, 0, 0 },
{ "Tal4cchar", TYPE_CUINT, 127, 0 },
{ "Tal4cschar", TYPE_CINT, 127, 0 },
{ "Tal4cuchar", TYPE_CUINT, 255, 0 },
{ "Tal4cshort", TYPE_CINT, 32767, 0 },
{ "Tal4cushort", TYPE_CUINT, 65535, 0 },
{ "Tal4cint", TYPE_CINT, 2147483647, 0 },
{ "Tal4cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Tal4clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal4culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal4cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal4cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal4cfloat", TYPE_CFLOAT, 0, 0 },
{ "Tal4cdouble", TYPE_CFLOAT, 0, 0 },
{ "Tal4cldouble", TYPE_CFLOAT, 0, 0 },
{ "Tal8cchar", TYPE_CUINT, 127, 0 },
{ "Tal8cschar", TYPE_CINT, 127, 0 },
{ "Tal8cuchar", TYPE_CUINT, 255, 0 },
{ "Tal8cshort", TYPE_CINT, 32767, 0 },
{ "Tal8cushort", TYPE_CUINT, 65535, 0 },
{ "Tal8cint", TYPE_CINT, 2147483647, 0 },
{ "Tal8cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Tal8clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal8culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal8cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal8cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal8cfloat", TYPE_CFLOAT, 0, 0 },
{ "Tal8cdouble", TYPE_CFLOAT, 0, 0 },
{ "Tal8cldouble", TYPE_CFLOAT, 0, 0 },
{ "Tal16cchar", TYPE_CUINT, 127, 0 },
{ "Tal16cschar", TYPE_CINT, 127, 0 },
{ "Tal16cuchar", TYPE_CUINT, 255, 0 },
{ "Tal16cshort", TYPE_CINT, 32767, 0 },
{ "Tal16cushort", TYPE_CUINT, 65535, 0 },
{ "Tal16cint", TYPE_CINT, 2147483647, 0 },
{ "Tal16cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Tal16clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal16culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal16cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Tal16cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Tal16cfloat", TYPE_CFLOAT, 0, 0 },
{ "Tal16cdouble", TYPE_CFLOAT, 0, 0 },
{ "Tal16cldouble", TYPE_CFLOAT, 0, 0 }
#define NCATYPES2 (sizeof (complex_attrib_types) / sizeof (complex_attrib_types[0]))
};
struct types attrib_array_types[] = {
{ "Talx1char", TYPE_UINT, 127, 'C' },
{ "Talx1schar", TYPE_INT, 127, 'C' },
{ "Talx1uchar", TYPE_UINT, 255, 'C' },
{ "Talx1short", TYPE_INT, 32767, 'S' },
{ "Talx1ushort", TYPE_UINT, 65535, 'S' },
{ "Talx1int", TYPE_INT, 2147483647, 'I' },
{ "Talx1uint", TYPE_UINT, 4294967295U, 'I' },
{ "Talx1long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Talx1ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Talx1llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Talx1ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Talx1bool", TYPE_UINT, 1, 'B' },
{ "Talx1ptr", TYPE_PTR, 0, 0 },
{ "Talx1cptr", TYPE_PTR, 0, 0 },
{ "Talx1iptr", TYPE_PTR, 0, 0 },
{ "Talx1float", TYPE_FLOAT, 0, 0 },
{ "Talx1double", TYPE_FLOAT, 0, 0 },
{ "Talx1ldouble", TYPE_FLOAT, 0, 0 },
{ "Talx1E0", TYPE_UENUM, 0, ' ' },
{ "Talx1E1", TYPE_UENUM, 1, ' ' },
{ "Talx1E2", TYPE_SENUM, 3, ' ' },
{ "Talx1E3", TYPE_SENUM, 127, ' ' },
{ "Talx1E4", TYPE_UENUM, 255, ' ' },
{ "Talx1E5", TYPE_SENUM, 32767, ' ' },
{ "Talx1E6", TYPE_UENUM, 65535, ' ' },
{ "Talx1E7", TYPE_SENUM, 2147483647, ' ' },
{ "Talx1E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Talx1E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Talx2short", TYPE_INT, 32767, 'S' },
{ "Talx2ushort", TYPE_UINT, 65535, 'S' },
{ "Talx2int", TYPE_INT, 2147483647, 'I' },
{ "Talx2uint", TYPE_UINT, 4294967295U, 'I' },
{ "Talx2long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Talx2ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Talx2llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Talx2ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Talx2ptr", TYPE_PTR, 0, 0 },
{ "Talx2cptr", TYPE_PTR, 0, 0 },
{ "Talx2iptr", TYPE_PTR, 0, 0 },
{ "Talx2float", TYPE_FLOAT, 0, 0 },
{ "Talx2double", TYPE_FLOAT, 0, 0 },
{ "Talx2ldouble", TYPE_FLOAT, 0, 0 },
{ "Talx2E0", TYPE_UENUM, 0, ' ' },
{ "Talx2E1", TYPE_UENUM, 1, ' ' },
{ "Talx2E2", TYPE_SENUM, 3, ' ' },
{ "Talx2E3", TYPE_SENUM, 127, ' ' },
{ "Talx2E4", TYPE_UENUM, 255, ' ' },
{ "Talx2E5", TYPE_SENUM, 32767, ' ' },
{ "Talx2E6", TYPE_UENUM, 65535, ' ' },
{ "Talx2E7", TYPE_SENUM, 2147483647, ' ' },
{ "Talx2E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Talx2E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Talx4int", TYPE_INT, 2147483647, 'I' },
{ "Talx4uint", TYPE_UINT, 4294967295U, 'I' },
{ "Talx4long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Talx4ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Talx4llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Talx4ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Talx4ptr", TYPE_PTR, 0, 0 },
{ "Talx4cptr", TYPE_PTR, 0, 0 },
{ "Talx4iptr", TYPE_PTR, 0, 0 },
{ "Talx4float", TYPE_FLOAT, 0, 0 },
{ "Talx4double", TYPE_FLOAT, 0, 0 },
{ "Talx4ldouble", TYPE_FLOAT, 0, 0 },
{ "Talx4E0", TYPE_UENUM, 0, ' ' },
{ "Talx4E1", TYPE_UENUM, 1, ' ' },
{ "Talx4E2", TYPE_SENUM, 3, ' ' },
{ "Talx4E3", TYPE_SENUM, 127, ' ' },
{ "Talx4E4", TYPE_UENUM, 255, ' ' },
{ "Talx4E5", TYPE_SENUM, 32767, ' ' },
{ "Talx4E6", TYPE_UENUM, 65535, ' ' },
{ "Talx4E7", TYPE_SENUM, 2147483647, ' ' },
{ "Talx4E8", TYPE_UENUM, 4294967295U, ' ' },
{ "Talx4E9", TYPE_SENUM, 1099511627775LL, ' ' },
{ "Taly8long", TYPE_INT, 9223372036854775807LL, 'L' },
{ "Taly8ulong", TYPE_UINT, 18446744073709551615ULL, 'L' },
{ "Talx8llong", TYPE_INT, 9223372036854775807LL, 'Q' },
{ "Talx8ullong", TYPE_UINT, 18446744073709551615ULL, 'Q' },
{ "Taly8ptr", TYPE_PTR, 0, 0 },
{ "Taly8cptr", TYPE_PTR, 0, 0 },
{ "Taly8iptr", TYPE_PTR, 0, 0 },
{ "Talx8double", TYPE_FLOAT, 0, 0 },
{ "Talx8ldouble", TYPE_FLOAT, 0, 0 }
#define NAATYPES2 (sizeof (attrib_array_types) / sizeof (attrib_array_types[0]))
};
struct types complex_attrib_array_types[] = {
{ "Talx1cchar", TYPE_CUINT, 127, 0 },
{ "Talx1cschar", TYPE_CINT, 127, 0 },
{ "Talx1cuchar", TYPE_CUINT, 255, 0 },
{ "Talx1cshort", TYPE_CINT, 32767, 0 },
{ "Talx1cushort", TYPE_CUINT, 65535, 0 },
{ "Talx1cint", TYPE_CINT, 2147483647, 0 },
{ "Talx1cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Talx1clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx1culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx1cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx1cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx1cfloat", TYPE_CFLOAT, 0, 0 },
{ "Talx1cdouble", TYPE_CFLOAT, 0, 0 },
{ "Talx1cldouble", TYPE_CFLOAT, 0, 0 },
{ "Talx2cchar", TYPE_CUINT, 127, 0 },
{ "Talx2cschar", TYPE_CINT, 127, 0 },
{ "Talx2cuchar", TYPE_CUINT, 255, 0 },
{ "Talx2cshort", TYPE_CINT, 32767, 0 },
{ "Talx2cushort", TYPE_CUINT, 65535, 0 },
{ "Talx2cint", TYPE_CINT, 2147483647, 0 },
{ "Talx2cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Talx2clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx2culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx2cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx2cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx2cfloat", TYPE_CFLOAT, 0, 0 },
{ "Talx2cdouble", TYPE_CFLOAT, 0, 0 },
{ "Talx2cldouble", TYPE_CFLOAT, 0, 0 },
{ "Talx4cshort", TYPE_CINT, 32767, 0 },
{ "Talx4cushort", TYPE_CUINT, 65535, 0 },
{ "Talx4cint", TYPE_CINT, 2147483647, 0 },
{ "Talx4cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Talx4clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx4culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx4cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx4cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx4cfloat", TYPE_CFLOAT, 0, 0 },
{ "Talx4cdouble", TYPE_CFLOAT, 0, 0 },
{ "Talx4cldouble", TYPE_CFLOAT, 0, 0 },
{ "Talx8cint", TYPE_CINT, 2147483647, 0 },
{ "Talx8cuint", TYPE_CUINT, 4294967295U, 0 },
{ "Talx8clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx8culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx8cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx8cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx8cfloat", TYPE_CFLOAT, 0, 0 },
{ "Talx8cdouble", TYPE_CFLOAT, 0, 0 },
{ "Talx8cldouble", TYPE_CFLOAT, 0, 0 },
{ "Taly16clong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Taly16culong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx16cllong", TYPE_CINT, 9223372036854775807LL, 0 },
{ "Talx16cullong", TYPE_CUINT, 18446744073709551615ULL, 0 },
{ "Talx16cdouble", TYPE_CFLOAT, 0, 0 },
{ "Talx16cldouble", TYPE_CFLOAT, 0, 0 }
#define NCAATYPES2 (sizeof (complex_attrib_array_types) / sizeof (complex_attrib_array_types[0]))
};

struct types bitfld_types[NTYPES2];
int n_bitfld_types;
struct types aligned_bitfld_types[NATYPES2];
int n_aligned_bitfld_types;

const char *attributes[] = {
"atal", 
"atpa", 
"atal1", 
"atal2", 
"atal4", 
"atal8", 
"atal16", 
#define NATTRIBS1 7
"atalpa", 
"atpaal", 
"atal1pa", 
"atal2pa", 
"atal4pa", 
"atal8pa", 
"atal16pa", 
"atpaal1", 
"atpaal2", 
"atpaal4", 
"atpaal8", 
"atpaal16"
#define NATTRIBS2 (sizeof (attributes) / sizeof (attributes[0]))
};

enum ETYPE
{
  ETYPE_TYPE,
  ETYPE_ARRAY,
  ETYPE_BITFLD,
  ETYPE_STRUCT,
  ETYPE_UNION,
  ETYPE_STRUCT_ARRAY,
  ETYPE_UNION_ARRAY
};

struct entry
{
#ifdef __GNUC__
  enum ETYPE etype : 8;
#else
  unsigned char etype;
#endif
  unsigned short len;
  unsigned char arr_len;
  struct types *type;
  const char *attrib;
  /* Used to chain together entries in the hash table.  */
  struct entry *next;
};

/* A prime number giving the number of slots in the hash table.  */ 
#define HASH_SIZE 32749 
static struct entry *hash_table[HASH_SIZE];

static int idx, limidx, output_one;
static const char *destdir;
static const char *srcdir;
FILE *outfile;

void
switchfiles (int fields)
{
  static int filecnt;
  static char *destbuf, *destptr;
  ++filecnt;
  if (outfile)
    fclose (outfile);
  if (output_one)
    {
      outfile = stdout;
      return;
    }
  if (destbuf == NULL)
    {
      size_t len = strlen (destdir);
      destbuf = malloc (len + 20);
      if (!destbuf)
	abort ();
      memcpy (destbuf, destdir, len);
      if (!len || destbuf[len - 1] != '/')
	destbuf[len++] = '/';
      destptr = destbuf + len;
    }
  sprintf (destptr, "t%03d_main.c", filecnt);
  outfile = fopen (destbuf, "w");
  if (outfile == NULL)
    {
    fail:
      fputs ("failed to create test files\n", stderr);
      exit (1);
    }
  fprintf (outfile, "\
/* { dg-options \"-I%s\" } */\n\
/* { dg-options \"-I%s -fno-common\" { target hppa*-*-hpux* } } */\n\
#include \"struct-layout-1.h\"\n\
\n\
#define TX(n, type, attrs, fields, ops) extern void test##n (void);\n\
#include \"t%03d_test.h\"\n\
#undef TX\n\
\n\
int main (void)\n\
{\n\
#define TX(n, type, attrs, fields, ops)   test##n ();\n\
#include \"t%03d_test.h\"\n\
#undef TX\n\
  if (fails)\n\
    {\n\
      fflush (stdout);\n\
      abort ();\n\
    }\n\
  exit (0);\n\
}\n", srcdir, srcdir, filecnt, filecnt);
  fclose (outfile);
  sprintf (destptr, "t%03d_x.c", filecnt);
  outfile = fopen (destbuf, "w");
  if (outfile == NULL)
    goto fail;
  fprintf (outfile, "\
/* { dg-options \"-w -I%s\" } */\n\
/* { dg-options \"-w -I%s -fno-common\" { target hppa*-*-hpux* } } */\n\
#include \"struct-layout-1_x1.h\"\n\
#include \"t%03d_test.h\"\n\
#include \"struct-layout-1_x2.h\"\n\
#include \"t%03d_test.h\"\n", srcdir, srcdir, filecnt, filecnt);
  fclose (outfile);
  sprintf (destptr, "t%03d_y.c", filecnt);
  outfile = fopen (destbuf, "w");
  if (outfile == NULL)
    goto fail;
  fprintf (outfile, "\
/* { dg-options \"-w -I%s\" } */\n\
/* { dg-options \"-w -I%s -fno-common\" { target hppa*-*-hpux* } } */\n\
#include \"struct-layout-1_y1.h\"\n\
#include \"t%03d_test.h\"\n\
#include \"struct-layout-1_y2.h\"\n\
#include \"t%03d_test.h\"\n", srcdir, srcdir, filecnt, filecnt);
  fclose (outfile);
  sprintf (destptr, "t%03d_test.h", filecnt);
  outfile = fopen (destbuf, "w");
  if (outfile == NULL)
    goto fail;
  if (fields <= 2)
    limidx = idx + 300;
  else if (fields <= 4)
    limidx = idx + 200;
  else if (fields <= 6)
    limidx = idx + 100;
  else
    limidx = idx + 50;
}

unsigned long long int
getrandll (void)
{
  unsigned long long int ret;
  ret = generate_random () & 0xffffff;
  ret |= (generate_random () & 0xffffffLL) << 24;
  ret |= ((unsigned long long int) generate_random ()) << 48;
  return ret;
}

int
subfield (struct entry *e, char *letter)
{
  int i, type;
  char buf[20];
  const char *p;
  switch (e[0].etype)
    {
    case ETYPE_STRUCT:
    case ETYPE_UNION:
    case ETYPE_STRUCT_ARRAY:
    case ETYPE_UNION_ARRAY:
      type = e[0].attrib ? 1 + (generate_random () & 3) : 0;
      if (e[0].etype == ETYPE_STRUCT || e[0].etype == ETYPE_STRUCT_ARRAY)
	p = "struct";
      else
	p = "union";
      if (e[0].etype == ETYPE_STRUCT_ARRAY || e[0].etype == ETYPE_UNION_ARRAY)
	{
	  if (e[0].arr_len == 255)
	    snprintf (buf, 20, "%c[]", *letter);
	  else
	    snprintf (buf, 20, "%c[%d]", *letter, e[0].arr_len);
	}
      else
        {
          buf[0] = *letter;
          buf[1] = '\0';
        }
      ++*letter;
      switch (type)
        {
        case 0:
        case 3:
        case 4:
          fprintf (outfile, "%s{", p);
          break;
        case 1:
          fprintf (outfile, "%s %s{", e[0].attrib, p);
          break;
        case 2:
          fprintf (outfile, "%s %s{", p, e[0].attrib);
          break;
        }

      for (i = 1; i <= e[0].len; )
	i += subfield (e + i, letter);

      switch (type)
        {
        case 0:
        case 1:
        case 2:
          fprintf (outfile, "}%s;", buf);
          break;
	case 3:
	  fprintf (outfile, "}%s %s;", e[0].attrib, buf);
	  break;
	case 4:
	  fprintf (outfile, "}%s %s;", buf, e[0].attrib);
	  break;
        }
      return 1 + e[0].len;
    case ETYPE_TYPE:
    case ETYPE_ARRAY:
      if (e[0].etype == ETYPE_ARRAY)
	{
	  if (e[0].arr_len == 255)
	    snprintf (buf, 20, "%c[]", *letter);
	  else
	    snprintf (buf, 20, "%c[%d]", *letter, e[0].arr_len);
	}
      else
        {
          buf[0] = *letter;
          buf[1] = '\0';
        }
      ++*letter;
      if (e[0].attrib)
	switch (generate_random () % 3)
          {
          case 0:
            fprintf (outfile, "%s %s %s;", e[0].attrib, e[0].type->name, buf);
            break;
          case 1:
            fprintf (outfile, "%s %s %s;", e[0].type->name, e[0].attrib, buf);
            break;
          case 2:
            fprintf (outfile, "%s %s %s;", e[0].type->name, buf, e[0].attrib);
            break;
          }
      else
	fprintf (outfile, "%s %s;", e[0].type->name, buf);
      return 1;
    case ETYPE_BITFLD:
      if (e[0].len == 0)
	{
	  if (e[0].attrib)
	    switch (generate_random () % 3)
	      {
	      case 0:
		fprintf (outfile, "%s %s:0;", e[0].attrib, e[0].type->name);
		break;
	      case 1:
		fprintf (outfile, "%s %s:0;", e[0].type->name, e[0].attrib);
		break;
	      case 2:
		fprintf (outfile, "%s:0 %s;", e[0].type->name, e[0].attrib);
		break;
	      }
	  else
	    fprintf (outfile, "%s:0;", e[0].type->name);
	  ++*letter;
	  return 1;
	}
      switch (e[0].type->bitfld)
	{
	case 'C':
	case 'S':
	case 'I':
	case 'L':
	case 'Q':
	  snprintf (buf, 20, "B%cN(%d)", e[0].type->bitfld, e[0].len);
	  break;
	case 'B':
	case ' ':
	  snprintf (buf, 20, "%d", e[0].len);
	  break;
	default:
	  abort ();
	}
      if (e[0].attrib)
	switch (generate_random () % 3)
	  {
	  case 0:
	    fprintf (outfile, "%s %s %c:%s;", e[0].attrib, e[0].type->name,
		     *letter, buf);
	    break;
	  case 1:
	    fprintf (outfile, "%s %s %c:%s;", e[0].type->name, e[0].attrib,
		     *letter, buf);
	    break;
	  case 2:
	    fprintf (outfile, "%s %c:%s %s;", e[0].type->name, *letter,
		     buf, e[0].attrib);
	    break;
	  }
      else
	fprintf (outfile, "%s %c:%s;", e[0].type->name, *letter, buf);
      ++*letter;
      return 1;
    default:
      abort ();
  }
}

char namebuf[1024];

void
output_FNB (char mode, struct entry *e)
{
  unsigned long long int l1, l2, m;
  int signs = 0;
  const char *p, *q;

  if (e->type->type == TYPE_OTHER)
    {
      if (mode == 'B')
        abort ();
      fprintf (outfile, "N(%d,%s)", idx, namebuf);
      return;
    }
  fprintf (outfile, "%c(%d,%s,", mode, idx, namebuf);
  l1 = getrandll ();
  l2 = getrandll ();
  switch (e->type->type)
    {
    case TYPE_INT:
      signs = generate_random () & 3;
      m = e->type->maxval;
      if (mode == 'B')
	m &= e->len > 1 ? (1ULL << (e->len - 1)) - 1 : 1;
      l1 &= m;
      l2 &= m;
      fprintf (outfile, "%s%llu%s,%s%llu%s",
	       (signs & 1) ? "-" : "", l1, l1 > 2147483647 ? "LL" : "",
	       (signs & 2) ? "-" : "", l2, l2 > 2147483647 ? "LL" : "");
      break;
    case TYPE_UINT:
      m = e->type->maxval;
      if (mode == 'B')
	m &= (1ULL << e->len) - 1;
      l1 &= m;
      l2 &= m;
      fprintf (outfile, "%lluU%s,%lluU%s", l1, l1 > 4294967295U ? "LL" : "",
	       l2, l2 > 4294967295U ? "LL" : "");
      break;
    case TYPE_FLOAT:
      l1 &= 0xffffff;
      l2 &= 0xffffff;
      signs = generate_random () & 3;
      fprintf (outfile, "%s%f,%s%f", (signs & 1) ? "-" : "",
	       ((double) l1) / 64, (signs & 2) ? "-" : "", ((double) l2) / 64);
      break;
    case TYPE_CINT:
      signs = generate_random () & 3;
      l1 &= e->type->maxval;
      l2 &= e->type->maxval;
      fprintf (outfile, "CINT(%s%llu%s,%s%llu%s),",
	       (signs & 1) ? "-" : "", l1, l1 > 2147483647 ? "LL" : "",
	       (signs & 2) ? "-" : "", l2, l2 > 2147483647 ? "LL" : "");
      signs = generate_random () & 3;
      l1 = getrandll ();
      l2 = getrandll ();
      l1 &= e->type->maxval;
      l2 &= e->type->maxval;
      fprintf (outfile, "CINT(%s%llu%s,%s%llu%s)",
	       (signs & 1) ? "-" : "", l1, l1 > 2147483647 ? "LL" : "",
	       (signs & 2) ? "-" : "", l2, l2 > 2147483647 ? "LL" : "");
      break;
    case TYPE_CUINT:
      l1 &= e->type->maxval;
      l2 &= e->type->maxval;
      fprintf (outfile, "CINT(%lluU%s,%lluU%s),",
	       l1, l1 > 4294967295U ? "LL" : "",
	       l2, l2 > 4294967295U ? "LL" : "");
      l1 = getrandll ();
      l2 = getrandll ();
      l1 &= e->type->maxval;
      l2 &= e->type->maxval;
      fprintf (outfile, "CINT(%lluU%s,%lluU%s)",
	       l1, l1 > 4294967295U ? "LL" : "",
	       l2, l2 > 4294967295U ? "LL" : "");
      break;
    case TYPE_CFLOAT:
      l1 &= 0xffffff;
      l2 &= 0xffffff;
      signs = generate_random () & 3;
      fprintf (outfile, "CDBL(%s%f,%s%f),",
	       (signs & 1) ? "-" : "", ((double) l1) / 64,
	       (signs & 2) ? "-" : "", ((double) l2) / 64);
      l1 = getrandll ();
      l2 = getrandll ();
      l1 &= 0xffffff;
      l2 &= 0xffffff;
      signs = generate_random () & 3;
      fprintf (outfile, "CDBL(%s%f,%s%f)",
	       (signs & 1) ? "-" : "", ((double) l1) / 64,
	       (signs & 2) ? "-" : "", ((double) l2) / 64);
      break;
    case TYPE_UENUM:
      if (e->type->maxval == 0)
	fputs ("e0_0,e0_0", outfile);
      else if (e->type->maxval == 1)
        fprintf (outfile, "e1_%lld,e1_%lld", l1 & 1, l2 & 1);
      else
        {
	  p = strchr (e->type->name, '\0');
	  while (--p >= e->type->name && *p >= '0' && *p <= '9');
	  p++;
          l1 %= 7;
          l2 %= 7;
          if (l1 > 3)
            l1 += e->type->maxval - 6;
          if (l2 > 3)
            l2 += e->type->maxval - 6;
	  fprintf (outfile, "e%s_%lld,e%s_%lld", p, l1, p, l2);
        }
      break;
    case TYPE_SENUM:
      p = strchr (e->type->name, '\0');
      while (--p >= e->type->name && *p >= '0' && *p <= '9');
      p++;
      l1 %= 7;
      l2 %= 7;
      fprintf (outfile, "e%s_%s%lld,e%s_%s%lld",
	       p, l1 < 3 ? "m" : "",
	       l1 == 3 ? 0LL : e->type->maxval - (l1 & 3),
	       p, l2 < 3 ? "m" : "",
	       l2 == 3 ? 0LL : e->type->maxval - (l2 & 3));
      break;
    case TYPE_PTR:
      l1 %= 256;
      l2 %= 256;
      fprintf (outfile, "(%s)&intarray[%lld],(%s)&intarray[%lld]",
	       e->type->name, l1, e->type->name, l2);
      break;
    case TYPE_FNPTR:
      l1 %= 10;
      l2 %= 10;
      fprintf (outfile, "fn%lld,fn%lld", l1, l2);
      break;
    default:
      abort ();
    }
  fputs (")", outfile);
}

int
subvalues (struct entry *e, char *p, char *letter)
{
  int i, j;
  char *q;
  if (p >= namebuf + sizeof (namebuf) - 32)
    abort ();
  p[0] = *letter;
  p[1] = '\0';
  q = p + 1;
  switch (e[0].etype)
    {
    case ETYPE_STRUCT_ARRAY:
    case ETYPE_UNION_ARRAY:
      if (e[0].arr_len == 0 || e[0].arr_len == 255)
	{
	  *letter += 1 + e[0].len;
	  return 1 + e[0].len;
	}
      i = generate_random () % e[0].arr_len;
      snprintf (p, sizeof (namebuf) - (p - namebuf) - 1,
		"%c[%d]", *letter, i);
      q = strchr (p, '\0');
      /* FALLTHROUGH */
    case ETYPE_STRUCT:
    case ETYPE_UNION:
      *q++ = '.';
      ++*letter;
      for (i = 1; i <= e[0].len; )
	{
	  i += subvalues (e + i, q, letter);
	  if (e[0].etype == ETYPE_UNION || e[0].etype == ETYPE_UNION_ARRAY)
	    {
	      *letter += e[0].len - i + 1;
	      break;
	    }
	}
      return 1 + e[0].len;
    case ETYPE_TYPE:
      ++*letter;
      output_FNB ('F', e);
      return 1;
    case ETYPE_ARRAY:
      if (e[0].arr_len == 0 || e[0].arr_len == 255)
	{
	  ++*letter;
	  return 1;
	}
      i = generate_random () % e[0].arr_len;
      snprintf (p, sizeof (namebuf) - (p - namebuf),
		"%c[%d]", *letter, i);
      output_FNB ('F', e);
      if ((generate_random () & 7) == 0)
	{
	  j = generate_random () % e[0].arr_len;
	  if (i != j)
	    {
	      snprintf (p, sizeof (namebuf) - (p - namebuf),
			"%c[%d]", *letter, j);
	      output_FNB ('F', e);
	    }
	}
      ++*letter;
      return 1;
    case ETYPE_BITFLD:
      ++*letter;
      if (e[0].len != 0)
	output_FNB ('B', e);
      return 1;
    }
}

/* DERIVED FROM:
--------------------------------------------------------------------
lookup2.c, by Bob Jenkins, December 1996, Public Domain.
hash(), hash2(), hash3, and mix() are externally useful functions.
Routines to test the hash are included if SELF_TEST is defined.
You can use this free for any purpose.  It has no warranty.
--------------------------------------------------------------------
*/

/*
--------------------------------------------------------------------
mix -- mix 3 32-bit values reversibly.
For every delta with one or two bit set, and the deltas of all three
  high bits or all three low bits, whether the original value of a,b,c
  is almost all zero or is uniformly distributed,
* If mix() is run forward or backward, at least 32 bits in a,b,c
  have at least 1/4 probability of changing.
* If mix() is run forward, every bit of c will change between 1/3 and
  2/3 of the time.  (Well, 22/100 and 78/100 for some 2-bit deltas.)
mix() was built out of 36 single-cycle latency instructions in a 
  structure that could supported 2x parallelism, like so:
      a -= b; 
      a -= c; x = (c>>13);
      b -= c; a ^= x;
      b -= a; x = (a<<8);
      c -= a; b ^= x;
      c -= b; x = (b>>13);
      ...
  Unfortunately, superscalar Pentiums and Sparcs can't take advantage 
  of that parallelism.  They've also turned some of those single-cycle
  latency instructions into multi-cycle latency instructions.  Still,
  this is the fastest good hash I could find.  There were about 2^^68
  to choose from.  I only looked at a billion or so.
--------------------------------------------------------------------
*/
/* same, but slower, works on systems that might have 8 byte hashval_t's */
#define mix(a,b,c) \
{ \
  a -= b; a -= c; a ^= (c>>13); \
  b -= c; b -= a; b ^= (a<< 8); \
  c -= a; c -= b; c ^= ((b&0xffffffff)>>13); \
  a -= b; a -= c; a ^= ((c&0xffffffff)>>12); \
  b -= c; b -= a; b = (b ^ (a<<16)) & 0xffffffff; \
  c -= a; c -= b; c = (c ^ (b>> 5)) & 0xffffffff; \
  a -= b; a -= c; a = (a ^ (c>> 3)) & 0xffffffff; \
  b -= c; b -= a; b = (b ^ (a<<10)) & 0xffffffff; \
  c -= a; c -= b; c = (c ^ (b>>15)) & 0xffffffff; \
}

/*
--------------------------------------------------------------------
hash() -- hash a variable-length key into a 32-bit value
  k     : the key (the unaligned variable-length array of bytes)
  len   : the length of the key, counting by bytes
  level : can be any 4-byte value
Returns a 32-bit value.  Every bit of the key affects every bit of
the return value.  Every 1-bit and 2-bit delta achieves avalanche.
About 36+6len instructions.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 32 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (ub1 **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = hash( k[i], len[i], h);

By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial.  It's free.

See http://burtleburtle.net/bob/hash/evahash.html
Use for hash table lookup, or anything where one collision in 2^32 is
acceptable.  Do NOT use for cryptographic purposes.
--------------------------------------------------------------------
*/

static hashval_t
iterative_hash (const void *k_in /* the key */,
                register size_t  length /* the length of the key */,
                register hashval_t initval /* the previous hash, or
                                              an arbitrary value */)
{
  register const unsigned char *k = (const unsigned char *)k_in;
  register hashval_t a,b,c,len;

  /* Set up the internal state */
  len = length;
  a = b = 0x9e3779b9;  /* the golden ratio; an arbitrary value */
  c = initval;           /* the previous hash value */

  /*---------------------------------------- handle most of the key */
    while (len >= 12)
      {
	a += (k[0] +((hashval_t)k[1]<<8) +((hashval_t)k[2]<<16) +((hashval_t)k[3]<<24));
	b += (k[4] +((hashval_t)k[5]<<8) +((hashval_t)k[6]<<16) +((hashval_t)k[7]<<24));
	c += (k[8] +((hashval_t)k[9]<<8) +((hashval_t)k[10]<<16)+((hashval_t)k[11]<<24));
	mix(a,b,c);
	k += 12; len -= 12;
      }

  /*------------------------------------- handle the last 11 bytes */
  c += length;
  switch(len)              /* all the case statements fall through */
    {
    case 11: c+=((hashval_t)k[10]<<24);
    case 10: c+=((hashval_t)k[9]<<16);
    case 9 : c+=((hashval_t)k[8]<<8);
      /* the first byte of c is reserved for the length */
    case 8 : b+=((hashval_t)k[7]<<24);
    case 7 : b+=((hashval_t)k[6]<<16);
    case 6 : b+=((hashval_t)k[5]<<8);
    case 5 : b+=k[4];
    case 4 : a+=((hashval_t)k[3]<<24);
    case 3 : a+=((hashval_t)k[2]<<16);
    case 2 : a+=((hashval_t)k[1]<<8);
    case 1 : a+=k[0];
      /* case 0: nothing left to add */
    }
  mix(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}

hashval_t
e_hash (const void *a)
{
  const struct entry *e = a;
  hashval_t ret = 0;
  int i;

  if (e[0].etype != ETYPE_STRUCT && e[0].etype != ETYPE_UNION)
    abort ();
  for (i = 0; i <= e[0].len; ++i)
    {
      int attriblen;
      ret = iterative_hash (&e[i], offsetof (struct entry, attrib), ret);
      attriblen = e[i].attrib ? strlen (e[i].attrib) : -1;
      ret = iterative_hash (&attriblen, sizeof (int), ret);
      if (e[i].attrib)
        ret = iterative_hash (e[i].attrib, attriblen, ret);
    }
  return ret;
}

int
e_eq (const void *a, const void *b)
{
  const struct entry *ea = a, *eb = b;
  int i;
  if (ea[0].etype != ETYPE_STRUCT && ea[0].etype != ETYPE_UNION)
    abort ();
  if (ea[0].len != eb[0].len)
    return 0;
  for (i = 0; i <= ea[0].len; ++i)
    {
      if (ea[i].etype != eb[i].etype
	  || ea[i].len != eb[i].len
	  || ea[i].arr_len != eb[i].arr_len
	  || ea[i].type != eb[i].type)
	return 0;
      if ((ea[i].attrib == NULL) ^ (eb[i].attrib == NULL))
	return 0;
      if (ea[i].attrib && strcmp (ea[i].attrib, eb[i].attrib) != 0)
	return 0;
    }
  return 1;
}

static int 
e_exists (const struct entry *e) 
{
  struct entry *h;
  hashval_t hval;

  hval = e_hash (e);
  for (h = hash_table[hval % HASH_SIZE]; h; h = h->next)
    if (e_eq (e, h))
      return 1;
  return 0;
}

static void
e_insert (struct entry *e)
{
  hashval_t hval;

  hval = e_hash (e);
  e->next = hash_table[hval % HASH_SIZE];
  hash_table[hval % HASH_SIZE] = e;
}

void
output (struct entry *e)
{
  int i;
  char c;
  struct entry *n;
  const char *skip_cint = "";

  if (e[0].etype != ETYPE_STRUCT && e[0].etype != ETYPE_UNION)
    abort ();

  if (e_exists (e))
    return;

  n = (struct entry *) malloc ((e[0].len + 1) * sizeof (struct entry));
  memcpy (n, e, (e[0].len + 1) * sizeof (struct entry));
  e_insert (n);

  if (idx == limidx)
    switchfiles (e[0].len);

  for (i = 1; i <= e[0].len; ++i)
    if ((e[i].etype == ETYPE_TYPE || e[i].etype == ETYPE_ARRAY)
	&& (e[i].type->type == TYPE_CINT || e[i].type->type == TYPE_CUINT))
      break;
  if (i <= e[0].len)
    skip_cint = "CI";
  if (e[0].attrib)
    fprintf (outfile, (generate_random () & 1)
	     ? "TX%s(%d,%s %s,," : "TX%s(%d,%s,%s,", skip_cint,
	     idx, e[0].etype == ETYPE_STRUCT ? "struct" : "union",
	     e[0].attrib);
  else if (e[0].etype == ETYPE_STRUCT)
    fprintf (outfile, "T%s(%d,", skip_cint, idx);
  else
    fprintf (outfile, "U%s(%d,", skip_cint, idx);
  c = 'a';
  for (i = 1; i <= e[0].len; )
    i += subfield (e + i, &c);
  fputs (",", outfile);
  c = 'a';
  for (i = 1; i <= e[0].len; )
    {
      i += subvalues (e + i, namebuf, &c);
      if (e[0].etype == ETYPE_UNION)
        break;
    }
  fputs (")\n", outfile);
  if (output_one && idx == limidx)
    exit (0);
  ++idx;
}

enum FEATURE
{
  FEATURE_VECTOR = 1,
  FEATURE_COMPLEX = 2,
  FEATURE_ALIGNEDPACKED = 4,
  FEATURE_ZEROARRAY = 8,
  FEATURE_ZEROBITFLD = 16,
  ALL_FEATURES = FEATURE_COMPLEX | FEATURE_VECTOR | FEATURE_ZEROARRAY
		 | FEATURE_ALIGNEDPACKED | FEATURE_ZEROBITFLD
};

void
singles (enum FEATURE features)
{
  struct entry e[2];
  int i;
  memset (e, 0, sizeof (e));
  e[0].etype = ETYPE_STRUCT;
  output (e);
  e[0].etype = ETYPE_UNION;
  output (e);
  for (i = 0;
       i < ((features & FEATURE_ALIGNEDPACKED) ? NATTRIBS2 : NATTRIBS1);
       ++i)
    {
      e[0].attrib = attributes[i];
      e[0].etype = ETYPE_STRUCT;
      output (e);
      e[0].etype = ETYPE_UNION;
      output (e);
    }
  e[0].len = 1;
  e[0].attrib = NULL;
  for (i = 0; i < NTYPES2; ++i)
    {
      e[0].etype = ETYPE_STRUCT;
      e[1].etype = ETYPE_TYPE;
      e[1].type = &base_types[i];
      output (e);
      e[0].etype = ETYPE_UNION;
      output (e);
    }
  if (features & FEATURE_COMPLEX)
    for (i = 0; i < NCTYPES2; ++i)
      {
	e[0].etype = ETYPE_STRUCT;
	e[1].etype = ETYPE_TYPE;
	e[1].type = &complex_types[i];
	output (e);
	e[0].etype = ETYPE_UNION;
	output (e);
      }
  if (features & FEATURE_VECTOR)
    for (i = 0; i < NVTYPES2; ++i)
      {
	e[0].etype = ETYPE_STRUCT;
	e[1].etype = ETYPE_TYPE;
	e[1].type = &vector_types[i];
	output (e);
	e[0].etype = ETYPE_UNION;
	output (e);
      }
}

void
choose_type (enum FEATURE features, struct entry *e, int r, int in_array)
{
  int i;

  i = NTYPES2 - NTYPES1;
  if (features & FEATURE_COMPLEX)
    i += NCTYPES2;
  if (features & FEATURE_VECTOR)
    i += NVTYPES2;
  if ((r & 3) == 0)
    {
      if (in_array)
	{
	  i += NAATYPES2;
	  if (features & FEATURE_COMPLEX)
	    i += NCAATYPES2;
	}
      else
	{
	  i += NATYPES2;
	  if (features & FEATURE_COMPLEX)
	    i += NCATYPES2;
	}
    }
  r >>= 2;
  r %= i;
  if (r < NTYPES2 - NTYPES1)
    e->type = &base_types[r + NTYPES1];
  r -= NTYPES2 - NTYPES1;
  if (e->type == NULL && (features & FEATURE_COMPLEX))
    {
      if (r < NCTYPES2)
	e->type = &complex_types[r];
      r -= NCTYPES2;
    }
  if (e->type == NULL && (features & FEATURE_VECTOR))
    {
      if (r < NVTYPES2)
	e->type = &vector_types[r];
      r -= NVTYPES2;
    }
  if (e->type == NULL && !in_array)
    {
      if (r < NATYPES2)
	e->type = &attrib_types[r];
      r -= NATYPES2;
    }
  if (e->type == NULL && !in_array && (features & FEATURE_COMPLEX))
    {
      if (r < NCATYPES2)
	e->type = &complex_attrib_types[r];
      r -= NCATYPES2;
    }
  if (e->type == NULL && in_array)
    {
      if (r < NAATYPES2)
	e->type = &attrib_array_types[r];
      r -= NAATYPES2;
    }
  if (e->type == NULL && in_array && (features & FEATURE_COMPLEX))
    {
      if (r < NCAATYPES2)
	e->type = &complex_attrib_array_types[r];
      r -= NCAATYPES2;
    }
  if (e->type == NULL)
    abort ();
}

/* This is from gcc.c-torture/execute/builtin-bitops-1.c.  */
static int
my_ffsll (unsigned long long x)
{
  int i;
  if (x == 0)
    return 0;
  /* We've tested LLONG_MAX for 64 bits so this should be safe.  */
  for (i = 0; i < 64; i++)
    if (x & (1ULL << i))
      break;
  return i + 1;
}

void
generate_fields (enum FEATURE features, struct entry *e, struct entry *parent,
		 int len)
{
  int r, i, j, ret = 1, n, incr, sametype;

  for (n = 0; n < len; n += incr)
    {
      r = generate_random ();
      /* 50% ETYPE_TYPE base_types NTYPES1
	 12.5% ETYPE_TYPE other
	 12.5% ETYPE_ARRAY
	 12.5% ETYPE_BITFLD
	 12.5% ETYPE_STRUCT|ETYPE_UNION|ETYPE_STRUCT_ARRAY|ETYPE_UNION_ARRAY */
      i = (r & 7);
      r >>= 3;
      incr = 1;
      switch (i)
	{
	case 0:
	case 1:
	case 2:
	case 3:
	  e[n].etype = ETYPE_TYPE;
	  e[n].type = &base_types[r % NTYPES1];
	  break;
	case 4:
	  e[n].etype = ETYPE_TYPE;
	  choose_type (features, &e[n], r, 0);
	  break;
	case 5:
	  e[n].etype = ETYPE_ARRAY;
	  i = r & 1;
	  r >>= 1;
	  if (i)
	    e[n].type = &base_types[r % NTYPES1];
	  else
	    choose_type (features, &e[n], r, 1);
	  r = generate_random ();
	  if ((features & FEATURE_ZEROARRAY) && (r & 3) == 0)
	    {
	      e[n].arr_len = 0;
	      if (n == len - 1 && (r & 4)
		  && (parent->etype == ETYPE_STRUCT
		      || parent->etype == ETYPE_STRUCT_ARRAY))
		{
		  int k;
		  for (k = 0; k < n; ++k)
		    if (e[k].etype != ETYPE_BITFLD || e[k].len)
		      {
			e[n].arr_len = 255;
			break;
		      }
		}
	    }
	  else if ((r & 3) != 3)
	    e[n].arr_len = (r >> 2) & 7;
	  else
	    e[n].arr_len = (r >> 2) & 31;
	  break;
	case 6:
	  sametype = 1;
	  switch (r & 7)
	    {
	    case 0:
	    case 1:
	    case 2:
	      break;
	    case 3:
	    case 4:
	    case 5:
	      incr = 1 + (r >> 3) % (len - n);
	      break;
	    case 6:
	    case 7:
	      sametype = 0;
	      incr = 1 + (r >> 3) % (len - n);
	      break;
	    }
	  for (j = n; j < n + incr; ++j)
	    {
	      int mi, ma;

	      e[j].etype = ETYPE_BITFLD;
	      if (j == n || !sametype)
		{
		  int k;
		  r = generate_random ();
		  k = r & 3;
		  r >>= 2;
		  if (!k)
		    e[j].type
		      = &aligned_bitfld_types[r % n_aligned_bitfld_types];
		  else
		    e[j].type
		      = &bitfld_types[r % n_bitfld_types];
		}
	      else
		e[j].type = e[n].type;
	      r = generate_random ();
	      mi = 0;
	      ma = 0;
	      switch (e[j].type->bitfld)
	        {
	        case 'C': ma = 8; break;
	        case 'S': ma = 16; break;
	        case 'I': ma = 32; break;
	        case 'L':
	        case 'Q': ma = 64; break;
	        case 'B': ma = 1; break;
	        case ' ':
		  if (e[j].type->type == TYPE_UENUM)
		    mi = my_ffsll (e[j].type->maxval + 1) - 1;
		  else if (e[j].type->type == TYPE_SENUM)
		    mi = my_ffsll (e[j].type->maxval + 1);
		  else
		    abort ();
		  if (!mi)
		    mi = 1;
		  if (mi <= 32)
		    ma = 32;
		  else
		    ma = 64;
		  break;
		default:
		  abort ();
	        }
	      e[j].len = ma + 1;
	      if (sametype && (r & 3) == 0 && ma > 1)
		{
		  int sum = 0, k;
		  for (k = n; k < j; ++k)
		    sum += e[k].len;
		  sum %= ma;
		  e[j].len = sum ? ma - sum : ma;
		}
	      r >>= 2;
	      if (! (features & FEATURE_ZEROBITFLD) && mi == 0)
		mi = 1;
	      if (e[j].len < mi || e[j].len > ma)
		e[j].len = mi + (r % (ma + 1 - mi));
	      r >>= 6;
	      if ((features & FEATURE_ZEROBITFLD) && (r & 3) == 0
		  && mi == 0)
		e[j].len = 0;
	    }
	  break;
	case 7:
	  switch (r & 7)
	    {
	    case 0:
	    case 1:
	    case 2:
	      e[n].etype = ETYPE_STRUCT;
	      break;
	    case 3:
	    case 4:
	      e[n].etype = ETYPE_UNION;
	      break;
	    case 5:
	    case 6:
	      e[n].etype = ETYPE_STRUCT_ARRAY;
	      break;
	    case 7:
	      e[n].etype = ETYPE_UNION_ARRAY;
	      break;
	    }
	  r >>= 3;
	  e[n].len = r % (len - n);
	  incr = 1 + e[n].len;
	  generate_fields (features, &e[n + 1], &e[n], e[n].len);
	  if (e[n].etype == ETYPE_STRUCT_ARRAY
	      || e[n].etype == ETYPE_UNION_ARRAY)
	    {
	      r = generate_random ();
	      if ((features & FEATURE_ZEROARRAY) && (r & 3) == 0)
		{
		  e[n].arr_len = 0;
		  if (n + incr == len && (r & 4)
		      && (parent->etype == ETYPE_STRUCT
			  || parent->etype == ETYPE_STRUCT_ARRAY))
		    {
		      int k;
		      for (k = 0; k < n; ++k)
			if (e[k].etype != ETYPE_BITFLD || e[k].len)
			  {
			    e[n].arr_len = 255;
			    break;
			  }
		    }
		}
	      else if ((r & 3) != 3)
		e[n].arr_len = (r >> 2) & 7;
	      else
		e[n].arr_len = (r >> 2) & 31;
	    }
	  break;
	}
      r = generate_random ();
      if ((r & 7) == 0)
	{
	  r >>= 3;
	  i = (features & FEATURE_ALIGNEDPACKED) ? NATTRIBS2 : NATTRIBS1;
	  e[n].attrib = attributes[r % i];
	  if (! (features & FEATURE_ALIGNEDPACKED)
	      && strcmp (e[n].attrib, "atpa") == 0
	      && ((e[n].type >= &attrib_types[0]
		   && e[n].type < &attrib_types[NATYPES2])
		  || (e[n].type >= &complex_attrib_types[0]
		      && e[n].type < &complex_attrib_types[NCATYPES2])
		  || (e[n].type >= &attrib_array_types[0]
		      && e[n].type < &attrib_array_types[NAATYPES2])
		  || (e[n].type >= &complex_attrib_array_types[0]
		      && e[n].type < &complex_attrib_array_types[NAATYPES2])
		  || (e[n].type >= &aligned_bitfld_types[0]
		      && e[n].type < &aligned_bitfld_types[n_aligned_bitfld_types])))
	    e[n].attrib = NULL;

	  /* If this is an array type, do not put aligned attributes on
	     elements.  Aligning elements to a value greater than their
	     size will result in a compiler error.  */

	  if ((e[n].etype == ETYPE_ARRAY)
	      && e[n].attrib != NULL
	      && (strncmp (e[n].attrib, "atal", 4) == 0))
            e[n].attrib = NULL;
	}
    }
}

void
generate_random_tests (enum FEATURE features, int len)
{
  struct entry e[len + 1];
  int i, r;
  if (len > 'z' - 'a' + 1)
    abort ();
  memset (e, 0, sizeof (e));
  r = generate_random ();
  if ((r & 7) == 0)
    e[0].etype = ETYPE_UNION;
  else
    e[0].etype = ETYPE_STRUCT;
  r >>= 3;
  e[0].len = len;
  if ((r & 31) == 0)
    {
      r >>= 5;
      if (features & FEATURE_ALIGNEDPACKED)
	r %= NATTRIBS2;
      else
	r %= NATTRIBS1;
      e[0].attrib = attributes[r];
    }
  generate_fields (features, &e[1], &e[0], len);
  output (e);
}

struct { const char *name; enum FEATURE f; }
features[] = {
{ "normal", 0 },
{ "complex", FEATURE_COMPLEX },
{ "vector", FEATURE_VECTOR },
{ "[0] :0", FEATURE_ZEROARRAY | FEATURE_ZEROBITFLD },
{ "complex vector [0]",
  FEATURE_COMPLEX | FEATURE_VECTOR | FEATURE_ZEROARRAY },
{ "aligned packed complex vector [0] :0",
  FEATURE_COMPLEX | FEATURE_VECTOR | FEATURE_ZEROARRAY
  | FEATURE_ALIGNEDPACKED | FEATURE_ZEROBITFLD },
};

int
main (int argc, char **argv)
{
  int i, j, count, c, n = 3000;
  char *optarg;

  if (sizeof (int) != 4 || sizeof (long long) != 8)
    return 1;
  
  i = 1;
  while (i < argc) 
    {
      c = '\0';
      if (argv[i][0] == '-' && argv[i][2] == '\0')
	c = argv[i][1];
      optarg = argv[i + 1];
      if (!optarg)
	goto usage;
      switch (c)
	{
	case 'n':
	  n = atoi (optarg);
	  break;
	case 'd':
	  destdir = optarg;
	  break;
	case 's':
	  srcdir = optarg;
	  break;
	case 'i':
	  output_one = 1;
	  limidx = atoi (optarg);
	  break;
	default:
	  fprintf (stderr, "unrecognized option %s\n", argv[i]);
	  goto usage;
      }
      i += 2;
    }

  if (output_one)
    {
      outfile = fopen ("/dev/null", "w");
      if (outfile == NULL)
        {
	  fputs ("could not open /dev/null", stderr);
	  return 1;
        }
      n = limidx + 1;
    }

  if (destdir == NULL && !output_one)
    {
    usage:
      fprintf (stderr, "Usage:\n\
%s [-s srcdir -d destdir] [-n count] [-i idx]\n\
Either -s srcdir -d destdir or -i idx must be used\n", argv[0]);
      return 1;
    }

  if (srcdir == NULL && !output_one)
    goto usage;

  for (i = 0; i < NTYPES2; ++i)
    if (base_types[i].bitfld)
      bitfld_types[n_bitfld_types++] = base_types[i];
  for (i = 0; i < NATYPES2; ++i)
    if (attrib_types[i].bitfld)
      aligned_bitfld_types[n_aligned_bitfld_types++] = attrib_types[i];
  for (i = 0; i < sizeof (features) / sizeof (features[0]); ++i)
    {
      int startidx = idx;
      if (! output_one)
	limidx = idx;
      if (!i)
        count = 200;
      else
        count = 20;
      for (j = 1; j <= 9; ++j)
        while (idx < startidx + j * count)
	  generate_random_tests (features[i].f, j);
      while (idx < startidx + count * 10)
	generate_random_tests (features[i].f, 10 + (generate_random () % 16));
    }
  for (i = 0; n > 3000 && i < sizeof (features) / sizeof (features[0]); ++i)
    {
      int startidx;
      startidx = idx;
      if (! output_one)
	limidx = idx;
      singles (features[i].f);
      if (!i)
	{
	  count = 1000;
	  while (idx < startidx + 1000)
	    generate_random_tests (features[i].f, 1);
	}
      else
	{
	  startidx = idx;
	  count = 100;
	  while (idx < startidx + 100)
	    generate_random_tests (features[i].f, 1);
	}
      startidx = idx;
      for (j = 2; j <= 9; ++j)
	while (idx < startidx + (j - 1) * count)
	  generate_random_tests (features[i].f, j);
      while (idx < startidx + count * 9)
        generate_random_tests (features[i].f, 10 + (generate_random () % 16));
    }
  if (! output_one)
    limidx = idx;
  while (idx < n)
    generate_random_tests (ALL_FEATURES, 1 + (generate_random () % 25));
  fclose (outfile);
  return 0;
}
