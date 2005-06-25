/* Utility macros to handle Java(TM) byte codes.

   Copyright (C) 1996, 1998, 1999, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#ifndef GCC_JAVAOP_H
#define GCC_JAVAOP_H

typedef	unsigned char	uint8;
#ifndef int16
#define int16 short
#endif
typedef unsigned int16	uint16;

#ifndef int32
#define int32 long
#endif
typedef unsigned int32	uint32;

/* A signed 64-bit (or more) integral type, suitable for Java's 'long'.  */
#ifndef int64
#define int64 long long
#endif
/* An unsigned 64-bit (or more) integral type, same length as int64. */
#ifndef uint64
#define uint64 unsigned int64
#endif

typedef uint16			jchar;
typedef	signed char		jbyte;
typedef int16                   jshort;
typedef int32                   jint;
typedef int64                   jlong;
typedef void*                   jref;

/* A 32-bit big-endian IEEE single-precision float. */
typedef struct _jfloat {
  unsigned int negative : 1;
  unsigned int exponent : 8;
  unsigned int mantissa : 23;
} jfloat;
#define JFLOAT_FINITE(f) ((f).exponent != 0xFF)
#define JFLOAT_QNAN_MASK 0x400000
#define JFLOAT_EXP_BIAS 0x7f

/* A 32-bit big-endian IEEE double-precision float. */
typedef struct _jdouble {
  unsigned int negative : 1;
  unsigned int exponent : 11;
  unsigned int mantissa0: 20;
  unsigned int mantissa1: 32;
} jdouble;
#define JDOUBLE_FINITE(f) ((f).exponent != 0x7FF)
#define JDOUBLE_QNAN_MASK 0x80000  /* apply to mantissa0 */
#define JDOUBLE_EXP_BIAS 0x3ff

/* A jword is an unsigned integral type big enough for a 32-bit jint
   or jfloat *or* a pointer.  It is the type appropriate for stack
   locations and local variables in a Java interpreter. */


#ifndef jword
#define jword uint32
#endif

#ifndef IMMEDIATE_u1
#define IMMEDIATE_u1 (PC++, CHECK_PC_IN_RANGE(PC), BCODE[PC-1])
#endif
#ifndef IMMEDIATE_s1
#define IMMEDIATE_s1 (PC++, CHECK_PC_IN_RANGE(PC), (signed char)BCODE[PC-1])
#endif
#ifndef IMMEDIATE_s2
#define IMMEDIATE_s2 (PC+=2, CHECK_PC_IN_RANGE(PC), \
  (signed char) BCODE[PC-2] * 256 + BCODE[PC-1])
#endif
#ifndef IMMEDIATE_u2
#define IMMEDIATE_u2 (PC+=2, CHECK_PC_IN_RANGE(PC),\
  (BCODE[PC-2] * 256 + BCODE[PC-1]))
#endif
#ifndef IMMEDIATE_s4
#define IMMEDIATE_s4 (PC+=4, CHECK_PC_IN_RANGE(PC), \
  (WORD_TO_INT((BCODE[PC-4] << 24) | (BCODE[PC-3] << 16) \
         | (BCODE[PC-2] << 8) | (BCODE[PC-1]))))
#endif

static inline jfloat
WORD_TO_FLOAT(jword w)
{
  jfloat f;

  f.negative = (w & 0x80000000) >> 31;
  f.exponent = (w & 0x7f800000) >> 23;
  f.mantissa = (w & 0x007fffff);

  return f;
} 

/* Sign extend w.  If the host on which this cross-compiler runs uses
   a 64-bit type for jword the appropriate sign extension is
   performed; if it's a 32-bit type the arithmetic does nothing but is
   harmless.  */
static inline jint
WORD_TO_INT(jword w)
{
  jint n = w & 0xffffffff; /* Mask lower 32 bits.  */
  n ^= (jint)1 << 31;
  n -= (jint)1 << 31; /* Sign extend lower 32 bits to upper.  */
  return n;
} 

static inline jlong
WORDS_TO_LONG(jword hi, jword lo)
{
  return ((jlong) hi << 32) | ((jlong)lo & (((jlong)1 << 32) -1));
}

static inline jdouble
WORDS_TO_DOUBLE(jword hi, jword lo)
{
  jdouble d;

  d.negative  = (hi & 0x80000000) >> 31;
  d.exponent  = (hi & 0x7ff00000) >> 20;
  d.mantissa0 = (hi & 0x000fffff);
  d.mantissa1 = lo;

  return d;
} 

/* If PREFIX_CHAR is the first character of the Utf8 encoding of a character,
   return the number of bytes taken by the encoding.
   Return -1 for a continuation character.  */
#define UT8_CHAR_LENGTH(PREFIX_CHAR) \
  ((unsigned char)(PREFIX_CHAR) < 128 ? 1 \
   : ((PREFIX_CHAR) & 0x40) == 0 ? -1 \
   : ((PREFIX_CHAR) & 0x20) == 0 ? 2 \
   : ((PREFIX_CHAR) & 0x10) == 0 ? 3 \
   : ((PREFIX_CHAR) & 0x08) == 0 ? 4 : 5)

#endif /* ! GCC_JAVAOP_H */
