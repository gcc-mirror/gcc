/* Utility macros to handle Java(TM) byte codes.

   Copyright (C) 1996, 1998, 1999  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

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

/* A signed 64-bit (or more) integral type, suiteable for Java's 'long'. */
#ifndef int64
#define int64 long long
#endif
/* An unsigned 64-bit (or more) integral type, same length as int64. */
#ifndef uint64
#define uint64 unsigned int64
#endif

typedef uint16			jchar;
#ifdef __STDC__
typedef	signed char		jbyte;
#else
typedef	char			jbyte;
#endif
typedef int16                   jshort;
typedef int32                   jint;
typedef int64                   jlong;
typedef void*                   jref;

/* A 32-bit IEEE single-precision float. */
#ifndef jfloat 
#define jfloat float
#endif

/* A 32-bit IEEE double-precision float. */
#ifndef jdouble
#define jdouble double
#endif

union Word {
  jint i;
  jfloat f;
  void *p;
};

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
{ union Word wu;
  wu.i = w;
  return wu.f;
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

union DWord {
  jdouble d;
  jlong l;
  jword w[2];
};

static inline jdouble
WORDS_TO_DOUBLE(jword hi, jword lo)
{ union DWord wu;
#if (1 == HOST_FLOAT_WORDS_BIG_ENDIAN)
  wu.l = WORDS_TO_LONG(lo, hi);
#else
  wu.l = WORDS_TO_LONG(hi, lo);
#endif
  return wu.d;
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
