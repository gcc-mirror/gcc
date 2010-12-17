/* Operations on HOST_WIDE_INT.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"

#if GCC_VERSION < 3004

/* The functions clz_hwi, ctz_hwi, ffs_hwi, floor_log2 and exact_log2
   are defined as inline functions in hwint.h if GCC_VERSION >= 3004.
   The definitions here are used for older versions of GCC and non-GCC
   bootstrap compilers.  */

/* Given X, an unsigned number, return the largest int Y such that 2**Y <= X.
   If X is 0, return -1.  */

int
floor_log2 (unsigned HOST_WIDE_INT x)
{
  int t = 0;

  if (x == 0)
    return -1;

  if (HOST_BITS_PER_WIDE_INT > 64)
    if (x >= (unsigned HOST_WIDE_INT) 1 << (t + 64))
      t += 64;
  if (HOST_BITS_PER_WIDE_INT > 32)
    if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 32))
      t += 32;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 16))
    t += 16;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 8))
    t += 8;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 4))
    t += 4;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 2))
    t += 2;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 1))
    t += 1;

  return t;
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.  */

int
exact_log2 (unsigned HOST_WIDE_INT x)
{
  if (x != (x & -x))
    return -1;
  return floor_log2 (x);
}

/* Given X, an unsigned number, return the number of least significant bits
   that are zero.  When X == 0, the result is the word size.  */

int
ctz_hwi (unsigned HOST_WIDE_INT x)
{
  return x ? floor_log2 (x & -x) : HOST_BITS_PER_WIDE_INT;
}

/* Similarly for most significant bits.  */

int
clz_hwi (unsigned HOST_WIDE_INT x)
{
  return HOST_BITS_PER_WIDE_INT - 1 - floor_log2(x);
}

/* Similar to ctz_hwi, except that the least significant bit is numbered
   starting from 1, and X == 0 yields 0.  */

int
ffs_hwi (unsigned HOST_WIDE_INT x)
{
  return 1 + floor_log2 (x & -x);
}

#endif /* GCC_VERSION < 3004 */
