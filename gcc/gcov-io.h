/* Machine-independent I/O routines for gcov.
   Copyright (C) 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
   Contributed by Bob Manson <manson@cygnus.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCOV_IO_H
#define GCOV_IO_H
#include <stdio.h>
#include <sys/types.h>

static int __fetch_long	PARAMS ((long *, char *, size_t)) ATTRIBUTE_UNUSED;
static int __store_long PARAMS ((long, char *, size_t)) ATTRIBUTE_UNUSED;
static int __read_long  PARAMS ((long *, FILE *, size_t)) ATTRIBUTE_UNUSED;
static int __write_long PARAMS ((long, FILE *, size_t)) ATTRIBUTE_UNUSED;

/* These routines only work for signed values. */

/* Store a portable representation of VALUE in DEST using BYTES*8-1 bits.
   Return a non-zero value if VALUE requires more than BYTES*8-1 bits
   to store. */

static int
__store_long (value, dest, bytes)
     long value;
     char *dest;
     size_t bytes;
{
  int upper_bit = (value < 0 ? 128 : 0);
  size_t i;

  if (value < 0)
    {
      long oldvalue = value;
      value = -value;
      if (oldvalue != -value)
	return 1;
    }

  for(i = 0 ; i < (sizeof (value) < bytes ? sizeof (value) : bytes) ; i++) {
    dest[i] = value & (i == (bytes - 1) ? 127 : 255);
    value = value / 256;
  }

  if (value && value != -1)
    return 1;

  for(; i < bytes ; i++) 
    dest[i] = 0;
  dest[bytes - 1] |= upper_bit;
  return 0;
}

/* Retrieve a quantity containing BYTES*8-1 bits from SOURCE and store
   the result in DEST. Returns a non-zero value if the value in SOURCE
   will not fit in DEST. */

static int
__fetch_long (dest, source, bytes)
     long *dest;
     char *source;
     size_t bytes;
{
  long value = 0;
  int i;

  for (i = bytes - 1; (size_t) i > (sizeof (*dest) - 1); i--)
    if (source[i] & ((size_t) i == (bytes - 1) ? 127 : 255 ))
      return 1;

  for (; i >= 0; i--)
    value = value * 256 + (source[i] & ((size_t)i == (bytes - 1) ? 127 : 255));

  if ((source[bytes - 1] & 128) && (value > 0))
    value = - value;

  *dest = value;
  return 0;
}

/* Write a BYTES*8-bit quantity to FILE, portably. Returns a non-zero
   value if the write fails, or if VALUE can't be stored in BYTES*8
   bits.

   Note that VALUE may not actually be large enough to hold BYTES*8
   bits, but BYTES characters will be written anyway.

   BYTES may be a maximum of 10. */

static int
__write_long (value, file, bytes)
     long value;
     FILE *file;
     size_t bytes;
{
  char c[10];

  if (bytes > 10 || __store_long (value, c, bytes))
    return 1;
  else
    return fwrite(c, 1, bytes, file) != bytes;
}

/* Read a quantity containing BYTES bytes from FILE, portably. Return
   a non-zero value if the read fails or if the value will not fit
   in DEST.

   Note that DEST may not be large enough to hold all of the requested
   data, but the function will read BYTES characters anyway.

   BYTES may be a maximum of 10. */

static int
__read_long (dest, file, bytes)
     long *dest;
     FILE *file;
     size_t bytes;
{
  char c[10];

  if (bytes > 10 || fread(c, 1, bytes, file) != bytes)
    return 1;
  else
    return __fetch_long (dest, c, bytes);
}

#endif
