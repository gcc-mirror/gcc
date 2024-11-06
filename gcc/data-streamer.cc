/* Generic streaming support for basic data types.

   Copyright (C) 2011-2024 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "data-streamer.h"

/* For offloading -- While streaming-out, host NUM_POLY_INT_COEFFS is
   stored at beginning of mode_table.  While streaming-in, the value is read
   in host_num_poly_int_coeffs.  */

#ifdef ACCEL_COMPILER
unsigned host_num_poly_int_coeffs = 0;
#endif

/* Pack WORK into BP in a variant of uleb format.  */

void
bp_pack_var_len_unsigned (struct bitpack_d *bp, unsigned HOST_WIDE_INT work)
{
  do
    {
      unsigned int half_byte = (work & 0x7);
      work >>= 3;
      if (work != 0)
	/* More half_bytes to follow.  */
	half_byte |= 0x8;

      bp_pack_value (bp, half_byte, 4);
    }
  while (work != 0);
}


/* Pack WORK into BP in a variant of sleb format.  */

void
bp_pack_var_len_int (struct bitpack_d *bp, HOST_WIDE_INT work)
{
  int more, half_byte;

  do
    {
      half_byte = (work & 0x7);
      /* arithmetic shift */
      work >>= 3;
      more = !((work == 0 && (half_byte & 0x4) == 0)
	       || (work == -1 && (half_byte & 0x4) != 0));
      if (more)
	half_byte |= 0x8;

      bp_pack_value (bp, half_byte, 4);
    }
  while (more);
}


/* Unpack VAL from BP in a variant of uleb format.  */

unsigned HOST_WIDE_INT
bp_unpack_var_len_unsigned (struct bitpack_d *bp)
{
  unsigned HOST_WIDE_INT result = 0;
  int shift = 0;
  unsigned HOST_WIDE_INT half_byte;

  while (true)
    {
      half_byte = bp_unpack_value (bp, 4);
      result |= (half_byte & 0x7) << shift;
      shift += 3;
      if ((half_byte & 0x8) == 0)
	return result;
    }
}


/* Unpack VAL from BP in a variant of sleb format.  */

HOST_WIDE_INT
bp_unpack_var_len_int (struct bitpack_d *bp)
{
  HOST_WIDE_INT result = 0;
  int shift = 0;
  unsigned HOST_WIDE_INT half_byte;

  while (true)
    {
      half_byte = bp_unpack_value (bp, 4);
      result |= (half_byte & 0x7) << shift;
      shift += 3;
      if ((half_byte & 0x8) == 0)
	{
	  if ((shift < HOST_BITS_PER_WIDE_INT) && (half_byte & 0x4))
	    result |= - (HOST_WIDE_INT_1U << shift);

	  return result;
	}
    }
}

/* Pack REAL_VALUE_TYPE R into BP.  */

void
bp_pack_real_value (struct bitpack_d *bp, const REAL_VALUE_TYPE *r)
{
  bp_pack_value (bp, r->cl, 2);
  bp_pack_value (bp, r->decimal, 1);
  bp_pack_value (bp, r->sign, 1);
  bp_pack_value (bp, r->signalling, 1);
  bp_pack_value (bp, r->canonical, 1);
  bp_pack_value (bp, r->uexp, EXP_BITS);
  for (unsigned i = 0; i < SIGSZ; i++)
    bp_pack_value (bp, r->sig[i], HOST_BITS_PER_LONG);
}

/* Unpack REAL_VALUE_TYPE R from BP.  */

void
bp_unpack_real_value (struct bitpack_d *bp, REAL_VALUE_TYPE *r)
{
  /* Clear all bits of the real value type so that we can later do
     bitwise comparisons to see if two values are the same.  */
  memset (r, 0, sizeof (*r));
  r->cl = (unsigned) bp_unpack_value (bp, 2);
  r->decimal = (unsigned) bp_unpack_value (bp, 1);
  r->sign = (unsigned) bp_unpack_value (bp, 1);
  r->signalling = (unsigned) bp_unpack_value (bp, 1);
  r->canonical = (unsigned) bp_unpack_value (bp, 1);
  r->uexp = (unsigned) bp_unpack_value (bp, EXP_BITS);
  for (unsigned i = 0; i < SIGSZ; i++)
    r->sig[i] = (unsigned long) bp_unpack_value (bp, HOST_BITS_PER_LONG);
}
