/* Routines for restoring various data types from a file stream.  This deals
   with various data types like strings, integers, enums, etc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "data-streamer.h"
#include "value-range.h"
#include "streamer-hooks.h"

/* Read a string from the string table in DATA_IN using input block
   IB.  Write the length to RLEN.  */

static const char *
string_for_index (class data_in *data_in, unsigned int loc, unsigned int *rlen)
{
  unsigned int len;
  const char *result;

  if (!loc)
    {
      *rlen = 0;
      return NULL;
    }

  /* Get the string stored at location LOC in DATA_IN->STRINGS.  */
  lto_input_block str_tab (data_in->strings, loc - 1, data_in->strings_len, NULL);
  len = streamer_read_uhwi (&str_tab);
  *rlen = len;

  if (str_tab.p + len > data_in->strings_len)
    internal_error ("bytecode stream: string too long for the string table");

  result = (const char *)(data_in->strings + str_tab.p);

  return result;
}


/* Read a string from the string table in DATA_IN using input block
   IB.  Write the length to RLEN.  */

const char *
streamer_read_indexed_string (class data_in *data_in,
			      class lto_input_block *ib, unsigned int *rlen)
{
  return string_for_index (data_in, streamer_read_uhwi (ib), rlen);
}


/* Read a NULL terminated string from the string table in DATA_IN.  */

const char *
streamer_read_string (class data_in *data_in, class lto_input_block *ib)
{
  unsigned int len;
  const char *ptr;

  ptr = streamer_read_indexed_string (data_in, ib, &len);
  if (!ptr)
    return NULL;
  if (ptr[len - 1] != '\0')
    internal_error ("bytecode stream: found non-null terminated string");

  return ptr;
}


/* Read a string from the string table in DATA_IN using the bitpack BP.
   Write the length to RLEN.  */

const char *
bp_unpack_indexed_string (class data_in *data_in,
			  struct bitpack_d *bp, unsigned int *rlen)
{
  return string_for_index (data_in, bp_unpack_var_len_unsigned (bp), rlen);
}


/* Read a NULL terminated string from the string table in DATA_IN.  */

const char *
bp_unpack_string (class data_in *data_in, struct bitpack_d *bp)
{
  unsigned int len;
  const char *ptr;

  ptr = bp_unpack_indexed_string (data_in, bp, &len);
  if (!ptr)
    return NULL;
  if (ptr[len - 1] != '\0')
    internal_error ("bytecode stream: found non-null terminated string");

  return ptr;
}


/* Read an unsigned HOST_WIDE_INT number from IB.  */

unsigned HOST_WIDE_INT
streamer_read_uhwi (class lto_input_block *ib)
{
  unsigned HOST_WIDE_INT result;
  int shift;
  unsigned HOST_WIDE_INT byte;
  unsigned int p = ib->p;
  unsigned int len = ib->len;

  const char *data = ib->data;
  result = data[p++];
  if ((result & 0x80) != 0)
    {
      result &= 0x7f;
      shift = 7;
      do
	{
	  byte = data[p++];
	  result |= (byte & 0x7f) << shift;
	  shift += 7;
	}
      while ((byte & 0x80) != 0);
    }

  /* We check for section overrun after the fact for performance reason.  */
  if (p > len)
    lto_section_overrun (ib);

  ib->p = p;
  return result;
}


/* Read a HOST_WIDE_INT number from IB.  */

HOST_WIDE_INT
streamer_read_hwi (class lto_input_block *ib)
{
  HOST_WIDE_INT result = 0;
  int shift = 0;
  unsigned HOST_WIDE_INT byte;

  while (true)
    {
      byte = streamer_read_uchar (ib);
      result |= (byte & 0x7f) << shift;
      shift += 7;
      if ((byte & 0x80) == 0)
	{
	  if ((shift < HOST_BITS_PER_WIDE_INT) && (byte & 0x40))
	    result |= - (HOST_WIDE_INT_1U << shift);

	  return result;
	}
    }
}

/* Read a poly_uint64 from IB.  */

poly_uint64
streamer_read_poly_uint64 (class lto_input_block *ib)
{
  using coeff_type = poly_int_traits<poly_uint64>::coeff_type;
  return poly_int_read_common<coeff_type> (streamer_read_uhwi, ib);
}

/* Read a poly_int64 from IB.  */

poly_int64
streamer_read_poly_int64 (class lto_input_block *ib)
{
  using coeff_type = poly_int_traits<poly_int64>::coeff_type;
  return poly_int_read_common<coeff_type> (streamer_read_hwi, ib);
}

/* Read gcov_type value from IB.  */

gcov_type
streamer_read_gcov_count (class lto_input_block *ib)
{
  gcov_type ret = streamer_read_hwi (ib);
  return ret;
}

/* Read REAL_VALUE_TYPE from IB.  */

void
streamer_read_real_value (class lto_input_block *ib, REAL_VALUE_TYPE *r)
{
  struct bitpack_d bp = streamer_read_bitpack (ib);
  bp_unpack_real_value (&bp, r);
}

void
streamer_read_value_range (class lto_input_block *ib, data_in *data_in,
			   value_range &vr)
{
  // Read the common fields to all vranges.
  value_range_kind kind = streamer_read_enum (ib, value_range_kind, VR_LAST);
  gcc_checking_assert (kind != VR_UNDEFINED);
  tree type = stream_read_tree (ib, data_in);

  // Initialize the value_range to the correct type.
  vr.set_type (type);

  if (is_a <irange> (vr))
    {
      irange &r = as_a <irange> (vr);
      r.set_undefined ();
      unsigned HOST_WIDE_INT num_pairs = streamer_read_uhwi (ib);
      for (unsigned i = 0; i < num_pairs; ++i)
	{
	  wide_int lb = streamer_read_wide_int (ib);
	  wide_int ub = streamer_read_wide_int (ib);
	  int_range<2> tmp (type, lb, ub);
	  r.union_ (tmp);
	}
      wide_int value = streamer_read_wide_int (ib);
      wide_int mask = streamer_read_wide_int (ib);
      irange_bitmask bm (value, mask);
      r.update_bitmask (bm);
      return;
    }
  if (is_a <frange> (vr))
    {
      frange &r = as_a <frange> (vr);

      // Stream in NAN bits.
      struct bitpack_d bp = streamer_read_bitpack (ib);
      bool pos_nan = (bool) bp_unpack_value (&bp, 1);
      bool neg_nan = (bool) bp_unpack_value (&bp, 1);
      nan_state nan (pos_nan, neg_nan);

      if (kind == VR_NAN)
	r.set_nan (type, nan);
      else
	{
	  REAL_VALUE_TYPE lb, ub;
	  streamer_read_real_value (ib, &lb);
	  streamer_read_real_value (ib, &ub);
	  r.set (type, lb, ub, nan);
	}
      return;
    }
  if (is_a <prange> (vr))
    {
      prange &r = as_a <prange> (vr);
      wide_int lb = streamer_read_wide_int (ib);
      wide_int ub = streamer_read_wide_int (ib);
      r.set (type, lb, ub);
      wide_int value = streamer_read_wide_int (ib);
      wide_int mask = streamer_read_wide_int (ib);
      irange_bitmask bm (value, mask);
      r.update_bitmask (bm);
      return;
    }
  gcc_unreachable ();
}

/* Read the physical representation of a wide_int val from
   input block IB.  */

wide_int
streamer_read_wide_int (class lto_input_block *ib)
{
  HOST_WIDE_INT abuf[WIDE_INT_MAX_INL_ELTS], *a = abuf;
  int i;
  int prec = streamer_read_uhwi (ib);
  int len = streamer_read_uhwi (ib);
  if (UNLIKELY (len > WIDE_INT_MAX_INL_ELTS))
    a = XALLOCAVEC (HOST_WIDE_INT, len);
  for (i = 0; i < len; i++)
    a[i] = streamer_read_hwi (ib);
  return wide_int::from_array (a, len, prec);
}

/* Read the physical representation of a widest_int val from
   input block IB.  */

widest_int
streamer_read_widest_int (class lto_input_block *ib)
{
  HOST_WIDE_INT abuf[WIDE_INT_MAX_INL_ELTS], *a = abuf;
  int i;
  int prec ATTRIBUTE_UNUSED = streamer_read_uhwi (ib);
  int len = streamer_read_uhwi (ib);
  if (UNLIKELY (len > WIDE_INT_MAX_INL_ELTS))
    a = XALLOCAVEC (HOST_WIDE_INT, len);
  for (i = 0; i < len; i++)
    a[i] = streamer_read_hwi (ib);
  return widest_int::from_array (a, len);
}

