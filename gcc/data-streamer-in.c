/* Routines for restoring various data types from a file stream.  This deals
   with various data types like strings, integers, enums, etc.

   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
#include "diagnostic.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "data-streamer.h"

/* Read a string from the string table in DATA_IN using input block
   IB.  Write the length to RLEN.  */

const char *
string_for_index (struct data_in *data_in, unsigned int loc, unsigned int *rlen)
{
  struct lto_input_block str_tab;
  unsigned int len;
  const char *result;

  if (!loc)
    {
      *rlen = 0;
      return NULL;
    }

  /* Get the string stored at location LOC in DATA_IN->STRINGS.  */
  LTO_INIT_INPUT_BLOCK (str_tab, data_in->strings, loc - 1,
			data_in->strings_len);
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
streamer_read_indexed_string (struct data_in *data_in,
			      struct lto_input_block *ib, unsigned int *rlen)
{
  return string_for_index (data_in, streamer_read_uhwi (ib), rlen);
}


/* Read a NULL terminated string from the string table in DATA_IN.  */

const char *
streamer_read_string (struct data_in *data_in, struct lto_input_block *ib)
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
bp_unpack_indexed_string (struct data_in *data_in,
			  struct bitpack_d *bp, unsigned int *rlen)
{
  return string_for_index (data_in, bp_unpack_var_len_unsigned (bp), rlen);
}


/* Read a NULL terminated string from the string table in DATA_IN.  */

const char *
bp_unpack_string (struct data_in *data_in, struct bitpack_d *bp)
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
streamer_read_uhwi (struct lto_input_block *ib)
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
streamer_read_hwi (struct lto_input_block *ib)
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
	    result |= - ((HOST_WIDE_INT)1 << shift);

	  return result;
	}
    }
}

/* Read gcov_type value from IB.  */

gcov_type
streamer_read_gcov_count (struct lto_input_block *ib)
{
  gcov_type ret = streamer_read_hwi (ib);
  gcc_assert (ret >= 0);
  return ret;
}
