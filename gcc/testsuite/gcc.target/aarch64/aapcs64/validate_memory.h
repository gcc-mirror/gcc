/* Memory validation functions for AArch64 procedure call standard.
   Copyright (C) 2012 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef VALIDATE_MEMORY_H
#define VALIDATE_MEMORY_H

enum structure_type
{
  flat = 0,
  i32in128,
  f32in64,
  i8in64,
  i16in64,
  i32in64,
};

/* Some explicit declarations as I can't include files outside the testsuite.
 */
typedef long unsigned int size_t;
int memcmp (void *, void *, size_t);

/* These two arrays contain element size and block size data for the enumeration
   above.  */
const int element_size[] =       { 1, 4,  4, 1, 2, 4 };
const int block_reverse_size[] = { 1, 16, 8, 8, 8, 8 };

int
validate_memory (void *mem1, char *mem2, size_t size, enum structure_type type)
{
  /* In big-endian mode, the data in mem2 will have been byte-reversed in
     register sized groups, while the data in mem1 will have been byte-reversed
     according to the true structure of the data.  To compare them, we need to
     compare chunks of data in reverse order.

     This is only implemented for homogeneous data layouts at the moment.  For
     hetrogeneous structures a custom compare case will need to be written.  */

  unsigned int i;
  char *cmem1 = (char *) mem1;
  switch (type)
    {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    case i8in64:
    case i16in64:
    case i32in64:
      for (i = 0; i < size; i += element_size[type])
	{
	  if (memcmp (cmem1 + i,
		      mem2 + block_reverse_size[type] - i - element_size[type],
		      element_size[type]))
	    return 1;
	}
      return 0;
      break;
#endif
    case f32in64:
    case i32in128:
    default:
      break;
    }
  return memcmp (mem1, mem2, size);
}

#endif  /* VALIDATE_MEMORY_H.  */
