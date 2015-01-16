/* Incremential hashing for jhash.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.

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
#include "hashtab.h"
#include "inchash.h"

/* Borrowed from hashtab.c iterative_hash implementation.  */
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


/* Produce good hash value combining VAL and VAL2.  */
hashval_t
iterative_hash_hashval_t (hashval_t val, hashval_t val2)
{
  /* the golden ratio; an arbitrary value.  */
  hashval_t a = 0x9e3779b9;

  mix (a, val, val2);
  return val2;
}

/* Produce good hash value combining VAL and VAL2.  */

hashval_t
iterative_hash_host_wide_int (HOST_WIDE_INT val, hashval_t val2)
{
  if (sizeof (HOST_WIDE_INT) == sizeof (hashval_t))
    return iterative_hash_hashval_t (val, val2);
  else
    {
      hashval_t a = (hashval_t) val;
      /* Avoid warnings about shifting of more than the width of the type on
         hosts that won't execute this path.  */
      int zero = 0;
      hashval_t b = (hashval_t) (val >> (sizeof (hashval_t) * 8 + zero));
      mix (a, b, val2);
      if (sizeof (HOST_WIDE_INT) > 2 * sizeof (hashval_t))
	{
	  hashval_t a = (hashval_t) (val >> (sizeof (hashval_t) * 16 + zero));
	  hashval_t b = (hashval_t) (val >> (sizeof (hashval_t) * 24 + zero));
	  mix (a, b, val2);
	}
      return val2;
    }
}
