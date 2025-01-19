/* d-port.cc -- D frontend interface to the gcc back-end.
   Copyright (C) 2013-2025 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/root/port.h"
#include "dmd/target.h"

#include "tree.h"


/* Implements the Port interface defined by the frontend.
   A mini library for doing compiler/system specific things.  */

/* Compare the first N bytes of S1 and S2 without regard to the case.  */

int
Port::memicmp (const char *s1, const char *s2, d_size_t n)
{
  int result = 0;

  for (d_size_t i = 0; i < n; i++)
    {
      char c1 = s1[i];
      char c2 = s2[i];

      result = c1 - c2;
      if (result)
	{
	  result = TOUPPER (c1) - TOUPPER (c2);
	  if (result)
	    break;
	}
    }

  return result;
}

/* Convert all characters in S to uppercase.  */

char *
Port::strupr (char *s)
{
  char *t = s;

  while (*s)
    {
      *s = TOUPPER (*s);
      s++;
    }

  return t;
}

/* Return true if the real_t value from string BUFFER overflows
   as a result of rounding down to float mode.  */

bool
Port::isFloat32LiteralOutOfRange (const char *buffer)
{
  real_t r;

  real_from_string3 (&r.rv (), buffer, TYPE_MODE (float_type_node));

  return r == target.RealProperties.infinity;
}

/* Return true if the real_t value from string BUFFER overflows
   as a result of rounding down to double mode.  */

bool
Port::isFloat64LiteralOutOfRange (const char *buffer)
{
  real_t r;

  real_from_string3 (&r.rv (), buffer, TYPE_MODE (double_type_node));

  return r == target.RealProperties.infinity;
}

/* Fetch a little-endian 16-bit value from BUFFER.  */

unsigned
Port::readwordLE (const void *buffer)
{
  const unsigned char *p = (const unsigned char *) buffer;

  return ((unsigned) p[1] << 8) | (unsigned) p[0];
}

/* Fetch a big-endian 16-bit value from BUFFER.  */

unsigned
Port::readwordBE (const void *buffer)
{
  const unsigned char *p = (const unsigned char *) buffer;

  return ((unsigned) p[0] << 8) | (unsigned) p[1];
}

/* Fetch a little-endian 32-bit value from BUFFER.  */

unsigned
Port::readlongLE (const void *buffer)
{
  const unsigned char *p = (const unsigned char *) buffer;

  return (((unsigned) p[3] << 24)
	  | ((unsigned) p[2] << 16)
	  | ((unsigned) p[1] << 8)
	  | (unsigned) p[0]);
}

/* Fetch a big-endian 32-bit value from BUFFER.  */

unsigned
Port::readlongBE (const void *buffer)
{
  const unsigned char *p = (const unsigned char *) buffer;

  return (((unsigned) p[0] << 24)
	  | ((unsigned) p[1] << 16)
	  | ((unsigned) p[2] << 8)
	  | (unsigned) p[3]);
}

/* Write an SZ-byte sized VALUE to BUFFER, ignoring endian-ness.  */

void
Port::valcpy (void *buffer, uint64_t value, d_size_t sz)
{
  gcc_assert (((d_size_t) buffer) % sz == 0);

  switch (sz)
    {
    case 1:
      *(uint8_t *) buffer = (uint8_t) value;
      break;

    case 2:
      *(uint16_t *) buffer = (uint16_t) value;
      break;

    case 4:
      *(uint32_t *) buffer = (uint32_t) value;
      break;

    case 8:
      *(uint64_t *) buffer = (uint64_t) value;
      break;

    default:
      gcc_unreachable ();
    }
}
