/* d-ctfloat.cc -- D frontend interface to the gcc back-end.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "dmd/root/ctfloat.h"
#include "dmd/target.h"

#include "tree.h"


/* Implements the CTFloat interface defined by the frontend.
   Compile-time floating-pointer helper functions.  */

/* Return the absolute value of R.  */

real_t
CTFloat::fabs (real_t r)
{
  real_t x;
  real_arithmetic (&x.rv (), ABS_EXPR, &r.rv (), NULL);
  return x.normalize ();
}

/* Return the value of R * 2 ^^ EXP.  */

real_t
CTFloat::ldexp (real_t r, int exp)
{
  real_t x;
  real_ldexp (&x.rv (), &r.rv (), exp);
  return x.normalize ();
}

/* Return true if longdouble value X is identical to Y.  */

bool
CTFloat::isIdentical (real_t x, real_t y)
{
  real_value rx = x.rv ();
  real_value ry = y.rv ();
  return real_identical (&rx, &ry);
}

/* Return true if real_t value R is NaN.  */

bool
CTFloat::isNaN (real_t r)
{
  return REAL_VALUE_ISNAN (r.rv ());
}

/* Same as isNaN, but also check if is signalling.  */

bool
CTFloat::isSNaN (real_t r)
{
  return REAL_VALUE_ISSIGNALING_NAN (r.rv ());
}

/* Return true if real_t value is +Inf.  */

bool
CTFloat::isInfinity (real_t r)
{
  return REAL_VALUE_ISINF (r.rv ());
}

/* Return a real_t value from string BUFFER rounded to long double mode.  */

real_t
CTFloat::parse (const char *buffer, bool &overflow)
{
  real_t r;
  real_from_string3 (&r.rv (), buffer, TYPE_MODE (long_double_type_node));

  /* Front-end checks overflow to see if the value is representable.  */
  overflow = (r == target.RealProperties.infinity) ? true : false;

  return r;
}

/* Format the real_t value R to string BUFFER, bounded by BUF_SIZE, as a decimal
   or hexadecimal, converting the result to uppercase if FMT requests it.  */

int
CTFloat::sprint (char *buffer, d_size_t buf_size, char fmt, real_t r)
{
  if (fmt == 'a' || fmt == 'A')
    {
      /* Converting to a hexadecimal string.  */
      real_to_hexadecimal (buffer, &r.rv (), buf_size, 0, 1);
      int buflen;

      switch (fmt)
	{
	case 'A':
	  buflen = strlen (buffer);
	  for (int i = 0; i < buflen; i++)
	    buffer[i] = TOUPPER (buffer[i]);

	  return buflen;

	case 'a':
	  return strlen (buffer);

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* Note: restricting the precision of significant digits to 18.  */
      real_to_decimal (buffer, &r.rv (), 32, 18, 1);
      return strlen (buffer);
    }
}

/* Return a hash value for real_t value R.  */

size_t
CTFloat::hash (real_t r)
{
  return real_hash (&r.rv ());
}
