/* Conversion routines from GCC internal float representation to MPFR.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.

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
#include "realmpfr.h"
#include "tree.h"	/* For TYPE_MODE in real_from_mpfr.  */
#include "stor-layout.h"

/* Convert from REAL_VALUE_TYPE to MPFR.  The caller is responsible
   for initializing and clearing the MPFR parameter.  */

void
mpfr_from_real (mpfr_ptr m, const REAL_VALUE_TYPE *r, mp_rnd_t rndmode)
{
  /* We use a string as an intermediate type.  */
  char buf[128];
  int ret;

  /* Take care of Infinity and NaN.  */
  if (r->cl == rvc_inf)
    {
      mpfr_set_inf (m, r->sign == 1 ? -1 : 1);
      return;
    }

  if (r->cl == rvc_nan)
    {
      mpfr_set_nan (m);
      return;
    }

  real_to_hexadecimal (buf, r, sizeof (buf), 0, 1);
  /* mpfr_set_str() parses hexadecimal floats from strings in the same
     format that GCC will output them.  Nothing extra is needed.  */
  ret = mpfr_set_str (m, buf, 16, rndmode);
  gcc_assert (ret == 0);
}

/* Convert from MPFR to REAL_VALUE_TYPE, for a given type TYPE and rounding
   mode RNDMODE.  TYPE is only relevant if M is a NaN.  */

void
real_from_mpfr (REAL_VALUE_TYPE *r, mpfr_srcptr m, tree type, mp_rnd_t rndmode)
{
  /* We use a string as an intermediate type.  */
  char buf[128], *rstr;
  mp_exp_t exp;

  /* Take care of Infinity and NaN.  */
  if (mpfr_inf_p (m))
    {
      real_inf (r);
      if (mpfr_sgn (m) < 0)
	*r = real_value_negate (r);
      return;
    }

  if (mpfr_nan_p (m))
    {
      real_nan (r, "", 1, TYPE_MODE (type));
      return;
    }

  rstr = mpfr_get_str (NULL, &exp, 16, 0, m, rndmode);

  /* The additional 12 chars add space for the sprintf below.  This
     leaves 6 digits for the exponent which is supposedly enough.  */
  gcc_assert (rstr != NULL && strlen (rstr) < sizeof (buf) - 12);

  /* REAL_VALUE_ATOF expects the exponent for mantissa * 2**exp,
     mpfr_get_str returns the exponent for mantissa * 16**exp, adjust
     for that.  */
  exp *= 4;

  if (rstr[0] == '-')
    sprintf (buf, "-0x.%sp%d", &rstr[1], (int) exp);
  else
    sprintf (buf, "0x.%sp%d", rstr, (int) exp);

  mpfr_free_str (rstr);

  real_from_string (r, buf);
}

