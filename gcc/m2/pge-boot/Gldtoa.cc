/* Gldtoa.c provides access to long double string conversion.

Copyright (C) 2016-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"

#include "gm2-libs-host.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_FP_DIGITS 500

typedef enum Mode { maxsignicant, decimaldigits } Mode;

extern int dtoa_calcmaxsig (char *p, int ndigits);
extern int dtoa_calcdecimal (char *p, int str_size, int ndigits);
extern int dtoa_calcsign (char *p, int str_size);

/* maxsignicant: return a string containing max(1,ndigits)
   significant digits.  The return string contains the string
   produced by snprintf.  decimaldigits: return a string produced by
   fcvt.  The string will contain ndigits past the decimal point
   (ndigits may be negative).  */

long double
ldtoa_strtold (const char *s, int *error)
{
  char *endp;
  long double d;

  errno = 0;
#if defined(HAVE_STRTOLD)
  d = strtold (s, &endp);
#else
  /* fall back to using strtod.  */
  d = (long double)strtod (s, &endp);
#endif
  if (endp != NULL && (*endp == '\0'))
    *error = (errno != 0);
  else
    *error = TRUE;
  return d;
}

char *
ldtoa_ldtoa (long double d, int mode, int ndigits, int *decpt, int *sign)
{
  char format[50];
  char *p;
  int r;
  switch (mode)
    {

    case maxsignicant:
      ndigits += 20; /* enough for exponent.  */
      p = (char *)malloc (ndigits);
      snprintf (format, 50, "%s%d%s", "%.", ndigits - 20, "LE");
      snprintf (p, ndigits, format, d);
      *sign = dtoa_calcsign (p, ndigits);
      *decpt = dtoa_calcmaxsig (p, ndigits);
      return p;
    case decimaldigits:
      p = (char *)malloc (MAX_FP_DIGITS + 20);
      snprintf (format, 50, "%s%d%s", "%.", MAX_FP_DIGITS, "LE");
      snprintf (p, MAX_FP_DIGITS + 20, format, d);
      *sign = dtoa_calcsign (p, MAX_FP_DIGITS + 20);
      *decpt = dtoa_calcdecimal (p, MAX_FP_DIGITS + 20, ndigits);
      return p;
    default:
      abort ();
    }
}

/* GNU Modula-2 hooks */

void
_M2_ldtoa_init (void)
{
}

void
_M2_ldtoa_finish (void)
{
}
#   ifdef __cplusplus
}
#   endif
