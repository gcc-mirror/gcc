/* Subroutines for long double support.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

int _U_Qfcmp (long double a, long double b, int);
long _U_Qfcnvfxt_quad_to_sgl (long double);

int _U_Qfeq (long double, long double);
int _U_Qfne (long double, long double);
int _U_Qfgt (long double, long double);
int _U_Qfge (long double, long double);
int _U_Qflt (long double, long double);
int _U_Qfle (long double, long double);
int _U_Qfcomp (long double, long double);
long double _U_Qfneg (long double);
#ifdef __LP64__
int __U_Qfcnvfxt_quad_to_sgl (long double);
#endif
unsigned int _U_Qfcnvfxt_quad_to_usgl(long double);

int
_U_Qfeq (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 4) != 0);
}

int
_U_Qfne (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 4) == 0);
}
	
int
_U_Qfgt (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 17) != 0);
}

int
_U_Qfge (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 21) != 0);
}

int
_U_Qflt (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 9) != 0);
}

int
_U_Qfle (long double a, long double b)
{
  return (_U_Qfcmp (a, b, 13) != 0);
}

int
_U_Qfcomp (long double a, long double b)
{
  if (_U_Qfcmp (a, b, 4) == 0)
    return 0;

  return (_U_Qfcmp (a, b, 22) != 0 ? 1 : -1);
}


long double
_U_Qfneg (long double a)
{
  return (0.0L - a);
}

#ifdef __LP64__
/* This routine is only necessary for the PA64 port; for reasons unknown
   _U_Qfcnvfxt_quad_to_sgl returns the integer in the high 32bits of the
   return value.  Ugh.  */
int
__U_Qfcnvfxt_quad_to_sgl (long double a)
{
  return _U_Qfcnvfxt_quad_to_sgl (a) >> 32;
}
#endif

/* HP only has signed conversion in library, so need to synthesize an
   unsigned version */
unsigned int
_U_Qfcnvfxt_quad_to_usgl(long double a)
{
  extern long long _U_Qfcnvfxt_quad_to_dbl (long double a);
  return (unsigned int) _U_Qfcnvfxt_quad_to_dbl (a);
}
