/* Miscellaneous BPABI functions.

   Copyright (C) 2003, 2004  Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   In addition to the permissions in the GNU General Public License, the
   Free Software Foundation gives you unlimited permission to link the
   compiled version of this file into combinations with other programs,
   and to distribute those combinations without any restriction coming
   from the use of this file.  (The General Public License restrictions
   do apply in other respects; for example, they cover modification of
   the file, and distribution when not linked into a combine
   executable.)

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

extern double __aeabi_ul2d (unsigned long long);
extern float __aeabi_ul2f (unsigned long long);
extern long long __divdi3 (long long, long long);
extern unsigned long long __udivdi3 (unsigned long long, 
				     unsigned long long);
extern long long __gnu_ldivmod_helper (long long, long long, long long *);
extern unsigned long long __gnu_uldivmod_helper (unsigned long long, 
						 unsigned long long, 
						 unsigned long long *);

/* These functions are based on __floatdidf and __floatdisf, but
   convert unsigned DImode values instead of signed DImode
   values.  */

#define WORD_SIZE (sizeof (int) * 8)
#define HIGH_HALFWORD_COEFF (((unsigned long long) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((unsigned long long) 1) << WORD_SIZE)

double
__aeabi_ul2d (unsigned long long u)
{
  double d = (unsigned) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (unsigned) (u & (HIGH_WORD_COEFF - 1));

  return d;
}

float
__aeabi_ul2f (unsigned long long u)
{
  /* Do the calculation in DFmode so that we don't lose any of the
     precision of the high word while multiplying it.  */
  double f = (unsigned) (u >> WORD_SIZE);
  f *= HIGH_HALFWORD_COEFF;
  f *= HIGH_HALFWORD_COEFF;
  f += (unsigned) (u & (HIGH_WORD_COEFF - 1));

  return (float) f;
}

long long
__gnu_ldivmod_helper (long long a, 
		      long long b, 
		      long long *remainder)
{
  long long quotient;

  quotient = __divdi3 (a, b);
  *remainder = a - b * quotient;
  return quotient;
}

unsigned long long
__gnu_uldivmod_helper (unsigned long long a, 
		       unsigned long long b,
		       unsigned long long *remainder)
{
  unsigned long long quotient;

  quotient = __udivdi3 (a, b);
  *remainder = a - b * quotient;
  return quotient;
}
