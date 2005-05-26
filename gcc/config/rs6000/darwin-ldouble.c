/* 128-bit long double support routines for Darwin.
   Copyright (C) 1993, 2003, 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Implementations of floating-point long double basic arithmetic
   functions called by the IBM C compiler when generating code for
   PowerPC platforms.  In particular, the following functions are
   implemented: __gcc_qadd, __gcc_qsub, __gcc_qmul, and __gcc_qdiv.
   Double-double algorithms are based on the paper "Doubled-Precision
   IEEE Standard 754 Floating-Point Arithmetic" by W. Kahan, February 26,
   1987.  An alternative published reference is "Software for
   Doubled-Precision Floating-Point Computations", by Seppo Linnainmaa,
   ACM TOMS vol 7 no 3, September 1981, pages 272-283.  */

/* Each long double is made up of two IEEE doubles.  The value of the
   long double is the sum of the values of the two parts.  The most
   significant part is required to be the value of the long double
   rounded to the nearest double, as specified by IEEE.  For Inf
   values, the least significant part is required to be one of +0.0 or
   -0.0.  No other requirements are made; so, for example, 1.0 may be
   represented as (1.0, +0.0) or (1.0, -0.0), and the low part of a
   NaN is don't-care.

   This code currently assumes big-endian.  */

#if !_SOFT_FLOAT && (defined (__MACH__) || defined (__powerpc64__) || defined (_AIX))

#define fabs(x) __builtin_fabs(x)
#define isless(x, y) __builtin_isless (x, y)
#define inf() __builtin_inf()

#define unlikely(x) __builtin_expect ((x), 0)

#define nonfinite(a) unlikely (! isless (fabs (a), inf ()))

/* All these routines actually take two long doubles as parameters,
   but GCC currently generates poor code when a union is used to turn
   a long double into a pair of doubles.  */

extern long double __gcc_qadd (double, double, double, double);
extern long double __gcc_qsub (double, double, double, double);
extern long double __gcc_qmul (double, double, double, double);
extern long double __gcc_qdiv (double, double, double, double);

#if defined __ELF__ && defined SHARED
/* Provide definitions of the old symbol names to satisfy apps and
   shared libs built against an older libgcc.  To access the _xlq
   symbols an explicit version reference is needed, so these won't
   satisfy an unadorned reference like _xlqadd.  If dot symbols are
   not needed, the assembler will remove the aliases from the symbol
   table.  */
__asm__ (".symver __gcc_qadd,_xlqadd@GCC_3.4\n\t"
	 ".symver __gcc_qsub,_xlqsub@GCC_3.4\n\t"
	 ".symver __gcc_qmul,_xlqmul@GCC_3.4\n\t"
	 ".symver __gcc_qdiv,_xlqdiv@GCC_3.4\n\t"
	 ".symver .__gcc_qadd,._xlqadd@GCC_3.4\n\t"
	 ".symver .__gcc_qsub,._xlqsub@GCC_3.4\n\t"
	 ".symver .__gcc_qmul,._xlqmul@GCC_3.4\n\t"
	 ".symver .__gcc_qdiv,._xlqdiv@GCC_3.4");
#endif

typedef union
{
  long double ldval;
  double dval[2];
} longDblUnion;

/* Add two 'long double' values and return the result.	*/
long double
__gcc_qadd (double a, double aa, double c, double cc)
{
  longDblUnion x;
  double z, q, zz, xh;

  z = a + c;

  if (nonfinite (z))
    {
      z = cc + aa + c + a;
      if (nonfinite (z))
	return z;
      x.dval[0] = z;  /* Will always be DBL_MAX.  */
      zz = aa + cc;
      if (fabs(a) > fabs(c))
	x.dval[1] = a - z + c + zz;
      else
	x.dval[1] = c - z + a + zz;
    }
  else
    {
      q = a - z;
      zz = q + c + (a - (q + z)) + aa + cc;
      xh = z + zz;

      if (nonfinite (xh))
	return xh;

      x.dval[0] = xh;
      x.dval[1] = z - xh + zz;
    }
  return x.ldval;
}

long double
__gcc_qsub (double a, double b, double c, double d)
{
  return __gcc_qadd (a, b, -c, -d);
}

long double
__gcc_qmul (double a, double b, double c, double d)
{
  longDblUnion z;
  double t, tau, u, v, w;
  
  t = a * c;			/* Highest order double term.  */

  if (unlikely (t == 0)		/* Preserve -0.  */
      || nonfinite (t))
    return t;

  /* Sum terms of two highest orders. */
  
  /* Use fused multiply-add to get low part of a * c.  */
  asm ("fmsub %0,%1,%2,%3" : "=f"(tau) : "f"(a), "f"(c), "f"(t));
  v = a*d;
  w = b*c;
  tau += v + w;	    /* Add in other second-order terms.	 */
  u = t + tau;

  /* Construct long double result.  */
  if (nonfinite (u))
    return u;
  z.dval[0] = u;
  z.dval[1] = (t - u) + tau;
  return z.ldval;
}

long double
__gcc_qdiv (double a, double b, double c, double d)
{
  longDblUnion z;
  double s, sigma, t, tau, u, v, w;
  
  t = a / c;                    /* highest order double term */
  
  if (unlikely (t == 0)		/* Preserve -0.  */
      || nonfinite (t))
    return t;

  /* Finite nonzero result requires corrections to the highest order term.  */

  s = c * t;                    /* (s,sigma) = c*t exactly.  */
  w = -(-b + d * t);	/* Written to get fnmsub for speed, but not
			   numerically necessary.  */
  
  /* Use fused multiply-add to get low part of c * t.	 */
  asm ("fmsub %0,%1,%2,%3" : "=f"(sigma) : "f"(c), "f"(t), "f"(s));
  v = a - s;
  
  tau = ((v-sigma)+w)/c;   /* Correction to t.  */
  u = t + tau;

  /* Construct long double result.  */
  if (nonfinite (u))
    return u;
  z.dval[0] = u;
  z.dval[1] = (t - u) + tau;
  return z.ldval;
}

#endif
