/* 128-bit long double support routines for Darwin.
   Copyright (C) 1993, 2003, 2004 Free Software Foundation, Inc.

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
   implemented: _xlqadd, _xlqsub, _xlqmul, and _xlqdiv.  Double-double
   algorithms are based on the paper "Doubled-Precision IEEE Standard
   754 Floating-Point Arithmetic" by W. Kahan, February 26, 1987.  An
   alternative published reference is "Software for Doubled-Precision
   Floating-Point Computations", by Seppo Linnainmaa, ACM TOMS vol 7
   no 3, September 1961, pages 272-283.  */

/* Each long double is made up of two IEEE doubles.  The value of the
   long double is the sum of the values of the two parts.  The most
   significant part is required to be the value of the long double
   rounded to the nearest double, as specified by IEEE.  For Inf
   values, the least significant part is required to be one of +0.0 or
   -0.0.  No other requirements are made; so, for example, 1.0 may be
   represented as (1.0, +0.0) or (1.0, -0.0), and the low part of a
   NaN is don't-care.

   This code currently assumes big-endian.  */

#if !_SOFT_FLOAT && (defined (__MACH__) || defined (__powerpc64__))

#define fabs(x) __builtin_fabs(x)

#define unlikely(x) __builtin_expect ((x), 0)

/* All these routines actually take two long doubles as parameters,
   but GCC currently generates poor code when a union is used to turn
   a long double into a pair of doubles.  */

extern long double _xlqadd (double, double, double, double);
extern long double _xlqsub (double, double, double, double);
extern long double _xlqmul (double, double, double, double);
extern long double _xlqdiv (double, double, double, double);

typedef union
{
  long double ldval;
  double dval[2];
} longDblUnion;

static const double FPKINF = 1.0/0.0;

/* Add two 'long double' values and return the result.	*/
long double
_xlqadd (double a, double b, double c, double d)
{
  longDblUnion z;
  double t, tau, u, FPR_zero, FPR_PosInf;

  FPR_zero = 0.0;
  FPR_PosInf = FPKINF;

  if (unlikely (a != a) || unlikely (c != c)) 
    return a + c;  /* NaN result.  */

  /* Ordered operands are arranged in order of their magnitudes.  */

  /* Switch inputs if |(c,d)| > |(a,b)|. */
  if (fabs (c) > fabs (a))
    {
      t = a;
      tau = b;
      a = c;
      b = d;
      c = t;
      d = tau;
    }

  /* b <- second largest magnitude double. */
  if (fabs (c) > fabs (b))
    {
      t = b;
      b = c;
      c = t;
    }

  /* Thanks to commutivity, sum is invariant w.r.t. the next
     conditional exchange. */
  tau = d + c;

  /* Order the smallest magnitude doubles.  */
  if (fabs (d) > fabs (c))
    {
      t = c;
      c = d;
      d = t;
    }

  t = (tau + b) + a;	     /* Sum values in ascending magnitude order.  */

  /* Infinite or zero result.  */
  if (unlikely (t == FPR_zero) || unlikely (fabs (t) == FPR_PosInf))
    return t;

  /* Usual case.  */
  tau = (((a-t) + b) + c) + d;
  u = t + tau;
  z.dval[0] = u;	       /* Final fixup for long double result.  */
  z.dval[1] = (t - u) + tau;
  return z.ldval;
}

long double
_xlqsub (double a, double b, double c, double d)
{
  return _xlqadd (a, b, -c, -d);
}

long double
_xlqmul (double a, double b, double c, double d)
{
  longDblUnion z;
  double t, tau, u, v, w, FPR_zero, FPR_PosInf;
  
  FPR_zero = 0.0;
  FPR_PosInf = FPKINF;

  t = a * c;			/* Highest order double term.  */

  if (unlikely (t != t) || unlikely (t == FPR_zero) 
      || unlikely (fabs (t) == FPR_PosInf))
    return t;

  /* Finite nonzero result requires summing of terms of two highest
     orders.	*/
  
  /* Use fused multiply-add to get low part of a * c.	 */
  asm ("fmsub %0,%1,%2,%3" : "=f"(tau) : "f"(a), "f"(c), "f"(t));
  v = a*d;
  w = b*c;
  tau += v + w;	    /* Add in other second-order terms.	 */
  u = t + tau;

  /* Construct long double result.  */
  z.dval[0] = u;
  z.dval[1] = (t - u) + tau;
  return z.ldval;
}

long double
_xlqdiv (double a, double b, double c, double d)
{
  longDblUnion z;
  double s, sigma, t, tau, u, v, w, FPR_zero, FPR_PosInf;
  
  FPR_zero = 0.0;
  FPR_PosInf = FPKINF;
  
  t = a / c;                    /* highest order double term */
  
  if (unlikely (t != t) || unlikely (t == FPR_zero) 
      || unlikely (fabs (t) == FPR_PosInf))
    return t;

  /* Finite nonzero result requires corrections to the highest order term.  */

  s = c * t;                    /* (s,sigma) = c*t exactly. */
  w = -(-b + d * t);	/* Written to get fnmsub for speed, but not
			   numerically necessary.  */
  
  /* Use fused multiply-add to get low part of c * t.	 */
  asm ("fmsub %0,%1,%2,%3" : "=f"(sigma) : "f"(c), "f"(t), "f"(s));
  v = a - s;
  
  tau = ((v-sigma)+w)/c;   /* Correction to t. */
  u = t + tau;

  /* Construct long double result. */
  z.dval[0] = u;
  z.dval[1] = (t - u) + tau;
  return z.ldval;
}

#endif
