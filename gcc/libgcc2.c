/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989, 92, 93, 94, 95, 96, 97, 98, 1999, 2000
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "tsystem.h"

#include "machmode.h"
#include "defaults.h" 

/* Don't use `fancy_abort' here even if config.h says to use it.  */
#ifdef abort
#undef abort
#endif

/* In a cross-compilation situation, default to inhibiting compilation
   of routines that use libc.  */

#if defined(CROSS_COMPILE) && !defined(inhibit_libc)
#define inhibit_libc
#endif

#include "libgcc2.h"

#if defined (L_negdi2) || defined (L_divdi3) || defined (L_moddi3)
#if defined (L_divdi3) || defined (L_moddi3)
static inline
#endif
DWtype
__negdi2 (DWtype u)
{
  DWunion w;
  DWunion uu;

  uu.ll = u;

  w.s.low = -uu.s.low;
  w.s.high = -uu.s.high - ((UWtype) w.s.low > 0);

  return w.ll;
}
#endif

/* Unless shift functions are defined whith full ANSI prototypes,
   parameter b will be promoted to int if word_type is smaller than an int.  */
#ifdef L_lshrdi3
DWtype
__lshrdi3 (DWtype u, word_type b)
{
  DWunion w;
  word_type bm;
  DWunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (Wtype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (UWtype)uu.s.high >> -bm;
    }
  else
    {
      UWtype carries = (UWtype)uu.s.high << bm;
      w.s.high = (UWtype)uu.s.high >> b;
      w.s.low = ((UWtype)uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashldi3
DWtype
__ashldi3 (DWtype u, word_type b)
{
  DWunion w;
  word_type bm;
  DWunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (Wtype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (UWtype)uu.s.low << -bm;
    }
  else
    {
      UWtype carries = (UWtype)uu.s.low >> bm;
      w.s.low = (UWtype)uu.s.low << b;
      w.s.high = ((UWtype)uu.s.high << b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashrdi3
DWtype
__ashrdi3 (DWtype u, word_type b)
{
  DWunion w;
  word_type bm;
  DWunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (Wtype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      /* w.s.high = 1..1 or 0..0 */
      w.s.high = uu.s.high >> (sizeof (Wtype) * BITS_PER_UNIT - 1);
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      UWtype carries = (UWtype)uu.s.high << bm;
      w.s.high = uu.s.high >> b;
      w.s.low = ((UWtype)uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ffsdi2
DWtype
__ffsdi2 (DWtype u)
{
  DWunion uu, w;
  uu.ll = u;
  w.s.high = 0;
  w.s.low = ffs (uu.s.low);
  if (w.s.low != 0)
    return w.ll;
  w.s.low = ffs (uu.s.high);
  if (w.s.low != 0)
    {
      w.s.low += BITS_PER_UNIT * sizeof (Wtype);
      return w.ll;
    }
  return w.ll;
}
#endif

#ifdef L_muldi3
DWtype
__muldi3 (DWtype u, DWtype v)
{
  DWunion w;
  DWunion uu, vv;

  uu.ll = u,
  vv.ll = v;

  w.ll = __umulsidi3 (uu.s.low, vv.s.low);
  w.s.high += ((UWtype) uu.s.low * (UWtype) vv.s.high
	       + (UWtype) uu.s.high * (UWtype) vv.s.low);

  return w.ll;
}
#endif

#ifdef L_udiv_w_sdiv
#if defined (sdiv_qrnnd)
UWtype
__udiv_w_sdiv (UWtype *rp, UWtype a1, UWtype a0, UWtype d)
{
  UWtype q, r;
  UWtype c0, c1, b1;

  if ((Wtype) d >= 0)
    {
      if (a1 < d - a1 - (a0 >> (W_TYPE_SIZE - 1)))
	{
	  /* dividend, divisor, and quotient are nonnegative */
	  sdiv_qrnnd (q, r, a1, a0, d);
	}
      else
	{
	  /* Compute c1*2^32 + c0 = a1*2^32 + a0 - 2^31*d */
	  sub_ddmmss (c1, c0, a1, a0, d >> 1, d << (W_TYPE_SIZE - 1));
	  /* Divide (c1*2^32 + c0) by d */
	  sdiv_qrnnd (q, r, c1, c0, d);
	  /* Add 2^31 to quotient */
	  q += (UWtype) 1 << (W_TYPE_SIZE - 1);
	}
    }
  else
    {
      b1 = d >> 1;			/* d/2, between 2^30 and 2^31 - 1 */
      c1 = a1 >> 1;			/* A/2 */
      c0 = (a1 << (W_TYPE_SIZE - 1)) + (a0 >> 1);

      if (a1 < b1)			/* A < 2^32*b1, so A/2 < 2^31*b1 */
	{
	  sdiv_qrnnd (q, r, c1, c0, b1); /* (A/2) / (d/2) */

	  r = 2*r + (a0 & 1);		/* Remainder from A/(2*b1) */
	  if ((d & 1) != 0)
	    {
	      if (r >= q)
		r = r - q;
	      else if (q - r <= d)
		{
		  r = r - q + d;
		  q--;
		}
	      else
		{
		  r = r - q + 2*d;
		  q -= 2;
		}
	    }
	}
      else if (c1 < b1)			/* So 2^31 <= (A/2)/b1 < 2^32 */
	{
	  c1 = (b1 - 1) - c1;
	  c0 = ~c0;			/* logical NOT */

	  sdiv_qrnnd (q, r, c1, c0, b1); /* (A/2) / (d/2) */

	  q = ~q;			/* (A/2)/b1 */
	  r = (b1 - 1) - r;

	  r = 2*r + (a0 & 1);		/* A/(2*b1) */

	  if ((d & 1) != 0)
	    {
	      if (r >= q)
		r = r - q;
	      else if (q - r <= d)
		{
		  r = r - q + d;
		  q--;
		}
	      else
		{
		  r = r - q + 2*d;
		  q -= 2;
		}
	    }
	}
      else				/* Implies c1 = b1 */
	{				/* Hence a1 = d - 1 = 2*b1 - 1 */
	  if (a0 >= -d)
	    {
	      q = -1;
	      r = a0 + d;
	    }
	  else
	    {
	      q = -2;
	      r = a0 + 2*d;
	    }
	}
    }

  *rp = r;
  return q;
}
#else
/* If sdiv_qrnnd doesn't exist, define dummy __udiv_w_sdiv.  */
UWtype
__udiv_w_sdiv (UWtype *rp __attribute__ ((__unused__)),
	       UWtype a1 __attribute__ ((__unused__)),
	       UWtype a0 __attribute__ ((__unused__)),
	       UWtype d __attribute__ ((__unused__)))
{
  return 0;
}
#endif
#endif

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
#define L_udivmoddi4
#endif

#ifdef L_udivmoddi4
static const UQItype __clz_tab[] =
{
  0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
};

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
static inline
#endif
UDWtype
__udivmoddi4 (UDWtype n, UDWtype d, UDWtype *rp)
{
  DWunion ww;
  DWunion nn, dd;
  DWunion rr;
  UWtype d0, d1, n0, n1, n2;
  UWtype q0, q1;
  UWtype b, bm;

  nn.ll = n;
  dd.ll = d;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;

#if !UDIV_NEEDS_NORMALIZATION
  if (d1 == 0)
    {
      if (d0 > n1)
	{
	  /* 0q = nn / 0D */

	  udiv_qrnnd (q0, n0, n1, n0, d0);
	  q1 = 0;

	  /* Remainder in n0.  */
	}
      else
	{
	  /* qq = NN / 0d */

	  if (d0 == 0)
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */

	  udiv_qrnnd (q1, n1, 0, n1, d0);
	  udiv_qrnnd (q0, n0, n1, n0, d0);

	  /* Remainder in n0.  */
	}

      if (rp != 0)
	{
	  rr.s.low = n0;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }

#else /* UDIV_NEEDS_NORMALIZATION */

  if (d1 == 0)
    {
      if (d0 > n1)
	{
	  /* 0q = nn / 0D */

	  count_leading_zeros (bm, d0);

	  if (bm != 0)
	    {
	      /* Normalize, i.e. make the most significant bit of the
		 denominator set.  */

	      d0 = d0 << bm;
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
	      n0 = n0 << bm;
	    }

	  udiv_qrnnd (q0, n0, n1, n0, d0);
	  q1 = 0;

	  /* Remainder in n0 >> bm.  */
	}
      else
	{
	  /* qq = NN / 0d */

	  if (d0 == 0)
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */

	  count_leading_zeros (bm, d0);

	  if (bm == 0)
	    {
	      /* From (n1 >= d0) /\ (the most significant bit of d0 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 leading quotient digit q1 = 1).

		 This special case is necessary, not an optimization.
		 (Shifts counts of W_TYPE_SIZE are undefined.)  */

	      n1 -= d0;
	      q1 = 1;
	    }
	  else
	    {
	      /* Normalize.  */

	      b = W_TYPE_SIZE - bm;

	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q1, n1, n2, n1, d0);
	    }

	  /* n1 != d0...  */

	  udiv_qrnnd (q0, n0, n1, n0, d0);

	  /* Remainder in n0 >> bm.  */
	}

      if (rp != 0)
	{
	  rr.s.low = n0 >> bm;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }
#endif /* UDIV_NEEDS_NORMALIZATION */

  else
    {
      if (d1 > n1)
	{
	  /* 00 = nn / DD */

	  q0 = 0;
	  q1 = 0;

	  /* Remainder in n1n0.  */
	  if (rp != 0)
	    {
	      rr.s.low = n0;
	      rr.s.high = n1;
	      *rp = rr.ll;
	    }
	}
      else
	{
	  /* 0q = NN / dd */

	  count_leading_zeros (bm, d1);
	  if (bm == 0)
	    {
	      /* From (n1 >= d1) /\ (the most significant bit of d1 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 quotient digit q0 = 0 or 1).

		 This special case is necessary, not an optimization.  */

	      /* The condition on the next line takes advantage of that
		 n1 >= d1 (true due to program flow).  */
	      if (n1 > d1 || n0 >= d0)
		{
		  q0 = 1;
		  sub_ddmmss (n1, n0, n1, n0, d1, d0);
		}
	      else
		q0 = 0;

	      q1 = 0;

	      if (rp != 0)
		{
		  rr.s.low = n0;
		  rr.s.high = n1;
		  *rp = rr.ll;
		}
	    }
	  else
	    {
	      UWtype m1, m0;
	      /* Normalize.  */

	      b = W_TYPE_SIZE - bm;

	      d1 = (d1 << bm) | (d0 >> b);
	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q0, n1, n2, n1, d1);
	      umul_ppmm (m1, m0, q0, d0);

	      if (m1 > n1 || (m1 == n1 && m0 > n0))
		{
		  q0--;
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
		}

	      q1 = 0;

	      /* Remainder in (n1n0 - m1m0) >> bm.  */
	      if (rp != 0)
		{
		  sub_ddmmss (n1, n0, n1, n0, m1, m0);
		  rr.s.low = (n1 << b) | (n0 >> bm);
		  rr.s.high = n1 >> bm;
		  *rp = rr.ll;
		}
	    }
	}
    }

  ww.s.low = q0;
  ww.s.high = q1;
  return ww.ll;
}
#endif

#ifdef L_divdi3
DWtype
__divdi3 (DWtype u, DWtype v)
{
  word_type c = 0;
  DWunion uu, vv;
  DWtype w;

  uu.ll = u;
  vv.ll = v;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = __negdi2 (uu.ll);
  if (vv.s.high < 0)
    c = ~c,
    vv.ll = __negdi2 (vv.ll);

  w = __udivmoddi4 (uu.ll, vv.ll, (UDWtype *) 0);
  if (c)
    w = __negdi2 (w);

  return w;
}
#endif

#ifdef L_moddi3
DWtype
__moddi3 (DWtype u, DWtype v)
{
  word_type c = 0;
  DWunion uu, vv;
  DWtype w;

  uu.ll = u;
  vv.ll = v;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = __negdi2 (uu.ll);
  if (vv.s.high < 0)
    vv.ll = __negdi2 (vv.ll);

  (void) __udivmoddi4 (uu.ll, vv.ll, &w);
  if (c)
    w = __negdi2 (w);

  return w;
}
#endif

#ifdef L_umoddi3
UDWtype
__umoddi3 (UDWtype u, UDWtype v)
{
  UDWtype w;

  (void) __udivmoddi4 (u, v, &w);

  return w;
}
#endif

#ifdef L_udivdi3
UDWtype
__udivdi3 (UDWtype n, UDWtype d)
{
  return __udivmoddi4 (n, d, (UDWtype *) 0);
}
#endif

#ifdef L_cmpdi2
word_type
__cmpdi2 (DWtype a, DWtype b)
{
  DWunion au, bu;

  au.ll = a, bu.ll = b;

  if (au.s.high < bu.s.high)
    return 0;
  else if (au.s.high > bu.s.high)
    return 2;
  if ((UWtype) au.s.low < (UWtype) bu.s.low)
    return 0;
  else if ((UWtype) au.s.low > (UWtype) bu.s.low)
    return 2;
  return 1;
}
#endif

#ifdef L_ucmpdi2
word_type
__ucmpdi2 (DWtype a, DWtype b)
{
  DWunion au, bu;

  au.ll = a, bu.ll = b;

  if ((UWtype) au.s.high < (UWtype) bu.s.high)
    return 0;
  else if ((UWtype) au.s.high > (UWtype) bu.s.high)
    return 2;
  if ((UWtype) au.s.low < (UWtype) bu.s.low)
    return 0;
  else if ((UWtype) au.s.low > (UWtype) bu.s.low)
    return 2;
  return 1;
}
#endif

#if defined(L_fixunstfdi) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 128)
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunstfdi (TFtype a)
{
  TFtype b;
  UDWtype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  v = (UWtype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the TFtype, leaving the low part as flonum.  */
  a -= (TFtype)v;
  /* Convert that to fixed (but not to DWtype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (UWtype) (- a);
  else
    v += (UWtype) a;
  return v;
}
#endif

#if defined(L_fixtfdi) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 128)
DWtype
__fixtfdi (TFtype a)
{
  if (a < 0)
    return - __fixunstfdi (-a);
  return __fixunstfdi (a);
}
#endif

#if defined(L_fixunsxfdi) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunsxfdi (XFtype a)
{
  XFtype b;
  UDWtype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  v = (UWtype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the XFtype, leaving the low part as flonum.  */
  a -= (XFtype)v;
  /* Convert that to fixed (but not to DWtype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (UWtype) (- a);
  else
    v += (UWtype) a;
  return v;
}
#endif

#if defined(L_fixxfdi) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 96)
DWtype
__fixxfdi (XFtype a)
{
  if (a < 0)
    return - __fixunsxfdi (-a);
  return __fixunsxfdi (a);
}
#endif

#ifdef L_fixunsdfdi
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunsdfdi (DFtype a)
{
  DFtype b;
  UDWtype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  v = (UWtype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the DFtype, leaving the low part as flonum.  */
  a -= (DFtype)v;
  /* Convert that to fixed (but not to DWtype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (UWtype) (- a);
  else
    v += (UWtype) a;
  return v;
}
#endif

#ifdef L_fixdfdi
DWtype
__fixdfdi (DFtype a)
{
  if (a < 0)
    return - __fixunsdfdi (-a);
  return __fixunsdfdi (a);
}
#endif

#ifdef L_fixunssfdi
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunssfdi (SFtype original_a)
{
  /* Convert the SFtype to a DFtype, because that is surely not going
     to lose any bits.  Some day someone else can write a faster version
     that avoids converting to DFtype, and verify it really works right.  */
  DFtype a = original_a;
  DFtype b;
  UDWtype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  v = (UWtype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the DFtype, leaving the low part as flonum.  */
  a -= (DFtype)v;
  /* Convert that to fixed (but not to DWtype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (UWtype) (- a);
  else
    v += (UWtype) a;
  return v;
}
#endif

#ifdef L_fixsfdi
DWtype
__fixsfdi (SFtype a)
{
  if (a < 0)
    return - __fixunssfdi (-a);
  return __fixunssfdi (a);
}
#endif

#if defined(L_floatdixf) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDWtype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

XFtype
__floatdixf (DWtype u)
{
  XFtype d;

  d = (Wtype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (UWtype) (u & (HIGH_WORD_COEFF - 1));

  return d;
}
#endif

#if defined(L_floatditf) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 128)
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDWtype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

TFtype
__floatditf (DWtype u)
{
  TFtype d;

  d = (Wtype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (UWtype) (u & (HIGH_WORD_COEFF - 1));

  return d;
}
#endif

#ifdef L_floatdidf
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDWtype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DFtype
__floatdidf (DWtype u)
{
  DFtype d;

  d = (Wtype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (UWtype) (u & (HIGH_WORD_COEFF - 1));

  return d;
}
#endif

#ifdef L_floatdisf
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDWtype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)
#define DI_SIZE (sizeof (DWtype) * BITS_PER_UNIT)

/* Define codes for all the float formats that we know of.  Note
   that this is copied from real.h.  */
   
#define UNKNOWN_FLOAT_FORMAT 0
#define IEEE_FLOAT_FORMAT 1
#define VAX_FLOAT_FORMAT 2
#define IBM_FLOAT_FORMAT 3

/* Default to IEEE float if not specified.  Nearly all machines use it.  */
#ifndef HOST_FLOAT_FORMAT
#define	HOST_FLOAT_FORMAT	IEEE_FLOAT_FORMAT
#endif

#if HOST_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
#define DF_SIZE 53
#define SF_SIZE 24
#endif

#if HOST_FLOAT_FORMAT == IBM_FLOAT_FORMAT
#define DF_SIZE 56
#define SF_SIZE 24
#endif

#if HOST_FLOAT_FORMAT == VAX_FLOAT_FORMAT
#define DF_SIZE 56
#define SF_SIZE 24
#endif

SFtype
__floatdisf (DWtype u)
{
  /* Do the calculation in DFmode
     so that we don't lose any of the precision of the high word
     while multiplying it.  */
  DFtype f;

  /* Protect against double-rounding error.
     Represent any low-order bits, that might be truncated in DFmode,
     by a bit that won't be lost.  The bit can go in anywhere below the
     rounding position of the SFmode.  A fixed mask and bit position
     handles all usual configurations.  It doesn't handle the case
     of 128-bit DImode, however.  */
  if (DF_SIZE < DI_SIZE
      && DF_SIZE > (DI_SIZE - DF_SIZE + SF_SIZE))
    {
#define REP_BIT ((UWtype) 1 << (DI_SIZE - DF_SIZE))
      if (! (- ((DWtype) 1 << DF_SIZE) < u
	     && u < ((DWtype) 1 << DF_SIZE)))
	{
	  if ((UWtype) u & (REP_BIT - 1))
	    u |= REP_BIT;
	}
    }
  f = (Wtype) (u >> WORD_SIZE);
  f *= HIGH_HALFWORD_COEFF;
  f *= HIGH_HALFWORD_COEFF;
  f += (UWtype) (u & (HIGH_WORD_COEFF - 1));

  return (SFtype) f;
}
#endif

#if defined(L_fixunsxfsi) && LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 96
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>

UWtype
__fixunsxfsi (XFtype a)
{
  if (a >= - (DFtype) LONG_MIN)
    return (Wtype) (a + LONG_MIN) - LONG_MIN;
  return (Wtype) a;
}
#endif

#ifdef L_fixunsdfsi
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>

UWtype
__fixunsdfsi (DFtype a)
{
  if (a >= - (DFtype) LONG_MIN)
    return (Wtype) (a + LONG_MIN) - LONG_MIN;
  return (Wtype) a;
}
#endif

#ifdef L_fixunssfsi
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>

UWtype
__fixunssfsi (SFtype a)
{
  if (a >= - (SFtype) LONG_MIN)
    return (Wtype) (a + LONG_MIN) - LONG_MIN;
  return (Wtype) a;
}
#endif

/* From here on down, the routines use normal data types.  */

#define SItype bogus_type
#define USItype bogus_type
#define DItype bogus_type
#define UDItype bogus_type
#define SFtype bogus_type
#define DFtype bogus_type
#undef Wtype
#undef UWtype
#undef HWtype
#undef UHWtype
#undef DWtype
#undef UDWtype

#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double

#ifdef L__gcc_bcmp

/* Like bcmp except the sign is meaningful.
   Result is negative if S1 is less than S2,
   positive if S1 is greater, 0 if S1 and S2 are equal.  */

int
__gcc_bcmp (const unsigned char *s1, const unsigned char *s2, size_t size)
{
  while (size > 0)
    {
      unsigned char c1 = *s1++, c2 = *s2++;
      if (c1 != c2)
	return c1 - c2;
      size--;
    }
  return 0;
}

#endif

#ifdef L__dummy
void
__dummy (void) {}
#endif

#ifdef L_varargs
#ifdef __i860__
#if defined(__svr4__) || defined(__alliant__)
	asm ("	.text");
	asm ("	.align	4");

/* The Alliant needs the added underscore.  */
	asm (".globl	__builtin_saveregs");
asm ("__builtin_saveregs:");
	asm (".globl	___builtin_saveregs");
asm ("___builtin_saveregs:");

        asm ("	andnot	0x0f,%sp,%sp");	/* round down to 16-byte boundary */
	asm ("	adds	-96,%sp,%sp");  /* allocate stack space for reg save
					   area and also for a new va_list
					   structure */
	/* Save all argument registers in the arg reg save area.  The
	   arg reg save area must have the following layout (according
	   to the svr4 ABI):

		struct {
		  union  {
		    float freg[8];
		    double dreg[4];
		  } float_regs;
		  long	ireg[12];
		};
	*/

	asm ("	fst.q	%f8,  0(%sp)"); /* save floating regs (f8-f15)  */
	asm ("	fst.q	%f12,16(%sp)"); 

	asm ("	st.l	%r16,32(%sp)"); /* save integer regs (r16-r27) */
	asm ("	st.l	%r17,36(%sp)"); 
	asm ("	st.l	%r18,40(%sp)");
	asm ("	st.l	%r19,44(%sp)");
	asm ("	st.l	%r20,48(%sp)");
	asm ("	st.l	%r21,52(%sp)");
	asm ("	st.l	%r22,56(%sp)");
	asm ("	st.l	%r23,60(%sp)");
	asm ("	st.l	%r24,64(%sp)");
	asm ("	st.l	%r25,68(%sp)");
	asm ("	st.l	%r26,72(%sp)");
	asm ("	st.l	%r27,76(%sp)");

	asm ("	adds	80,%sp,%r16");  /* compute the address of the new
					   va_list structure.  Put in into
					   r16 so that it will be returned
					   to the caller.  */

	/* Initialize all fields of the new va_list structure.  This
	   structure looks like:

		typedef struct {
		    unsigned long	ireg_used;
		    unsigned long	freg_used;
		    long		*reg_base;
		    long		*mem_ptr;
		} va_list;
	*/

	asm ("	st.l	%r0, 0(%r16)"); /* nfixed */
	asm ("	st.l	%r0, 4(%r16)"); /* nfloating */
	asm ("  st.l    %sp, 8(%r16)"); /* __va_ctl points to __va_struct.  */
	asm ("	bri	%r1");		/* delayed return */
	asm ("	st.l	%r28,12(%r16)"); /* pointer to overflow args */

#else /* not __svr4__ */
#if defined(__PARAGON__)
	/*
	 *	we'll use SVR4-ish varargs but need SVR3.2 assembler syntax,
	 *	and we stand a better chance of hooking into libraries
	 *	compiled by PGI.  [andyp@ssd.intel.com]
	 */
	asm ("	.text");
	asm ("	.align	4");
	asm (".globl	__builtin_saveregs");
asm ("__builtin_saveregs:");
	asm (".globl	___builtin_saveregs");
asm ("___builtin_saveregs:");

        asm ("	andnot	0x0f,sp,sp");	/* round down to 16-byte boundary */
	asm ("	adds	-96,sp,sp");	/* allocate stack space for reg save
					   area and also for a new va_list
					   structure */
	/* Save all argument registers in the arg reg save area.  The
	   arg reg save area must have the following layout (according
	   to the svr4 ABI):

		struct {
		  union  {
		    float freg[8];
		    double dreg[4];
		  } float_regs;
		  long	ireg[12];
		};
	*/

	asm ("	fst.q	f8,  0(sp)");
	asm ("	fst.q	f12,16(sp)"); 
	asm ("	st.l	r16,32(sp)");
	asm ("	st.l	r17,36(sp)"); 
	asm ("	st.l	r18,40(sp)");
	asm ("	st.l	r19,44(sp)");
	asm ("	st.l	r20,48(sp)");
	asm ("	st.l	r21,52(sp)");
	asm ("	st.l	r22,56(sp)");
	asm ("	st.l	r23,60(sp)");
	asm ("	st.l	r24,64(sp)");
	asm ("	st.l	r25,68(sp)");
	asm ("	st.l	r26,72(sp)");
	asm ("	st.l	r27,76(sp)");

	asm ("	adds	80,sp,r16");  /* compute the address of the new
					   va_list structure.  Put in into
					   r16 so that it will be returned
					   to the caller.  */

	/* Initialize all fields of the new va_list structure.  This
	   structure looks like:

		typedef struct {
		    unsigned long	ireg_used;
		    unsigned long	freg_used;
		    long		*reg_base;
		    long		*mem_ptr;
		} va_list;
	*/

	asm ("	st.l	r0, 0(r16)"); /* nfixed */
	asm ("	st.l	r0, 4(r16)"); /* nfloating */
	asm ("  st.l    sp, 8(r16)"); /* __va_ctl points to __va_struct.  */
	asm ("	bri	r1");		/* delayed return */
	asm ("	 st.l	r28,12(r16)"); /* pointer to overflow args */
#else /* not __PARAGON__ */
	asm ("	.text");
	asm ("	.align	4");

	asm (".globl	___builtin_saveregs");
	asm ("___builtin_saveregs:");
	asm ("	mov	sp,r30");
	asm ("	andnot	0x0f,sp,sp");
	asm ("	adds	-96,sp,sp");  /* allocate sufficient space on the stack */

/* Fill in the __va_struct.  */
	asm ("	st.l	r16, 0(sp)"); /* save integer regs (r16-r27) */
	asm ("	st.l	r17, 4(sp)"); /* int	fixed[12] */
	asm ("	st.l	r18, 8(sp)");
	asm ("	st.l	r19,12(sp)");
	asm ("	st.l	r20,16(sp)");
	asm ("	st.l	r21,20(sp)");
	asm ("	st.l	r22,24(sp)");
	asm ("	st.l	r23,28(sp)");
	asm ("	st.l	r24,32(sp)");
	asm ("	st.l	r25,36(sp)");
	asm ("	st.l	r26,40(sp)");
	asm ("	st.l	r27,44(sp)");

	asm ("	fst.q	f8, 48(sp)"); /* save floating regs (f8-f15) */
	asm ("	fst.q	f12,64(sp)"); /* int floating[8] */

/* Fill in the __va_ctl.  */
	asm ("  st.l    sp, 80(sp)"); /* __va_ctl points to __va_struct.  */
	asm ("	st.l	r28,84(sp)"); /* pointer to more args */
	asm ("	st.l	r0, 88(sp)"); /* nfixed */
	asm ("	st.l	r0, 92(sp)"); /* nfloating */

	asm ("	adds	80,sp,r16");  /* return address of the __va_ctl.  */
	asm ("	bri	r1");
	asm ("	mov	r30,sp");
				/* recover stack and pass address to start 
				   of data.  */
#endif /* not __PARAGON__ */
#endif /* not __svr4__ */
#else /* not __i860__ */
#ifdef __sparc__
	asm (".global __builtin_saveregs");
	asm ("__builtin_saveregs:");
	asm (".global ___builtin_saveregs");
	asm ("___builtin_saveregs:");
#ifdef NEED_PROC_COMMAND
	asm (".proc 020");
#endif
	asm ("st %i0,[%fp+68]");
	asm ("st %i1,[%fp+72]");
	asm ("st %i2,[%fp+76]");
	asm ("st %i3,[%fp+80]");
	asm ("st %i4,[%fp+84]");
	asm ("retl");
	asm ("st %i5,[%fp+88]");
#ifdef NEED_TYPE_COMMAND
	asm (".type __builtin_saveregs,#function");
	asm (".size __builtin_saveregs,.-__builtin_saveregs");
#endif
#else /* not __sparc__ */
#if defined(__MIPSEL__) | defined(__R3000__) | defined(__R2000__) | defined(__mips__)

  asm ("	.text");
#ifdef __mips16
  asm ("	.set nomips16");
#endif
  asm ("	.ent __builtin_saveregs");
  asm ("	.globl __builtin_saveregs");
  asm ("__builtin_saveregs:");
  asm ("	sw	$4,0($30)");
  asm ("	sw	$5,4($30)");
  asm ("	sw	$6,8($30)");
  asm ("	sw	$7,12($30)");
  asm ("	j	$31");
  asm ("	.end __builtin_saveregs");
#else /* not __mips__, etc.  */

void * __attribute__ ((__noreturn__))
__builtin_saveregs (void)
{
  abort ();
}

#endif /* not __mips__ */
#endif /* not __sparc__ */
#endif /* not __i860__ */
#endif

#ifdef L_eprintf
#ifndef inhibit_libc

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
/* This is used by the `assert' macro.  */
void
__eprintf (const char *string, const char *expression,
	   unsigned int line, const char *filename)
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}

#endif
#endif

#ifdef L_bb

/* Structure emitted by -a  */
struct bb
{
  long zero_word;
  const char *filename;
  long *counts;
  long ncounts;
  struct bb *next;
  const unsigned long *addresses;

  /* Older GCC's did not emit these fields.  */
  long nwords;
  const char **functions;
  const long *line_nums;
  const char **filenames;
  char *flags;
};

#ifdef BLOCK_PROFILER_CODE
BLOCK_PROFILER_CODE
#else
#ifndef inhibit_libc

/* Simple minded basic block profiling output dumper for
   systems that don't provide tcov support.  At present,
   it requires atexit and stdio.  */

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
char *ctime ();

#include "gbl-ctors.h"
#include "gcov-io.h"
#include <string.h>
#ifdef TARGET_HAS_F_SETLKW
#include <fcntl.h>
#include <errno.h>
#endif

static struct bb *bb_head;

static int num_digits (long value, int base) __attribute__ ((const));

/* Return the number of digits needed to print a value */
/* __inline__ */ static int num_digits (long value, int base)
{
  int minus = (value < 0 && base != 16);
  unsigned long v = (minus) ? -value : value;
  int ret = minus;

  do
    {
      v /= base;
      ret++;
    }
  while (v);

  return ret;
}

void
__bb_exit_func (void)
{
  FILE *da_file, *file;
  long time_value;
  int i;

  if (bb_head == 0)
    return;

  i = strlen (bb_head->filename) - 3;

  if (!strcmp (bb_head->filename+i, ".da"))
    {
      /* Must be -fprofile-arcs not -a.
	 Dump data in a form that gcov expects.  */

      struct bb *ptr;

      for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
	{
	  int firstchar;

	  /* Make sure the output file exists -
	     but don't clobber exiting data.  */
	  if ((da_file = fopen (ptr->filename, "a")) != 0)
	    fclose (da_file);

	  /* Need to re-open in order to be able to write from the start.  */
	  da_file = fopen (ptr->filename, "r+b");
	  /* Some old systems might not allow the 'b' mode modifier.
	     Therefore, try to open without it.  This can lead to a race
	     condition so that when you delete and re-create the file, the
	     file might be opened in text mode, but then, you shouldn't
	     delete the file in the first place.  */
	  if (da_file == 0)
	    da_file = fopen (ptr->filename, "r+");
	  if (da_file == 0)
	    {
	      fprintf (stderr, "arc profiling: Can't open output file %s.\n",
		       ptr->filename);
	      continue;
	    }

	  /* After a fork, another process might try to read and/or write
	     the same file simultanously.  So if we can, lock the file to
	     avoid race conditions.  */
#if defined (TARGET_HAS_F_SETLKW)
	  {
	    struct flock s_flock;

	    s_flock.l_type = F_WRLCK;
	    s_flock.l_whence = SEEK_SET;
	    s_flock.l_start = 0;
	    s_flock.l_len = 1;
	    s_flock.l_pid = getpid ();

	    while (fcntl (fileno (da_file), F_SETLKW, &s_flock)
		   && errno == EINTR);
	  }
#endif

	  /* If the file is not empty, and the number of counts in it is the
	     same, then merge them in.  */
	  firstchar = fgetc (da_file);
	  if (firstchar == EOF)
	    {
	      if (ferror (da_file))
		{
		  fprintf (stderr, "arc profiling: Can't read output file ");
		  perror (ptr->filename);
		}
	    }
	  else
	    {
	      long n_counts = 0;
	      
	      if (ungetc (firstchar, da_file) == EOF)
		rewind (da_file);
	      if (__read_long (&n_counts, da_file, 8) != 0)
		{
		  fprintf (stderr, "arc profiling: Can't read output file %s.\n",
			   ptr->filename);
		  continue;
		}

	      if (n_counts == ptr->ncounts)
		{
		  int i;

		  for (i = 0; i < n_counts; i++)
		    {
		      long v = 0;

		      if (__read_long (&v, da_file, 8) != 0)
			{
			  fprintf (stderr, "arc profiling: Can't read output file %s.\n",
				   ptr->filename);
			  break;
			}
		      ptr->counts[i] += v;
		    }
		}

	    }

	  rewind (da_file);

	  /* ??? Should first write a header to the file.  Preferably, a 4 byte
	     magic number, 4 bytes containing the time the program was
	     compiled, 4 bytes containing the last modification time of the
	     source file, and 4 bytes indicating the compiler options used.

	     That way we can easily verify that the proper source/executable/
	     data file combination is being used from gcov.  */

	  if (__write_long (ptr->ncounts, da_file, 8) != 0)
	    {
	      
	      fprintf (stderr, "arc profiling: Error writing output file %s.\n",
		       ptr->filename);
	    }
	  else
	    {
	      int j;
	      long *count_ptr = ptr->counts;
	      int ret = 0;
	      for (j = ptr->ncounts; j > 0; j--)
		{
		  if (__write_long (*count_ptr, da_file, 8) != 0)
		    {
		      ret=1;
		      break;
		    }
		  count_ptr++;
		}
	      if (ret)
		fprintf (stderr, "arc profiling: Error writing output file %s.\n",
			 ptr->filename);
	    }
	  
	  if (fclose (da_file) == EOF)
	    fprintf (stderr, "arc profiling: Error closing output file %s.\n",
		     ptr->filename);
	}

      return;
    }

  /* Must be basic block profiling.  Emit a human readable output file.  */

  file = fopen ("bb.out", "a");

  if (!file)
    perror ("bb.out");

  else
    {
      struct bb *ptr;

      /* This is somewhat type incorrect, but it avoids worrying about
	 exactly where time.h is included from.  It should be ok unless
	 a void * differs from other pointer formats, or if sizeof (long)
	 is < sizeof (time_t).  It would be nice if we could assume the
	 use of rationale standards here.  */

      time ((void *) &time_value);
      fprintf (file, "Basic block profiling finished on %s\n", ctime ((void *) &time_value));

      /* We check the length field explicitly in order to allow compatibility
	 with older GCC's which did not provide it.  */

      for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
	{
	  int i;
	  int func_p	= (ptr->nwords >= (long) sizeof (struct bb)
			   && ptr->nwords <= 1000
			   && ptr->functions);
	  int line_p	= (func_p && ptr->line_nums);
	  int file_p	= (func_p && ptr->filenames);
	  int addr_p	= (ptr->addresses != 0);
	  long ncounts	= ptr->ncounts;
	  long cnt_max  = 0;
	  long line_max = 0;
	  long addr_max = 0;
	  int file_len	= 0;
	  int func_len	= 0;
	  int blk_len	= num_digits (ncounts, 10);
	  int cnt_len;
	  int line_len;
	  int addr_len;

	  fprintf (file, "File %s, %ld basic blocks \n\n",
		   ptr->filename, ncounts);

	  /* Get max values for each field.  */
	  for (i = 0; i < ncounts; i++)
	    {
	      const char *p;
	      int len;

	      if (cnt_max < ptr->counts[i])
		cnt_max = ptr->counts[i];

	      if (addr_p && (unsigned long) addr_max < ptr->addresses[i])
		addr_max = ptr->addresses[i];

	      if (line_p && line_max < ptr->line_nums[i])
		line_max = ptr->line_nums[i];

	      if (func_p)
		{
		  p = (ptr->functions[i]) ? (ptr->functions[i]) : "<none>";
		  len = strlen (p);
		  if (func_len < len)
		    func_len = len;
		}

	      if (file_p)
		{
		  p = (ptr->filenames[i]) ? (ptr->filenames[i]) : "<none>";
		  len = strlen (p);
		  if (file_len < len)
		    file_len = len;
		}
	    }

	  addr_len = num_digits (addr_max, 16);
	  cnt_len  = num_digits (cnt_max, 10);
	  line_len = num_digits (line_max, 10);

	  /* Now print out the basic block information.  */
	  for (i = 0; i < ncounts; i++)
	    {
	      fprintf (file,
		       "    Block #%*d: executed %*ld time(s)",
		       blk_len, i+1,
		       cnt_len, ptr->counts[i]);

	      if (addr_p)
		fprintf (file, " address= 0x%.*lx", addr_len,
			 ptr->addresses[i]);

	      if (func_p)
		fprintf (file, " function= %-*s", func_len,
			 (ptr->functions[i]) ? ptr->functions[i] : "<none>");

	      if (line_p)
		fprintf (file, " line= %*ld", line_len, ptr->line_nums[i]);

	      if (file_p)
		fprintf (file, " file= %s",
			 (ptr->filenames[i]) ? ptr->filenames[i] : "<none>");

	      fprintf (file, "\n");
	    }

	  fprintf (file, "\n");
	  fflush (file);
	}

      fprintf (file, "\n\n");
      fclose (file);
    }
}

void
__bb_init_func (struct bb *blocks)
{
  /* User is supposed to check whether the first word is non-0,
     but just in case....  */

  if (blocks->zero_word)
    return;

  /* Initialize destructor.  */
  if (!bb_head)
    atexit (__bb_exit_func);

  /* Set up linked list.  */
  blocks->zero_word = 1;
  blocks->next = bb_head;
  bb_head = blocks;
}

/* Called before fork or exec - write out profile information gathered so
   far and reset it to zero.  This avoids duplication or loss of the
   profile information gathered so far.  */
void
__bb_fork_func (void)
{
  struct bb *ptr;

  __bb_exit_func ();
  for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
    {
      long i;
      for (i = ptr->ncounts - 1; i >= 0; i--)
	ptr->counts[i] = 0;
    }
}

#ifndef MACHINE_STATE_SAVE
#define MACHINE_STATE_SAVE(ID)
#endif
#ifndef MACHINE_STATE_RESTORE
#define MACHINE_STATE_RESTORE(ID)
#endif

/* Number of buckets in hashtable of basic block addresses.  */

#define BB_BUCKETS 311

/* Maximum length of string in file bb.in.  */

#define BBINBUFSIZE 500

struct bb_edge
{
  struct bb_edge *next;
  unsigned long src_addr;
  unsigned long dst_addr;
  unsigned long count;
};

enum bb_func_mode
{
  TRACE_KEEP = 0, TRACE_ON = 1, TRACE_OFF = 2
};

struct bb_func
{
  struct bb_func *next;
  char *funcname;
  char *filename;
  enum bb_func_mode mode;
};

/* This is the connection to the outside world.
   The BLOCK_PROFILER macro must set __bb.blocks
   and __bb.blockno.  */

struct {
  unsigned long blockno;
  struct bb *blocks;
} __bb;

/* Vars to store addrs of source and destination basic blocks 
   of a jump.  */

static unsigned long bb_src = 0;
static unsigned long bb_dst = 0;

static FILE *bb_tracefile = (FILE *) 0;
static struct bb_edge **bb_hashbuckets = (struct bb_edge **) 0;
static struct bb_func *bb_func_head = (struct bb_func *) 0;
static unsigned long bb_callcount = 0;
static int bb_mode = 0;

static unsigned long *bb_stack = (unsigned long *) 0;
static size_t bb_stacksize = 0;

static int reported = 0;

/* Trace modes:
Always             :   Print execution frequencies of basic blocks
                       to file bb.out.
bb_mode & 1 != 0   :   Dump trace of basic blocks to file bbtrace[.gz]
bb_mode & 2 != 0   :   Print jump frequencies to file bb.out.
bb_mode & 4 != 0   :   Cut call instructions from basic block flow.
bb_mode & 8 != 0   :   Insert return instructions in basic block flow.
*/

#ifdef HAVE_POPEN

/*#include <sys/types.h>*/
#include <sys/stat.h>
/*#include <malloc.h>*/

/* Commands executed by gopen.  */

#define GOPENDECOMPRESS "gzip -cd "
#define GOPENCOMPRESS "gzip -c >"

/* Like fopen but pipes through gzip.  mode may only be "r" or "w".
   If it does not compile, simply replace gopen by fopen and delete
   '.gz' from any first parameter to gopen.  */

static FILE *
gopen (char *fn, char *mode)
{
  int use_gzip;
  char *p;

  if (mode[1])
    return (FILE *) 0;

  if (mode[0] != 'r' && mode[0] != 'w') 
    return (FILE *) 0;

  p = fn + strlen (fn)-1;
  use_gzip = ((p[-1] == '.' && (p[0] == 'Z' || p[0] == 'z'))
	      || (p[-2] == '.' && p[-1] == 'g' && p[0] == 'z'));

  if (use_gzip)
    {
      if (mode[0]=='r')
        {
          FILE *f;
          char *s = (char *) malloc (sizeof (char) * strlen (fn)
				     + sizeof (GOPENDECOMPRESS));
          strcpy (s, GOPENDECOMPRESS);
          strcpy (s + (sizeof (GOPENDECOMPRESS)-1), fn);
          f = popen (s, mode);
          free (s);
          return f;
        }

      else
        {
          FILE *f;
          char *s = (char *) malloc (sizeof (char) * strlen (fn)
				     + sizeof (GOPENCOMPRESS));
          strcpy (s, GOPENCOMPRESS);
          strcpy (s + (sizeof (GOPENCOMPRESS)-1), fn);
          if (!(f = popen (s, mode)))
            f = fopen (s, mode);
          free (s);
          return f;
        }
    }

  else
    return fopen (fn, mode);
}

static int
gclose (FILE *f)
{
  struct stat buf;

  if (f != 0)
    {
      if (!fstat (fileno (f), &buf) && S_ISFIFO (buf.st_mode))
        return pclose (f);

      return fclose (f);
    }
  return 0;
}

#endif /* HAVE_POPEN */

/* Called once per program.  */

static void
__bb_exit_trace_func (void)
{
  FILE *file = fopen ("bb.out", "a");
  struct bb_func *f;
  struct bb *b;
        
  if (!file)
    perror ("bb.out");

  if (bb_mode & 1)
    {
      if (!bb_tracefile)
        perror ("bbtrace");
      else
#ifdef HAVE_POPEN
        gclose (bb_tracefile);
#else
        fclose (bb_tracefile);
#endif /* HAVE_POPEN */
    }

  /* Check functions in `bb.in'.  */

  if (file)
    {
      long time_value;
      const struct bb_func *p;
      int printed_something = 0;
      struct bb *ptr;
      long blk;

      /* This is somewhat type incorrect.  */
      time ((void *) &time_value);

      for (p = bb_func_head; p != (struct bb_func *) 0; p = p->next)
        {
          for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
            {
              if (!ptr->filename || (p->filename != (char *) 0 && strcmp (p->filename, ptr->filename)))
                continue;
              for (blk = 0; blk < ptr->ncounts; blk++)
                {
                  if (!strcmp (p->funcname, ptr->functions[blk]))
                    goto found;
                }
            }
  
          if (!printed_something)
            {
              fprintf (file, "Functions in `bb.in' not executed during basic block profiling on %s\n", ctime ((void *) &time_value));
              printed_something = 1;
            }

          fprintf (file, "\tFunction %s", p->funcname);
          if (p->filename)
              fprintf (file, " of file %s", p->filename);
          fprintf (file, "\n" );
  
found:        ;
        }

      if (printed_something)
       fprintf (file, "\n");

    }

  if (bb_mode & 2)
    {
      if (!bb_hashbuckets)
        {
          if (!reported)
            {
              fprintf (stderr, "Profiler: out of memory\n");
              reported = 1;
            }
          return;
        }
    
      else if (file)
        {
          long time_value;
          int i;
          unsigned long addr_max = 0;
          unsigned long cnt_max  = 0;
          int cnt_len;
          int addr_len;
    
          /* This is somewhat type incorrect, but it avoids worrying about
             exactly where time.h is included from.  It should be ok unless
             a void * differs from other pointer formats, or if sizeof (long)
             is < sizeof (time_t).  It would be nice if we could assume the
             use of rationale standards here.  */
    
          time ((void *) &time_value);
          fprintf (file, "Basic block jump tracing");

          switch (bb_mode & 12)
            {
              case 0:
                fprintf (file, " (with call)");
              break;

              case 4:
		/* Print nothing.  */
              break;

              case 8:
                fprintf (file, " (with call & ret)");
              break;

              case 12:
                fprintf (file, " (with ret)");
              break;
            }

          fprintf (file, " finished on %s\n", ctime ((void *) &time_value));
    
          for (i = 0; i < BB_BUCKETS; i++)
            {
               struct bb_edge *bucket = bb_hashbuckets[i];
               for ( ; bucket; bucket = bucket->next )
                 {
                   if (addr_max < bucket->src_addr) 
                     addr_max = bucket->src_addr;
                   if (addr_max < bucket->dst_addr) 
                     addr_max = bucket->dst_addr;
                   if (cnt_max < bucket->count) 
                     cnt_max = bucket->count;
                 }
            }
          addr_len = num_digits (addr_max, 16);
          cnt_len  = num_digits (cnt_max, 10);
    
          for ( i = 0; i < BB_BUCKETS; i++)
            {
               struct bb_edge *bucket = bb_hashbuckets[i];
               for ( ; bucket; bucket = bucket->next )
                 {
                   fprintf (file,
	"Jump from block 0x%.*lx to block 0x%.*lx executed %*lu time(s)\n", 
                            addr_len, bucket->src_addr, 
                            addr_len, bucket->dst_addr, 
                            cnt_len, bucket->count);
                 }
            }
  
          fprintf (file, "\n");

        }
    }

   if (file)
     fclose (file);

   /* Free allocated memory.  */

   f = bb_func_head;
   while (f)
     {
       struct bb_func *old = f;

       f = f->next;
       if (old->funcname) free (old->funcname);
       if (old->filename) free (old->filename);
       free (old);
     }

   if (bb_stack)
     free (bb_stack);

   if (bb_hashbuckets)
     {
       int i;

       for (i = 0; i < BB_BUCKETS; i++)
         {
           struct bb_edge *old, *bucket = bb_hashbuckets[i];

           while (bucket)
             {
               old = bucket;
               bucket = bucket->next;
               free (old);
             }
         }
       free (bb_hashbuckets);
     }

   for (b = bb_head; b; b = b->next)
     if (b->flags) free (b->flags);
}

/* Called once per program.  */

static void
__bb_init_prg (void)
{
  FILE *file;
  char buf[BBINBUFSIZE];
  const char *p;
  const char *pos;
  enum bb_func_mode m;
  int i;

  /* Initialize destructor.  */
  atexit (__bb_exit_func);

  if (!(file = fopen ("bb.in", "r")))
    return;

  while(fgets (buf, BBINBUFSIZE, file) != 0)
    {
      i = strlen (buf);
      if (buf[i] == '\n')
	buf[i--] = '\0';

      p = buf;
      if (*p == '-') 
        { 
          m = TRACE_OFF; 
          p++; 
        }
      else 
        { 
          m = TRACE_ON; 
        }
      if (!strcmp (p, "__bb_trace__"))
        bb_mode |= 1;
      else if (!strcmp (p, "__bb_jumps__"))
        bb_mode |= 2;
      else if (!strcmp (p, "__bb_hidecall__"))
        bb_mode |= 4;
      else if (!strcmp (p, "__bb_showret__"))
        bb_mode |= 8;
      else 
        {
          struct bb_func *f = (struct bb_func *) malloc (sizeof (struct bb_func));
          if (f)
            {
              unsigned long l;
              f->next = bb_func_head;
              if ((pos = strchr (p, ':')))
                {
                  if (!(f->funcname = (char *) malloc (strlen (pos+1)+1)))
                    continue;
                  strcpy (f->funcname, pos+1);
                  l = pos-p;
                  if ((f->filename = (char *) malloc (l+1)))
                    {
                      strncpy (f->filename, p, l);
                      f->filename[l] = '\0';
                    }
                  else
                    f->filename = (char *) 0;
                }
              else
                {
                  if (!(f->funcname = (char *) malloc (strlen (p)+1)))
                    continue;
                  strcpy (f->funcname, p);
                  f->filename = (char *) 0;
                }
              f->mode = m;
              bb_func_head = f;
	    }
         }
    }
  fclose (file);

#ifdef HAVE_POPEN 

  if (bb_mode & 1)
      bb_tracefile = gopen ("bbtrace.gz", "w");

#else

  if (bb_mode & 1)
      bb_tracefile = fopen ("bbtrace", "w");

#endif /* HAVE_POPEN */

  if (bb_mode & 2)
    {
      bb_hashbuckets = (struct bb_edge **) 
                   malloc (BB_BUCKETS * sizeof (struct bb_edge *));
      if (bb_hashbuckets)
	/* Use a loop here rather than calling bzero to avoid having to
	   conditionalize its existance.  */
	for (i = 0; i < BB_BUCKETS; i++)
	  bb_hashbuckets[i] = 0;
    }

  if (bb_mode & 12)
    {
      bb_stacksize = 10;
      bb_stack = (unsigned long *) malloc (bb_stacksize * sizeof (*bb_stack));
    }

  /* Initialize destructor.  */
  atexit (__bb_exit_trace_func);
}

/* Called upon entering a basic block.  */

void
__bb_trace_func (void)
{
  struct bb_edge *bucket;

  MACHINE_STATE_SAVE("1")

  if (!bb_callcount || (__bb.blocks->flags && (__bb.blocks->flags[__bb.blockno] & TRACE_OFF)))
    goto skip;

  bb_dst = __bb.blocks->addresses[__bb.blockno];
  __bb.blocks->counts[__bb.blockno]++;

  if (bb_tracefile)
    {
      fwrite (&bb_dst, sizeof (unsigned long), 1, bb_tracefile);
    }

  if (bb_hashbuckets)
    {
      struct bb_edge **startbucket, **oldnext;

      oldnext = startbucket
	= & bb_hashbuckets[ (((int) bb_src*8) ^ (int) bb_dst) % BB_BUCKETS ];
      bucket = *startbucket;

      for (bucket = *startbucket; bucket; 
           oldnext = &(bucket->next), bucket = *oldnext)
        {
          if (bucket->src_addr == bb_src
	      && bucket->dst_addr == bb_dst)
            {
              bucket->count++;
              *oldnext = bucket->next;
              bucket->next = *startbucket;
              *startbucket = bucket;
              goto ret;
            }
        }

      bucket = (struct bb_edge *) malloc (sizeof (struct bb_edge));

      if (!bucket)
        {
          if (!reported)
            {
              fprintf (stderr, "Profiler: out of memory\n");
              reported = 1;
            }
        }

      else
        {
          bucket->src_addr = bb_src;
          bucket->dst_addr = bb_dst;
          bucket->next = *startbucket;
          *startbucket = bucket;
          bucket->count = 1;
        }
    }

ret:
  bb_src = bb_dst;

skip:
  ;

  MACHINE_STATE_RESTORE("1")

}

/* Called when returning from a function and `__bb_showret__' is set.  */

static void
__bb_trace_func_ret (void)
{
  struct bb_edge *bucket;

  if (!bb_callcount || (__bb.blocks->flags && (__bb.blocks->flags[__bb.blockno] & TRACE_OFF)))
    goto skip;

  if (bb_hashbuckets)
    {
      struct bb_edge **startbucket, **oldnext;

      oldnext = startbucket
	= & bb_hashbuckets[ (((int) bb_dst * 8) ^ (int) bb_src) % BB_BUCKETS ];
      bucket = *startbucket;

      for (bucket = *startbucket; bucket; 
           oldnext = &(bucket->next), bucket = *oldnext)
        {
          if (bucket->src_addr == bb_dst
	       && bucket->dst_addr == bb_src)
            {
              bucket->count++;
              *oldnext = bucket->next;
              bucket->next = *startbucket;
              *startbucket = bucket;
              goto ret;
            }
        }

      bucket = (struct bb_edge *) malloc (sizeof (struct bb_edge));

      if (!bucket)
        {
          if (!reported)
            {
              fprintf (stderr, "Profiler: out of memory\n");
              reported = 1;
            }
        }

      else
        {
          bucket->src_addr = bb_dst;
          bucket->dst_addr = bb_src;
          bucket->next = *startbucket;
          *startbucket = bucket;
          bucket->count = 1;
        }
    }

ret:
  bb_dst = bb_src;

skip:
  ;

}

/* Called upon entering the first function of a file.  */

static void
__bb_init_file (struct bb *blocks)
{

  const struct bb_func *p;
  long blk, ncounts = blocks->ncounts;
  const char **functions = blocks->functions;

  /* Set up linked list.  */
  blocks->zero_word = 1;
  blocks->next = bb_head;
  bb_head = blocks;

  blocks->flags = 0;
  if (!bb_func_head
      || !(blocks->flags = (char *) malloc (sizeof (char) * blocks->ncounts)))
    return;

  for (blk = 0; blk < ncounts; blk++)
    blocks->flags[blk] = 0;

  for (blk = 0; blk < ncounts; blk++)
    {
      for (p = bb_func_head; p; p = p->next)
        {
          if (!strcmp (p->funcname, functions[blk])
	      && (!p->filename || !strcmp (p->filename, blocks->filename)))
            {
              blocks->flags[blk] |= p->mode;
            }
        }
    }

}

/* Called when exiting from a function.  */

void
__bb_trace_ret (void)
{

  MACHINE_STATE_SAVE("2")

  if (bb_callcount)
    {
      if ((bb_mode & 12) && bb_stacksize > bb_callcount)
        {
          bb_src = bb_stack[bb_callcount];
          if (bb_mode & 8)
            __bb_trace_func_ret ();
        }

      bb_callcount -= 1;
    }

  MACHINE_STATE_RESTORE("2")

}

/* Called when entering a function.  */

void
__bb_init_trace_func (struct bb *blocks, unsigned long blockno)
{
  static int trace_init = 0;

  MACHINE_STATE_SAVE("3")

  if (!blocks->zero_word)
    { 
      if (!trace_init)
        { 
          trace_init = 1;
          __bb_init_prg ();
        }
      __bb_init_file (blocks);
    }

  if (bb_callcount)
    {

      bb_callcount += 1;

      if (bb_mode & 12)
        {
          if (bb_callcount >= bb_stacksize)
            {
              size_t newsize = bb_callcount + 100;

              bb_stack = (unsigned long *) realloc (bb_stack, newsize);
              if (! bb_stack)
                {
                  if (!reported)
                    {
                      fprintf (stderr, "Profiler: out of memory\n");
                      reported = 1;
                    }
                  bb_stacksize = 0;
                  goto stack_overflow;
                }
	      bb_stacksize = newsize;
            }
          bb_stack[bb_callcount] = bb_src;

          if (bb_mode & 4)
            bb_src = 0;

        }

stack_overflow:;

    }

  else if (blocks->flags && (blocks->flags[blockno] & TRACE_ON))
    {
      bb_callcount = 1;
      bb_src = 0;

      if (bb_stack)
          bb_stack[bb_callcount] = bb_src;
    }

  MACHINE_STATE_RESTORE("3")
}

#endif /* not inhibit_libc */
#endif /* not BLOCK_PROFILER_CODE */
#endif /* L_bb */

#ifdef L_shtab
unsigned int __shtab[] = {
    0x00000001, 0x00000002, 0x00000004, 0x00000008,
    0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800,
    0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000,
    0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000,
    0x10000000, 0x20000000, 0x40000000, 0x80000000
  };
#endif

#ifdef L_clear_cache
/* Clear part of an instruction cache.  */

#define INSN_CACHE_PLANE_SIZE (INSN_CACHE_SIZE / INSN_CACHE_DEPTH)

void
__clear_cache (char *beg __attribute__((__unused__)),
	       char *end __attribute__((__unused__)))
{
#ifdef CLEAR_INSN_CACHE 
  CLEAR_INSN_CACHE (beg, end);
#else
#ifdef INSN_CACHE_SIZE
  static char array[INSN_CACHE_SIZE + INSN_CACHE_PLANE_SIZE + INSN_CACHE_LINE_WIDTH];
  static int initialized;
  int offset;
  void *start_addr
  void *end_addr;
  typedef (*function_ptr) (void);

#if (INSN_CACHE_SIZE / INSN_CACHE_LINE_WIDTH) < 16
  /* It's cheaper to clear the whole cache.
     Put in a series of jump instructions so that calling the beginning
     of the cache will clear the whole thing.  */

  if (! initialized)
    {
      int ptr = (((int) array + INSN_CACHE_LINE_WIDTH - 1)
		 & -INSN_CACHE_LINE_WIDTH);
      int end_ptr = ptr + INSN_CACHE_SIZE;

      while (ptr < end_ptr)
	{
	  *(INSTRUCTION_TYPE *)ptr
	    = JUMP_AHEAD_INSTRUCTION + INSN_CACHE_LINE_WIDTH;
	  ptr += INSN_CACHE_LINE_WIDTH;
	}
      *(INSTRUCTION_TYPE *) (ptr - INSN_CACHE_LINE_WIDTH) = RETURN_INSTRUCTION;

      initialized = 1;
    }

  /* Call the beginning of the sequence.  */
  (((function_ptr) (((int) array + INSN_CACHE_LINE_WIDTH - 1)
		    & -INSN_CACHE_LINE_WIDTH))
   ());

#else /* Cache is large.  */

  if (! initialized)
    {
      int ptr = (((int) array + INSN_CACHE_LINE_WIDTH - 1)
		 & -INSN_CACHE_LINE_WIDTH);

      while (ptr < (int) array + sizeof array)
	{
	  *(INSTRUCTION_TYPE *)ptr = RETURN_INSTRUCTION;
	  ptr += INSN_CACHE_LINE_WIDTH;
	}

      initialized = 1;
    }

  /* Find the location in array that occupies the same cache line as BEG.  */

  offset = ((int) beg & -INSN_CACHE_LINE_WIDTH) & (INSN_CACHE_PLANE_SIZE - 1);
  start_addr = (((int) (array + INSN_CACHE_PLANE_SIZE - 1)
		 & -INSN_CACHE_PLANE_SIZE)
		+ offset);

  /* Compute the cache alignment of the place to stop clearing.  */
#if 0  /* This is not needed for gcc's purposes.  */
  /* If the block to clear is bigger than a cache plane,
     we clear the entire cache, and OFFSET is already correct.  */ 
  if (end < beg + INSN_CACHE_PLANE_SIZE)
#endif
    offset = (((int) (end + INSN_CACHE_LINE_WIDTH - 1)
	       & -INSN_CACHE_LINE_WIDTH)
	      & (INSN_CACHE_PLANE_SIZE - 1));

#if INSN_CACHE_DEPTH > 1
  end_addr = (start_addr & -INSN_CACHE_PLANE_SIZE) + offset;
  if (end_addr <= start_addr)
    end_addr += INSN_CACHE_PLANE_SIZE;

  for (plane = 0; plane < INSN_CACHE_DEPTH; plane++)
    {
      int addr = start_addr + plane * INSN_CACHE_PLANE_SIZE;
      int stop = end_addr + plane * INSN_CACHE_PLANE_SIZE;

      while (addr != stop)
	{
	  /* Call the return instruction at ADDR.  */
	  ((function_ptr) addr) ();

	  addr += INSN_CACHE_LINE_WIDTH;
	}
    }
#else /* just one plane */
  do
    {
      /* Call the return instruction at START_ADDR.  */
      ((function_ptr) start_addr) ();

      start_addr += INSN_CACHE_LINE_WIDTH;
    }
  while ((start_addr % INSN_CACHE_SIZE) != offset);
#endif /* just one plane */
#endif /* Cache is large */
#endif /* Cache exists */
#endif /* CLEAR_INSN_CACHE */
}

#endif /* L_clear_cache */

#ifdef L_trampoline

/* Jump to a trampoline, loading the static chain address.  */

#if defined(WINNT) && ! defined(__CYGWIN__) && ! defined (_UWIN)

long
getpagesize (void)
{
#ifdef _ALPHA_
  return 8192;
#else
  return 4096;
#endif
}

#ifdef __i386__
extern int VirtualProtect (char *, int, int, int *) __attribute__((stdcall));
#endif

int
mprotect (char *addr, int len, int prot)
{
  int np, op;

  if (prot == 7)
    np = 0x40;
  else if (prot == 5)
    np = 0x20;
  else if (prot == 4)
    np = 0x10;
  else if (prot == 3)
    np = 0x04;
  else if (prot == 1)
    np = 0x02;
  else if (prot == 0)
    np = 0x01;

  if (VirtualProtect (addr, len, np, &op))
    return 0;
  else
    return -1;
}

#endif /* WINNT && ! __CYGWIN__ && ! _UWIN */

#ifdef TRANSFER_FROM_TRAMPOLINE 
TRANSFER_FROM_TRAMPOLINE 
#endif

#if defined (NeXT) && defined (__MACH__)

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in next.h.  */
#ifdef NeXTStep21
 #include <mach.h>
#else
 #include <mach/mach.h>
#endif

void
__enable_execute_stack (char *addr)
{
  kern_return_t r;
  char *eaddr = addr + TRAMPOLINE_SIZE;
  vm_address_t a = (vm_address_t) addr;

  /* turn on execute access on stack */
  r = vm_protect (task_self (), a, TRAMPOLINE_SIZE, FALSE, VM_PROT_ALL);
  if (r != KERN_SUCCESS)
    {
      mach_error("vm_protect VM_PROT_ALL", r);
      exit(1);
    }

  /* We inline the i-cache invalidation for speed */

#ifdef CLEAR_INSN_CACHE
  CLEAR_INSN_CACHE (addr, eaddr);
#else
  __clear_cache ((int) addr, (int) eaddr);
#endif
} 

#endif /* defined (NeXT) && defined (__MACH__) */

#ifdef __convex__

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in convex.h.  */

#include <sys/mman.h>
#include <sys/vmparam.h>
#include <machine/machparam.h>

void
__enable_execute_stack (void)
{
  int fp;
  static unsigned lowest = USRSTACK;
  unsigned current = (unsigned) &fp & -NBPG;

  if (lowest > current)
    {
      unsigned len = lowest - current;
      mremap (current, &len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE);
      lowest = current;
    }

  /* Clear instruction cache in case an old trampoline is in it.  */
  asm ("pich");
}
#endif /* __convex__ */

#ifdef __sysV88__

/* Modified from the convex -code above.  */

#include <sys/param.h>
#include <errno.h>
#include <sys/m88kbcs.h>

void
__enable_execute_stack (void)
{
  int save_errno;
  static unsigned long lowest = USRSTACK;
  unsigned long current = (unsigned long) &save_errno & -NBPC;
  
  /* Ignore errno being set. memctl sets errno to EINVAL whenever the
     address is seen as 'negative'. That is the case with the stack.   */

  save_errno=errno;
  if (lowest > current)
    {
      unsigned len=lowest-current;
      memctl(current,len,MCT_TEXT);
      lowest = current;
    }
  else
    memctl(current,NBPC,MCT_TEXT);
  errno=save_errno;
}

#endif /* __sysV88__ */

#ifdef __sysV68__

#include <sys/signal.h>
#include <errno.h>

/* Motorola forgot to put memctl.o in the libp version of libc881.a,
   so define it here, because we need it in __clear_insn_cache below */
/* On older versions of this OS, no memctl or MCT_TEXT are defined;
   hence we enable this stuff only if MCT_TEXT is #define'd.  */

#ifdef MCT_TEXT
asm("\n\
	global memctl\n\
memctl:\n\
	movq &75,%d0\n\
	trap &0\n\
	bcc.b noerror\n\
	jmp cerror%\n\
noerror:\n\
	movq &0,%d0\n\
	rts");
#endif

/* Clear instruction cache so we can call trampolines on stack.
   This is called from FINALIZE_TRAMPOLINE in mot3300.h.  */

void
__clear_insn_cache (void)
{
#ifdef MCT_TEXT
  int save_errno;

  /* Preserve errno, because users would be surprised to have
  errno changing without explicitly calling any system-call. */
  save_errno = errno;

  /* Keep it simple : memctl (MCT_TEXT) always fully clears the insn cache. 
     No need to use an address derived from _start or %sp, as 0 works also. */
  memctl(0, 4096, MCT_TEXT);
  errno = save_errno;
#endif
}

#endif /* __sysV68__ */

#ifdef __pyr__

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/vmmac.h>

/* Modified from the convex -code above.
   mremap promises to clear the i-cache.  */

void
__enable_execute_stack (void)
{
  int fp;
  if (mprotect (((unsigned int)&fp/PAGSIZ)*PAGSIZ, PAGSIZ,
		PROT_READ|PROT_WRITE|PROT_EXEC))
    {
      perror ("mprotect in __enable_execute_stack");
      fflush (stderr);
      abort ();
    }
}
#endif /* __pyr__ */

#if defined (sony_news) && defined (SYSTYPE_BSD)

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <syscall.h>
#include <machine/sysnews.h>

/* cacheflush function for NEWS-OS 4.2.
   This function is called from trampoline-initialize code
   defined in config/mips/mips.h.  */

void
cacheflush (char *beg, int size, int flag)
{
  if (syscall (SYS_sysnews, NEWS_CACHEFLUSH, beg, size, FLUSH_BCACHE))
    {
      perror ("cache_flush");
      fflush (stderr);
      abort ();
    }
}

#endif /* sony_news */
#endif /* L_trampoline */

#ifndef __CYGWIN__
#ifdef L__main

#include "gbl-ctors.h"
/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#define SYMBOL__MAIN __main
#endif

#ifdef INIT_SECTION_ASM_OP
#undef HAS_INIT_SECTION
#define HAS_INIT_SECTION
#endif

#if !defined (HAS_INIT_SECTION) || !defined (OBJECT_FORMAT_ELF)

/* Some ELF crosses use crtstuff.c to provide __CTOR_LIST__, but use this
   code to run constructors.  In that case, we need to handle EH here, too.  */

#ifdef EH_FRAME_SECTION
#include "frame.h"
extern unsigned char __EH_FRAME_BEGIN__[];
#endif

/* Run all the global destructors on exit from the program.  */

void
__do_global_dtors (void)
{
#ifdef DO_GLOBAL_DTORS_BODY
  DO_GLOBAL_DTORS_BODY;
#else
  static func_ptr *p = __DTOR_LIST__ + 1;
  while (*p)
    {
      p++;
      (*(p-1)) ();
    }
#endif
#if defined (EH_FRAME_SECTION) && !defined (HAS_INIT_SECTION)
  {
    static int completed = 0;
    if (! completed)
      {
	completed = 1;
	__deregister_frame_info (__EH_FRAME_BEGIN__);
      }
  }
#endif
}
#endif

#ifndef HAS_INIT_SECTION
/* Run all the global constructors on entry to the program.  */

void
__do_global_ctors (void)
{
#ifdef EH_FRAME_SECTION
  {
    static struct object object;
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
  }
#endif
  DO_GLOBAL_CTORS_BODY;
  atexit (__do_global_dtors);
}
#endif /* no HAS_INIT_SECTION */

#if !defined (HAS_INIT_SECTION) || defined (INVOKE__main)
/* Subroutine called automatically by `main'.
   Compiling a global function named `main'
   produces an automatic call to this function at the beginning.

   For many systems, this routine calls __do_global_ctors.
   For systems which support a .init section we use the .init section
   to run __do_global_ctors, so we need not do anything here.  */

void
SYMBOL__MAIN ()
{
  /* Support recursive calls to `main': run initializers just once.  */
  static int initialized;
  if (! initialized)
    {
      initialized = 1;
      __do_global_ctors ();
    }
}
#endif /* no HAS_INIT_SECTION or INVOKE__main */

#endif /* L__main */
#endif /* __CYGWIN__ */

#ifdef L_ctors

#include "gbl-ctors.h"

/* Provide default definitions for the lists of constructors and
   destructors, so that we don't get linker errors.  These symbols are
   intentionally bss symbols, so that gld and/or collect will provide
   the right values.  */

/* We declare the lists here with two elements each,
   so that they are valid empty lists if no other definition is loaded.

   If we are using the old "set" extensions to have the gnu linker
   collect ctors and dtors, then we __CTOR_LIST__ and __DTOR_LIST__
   must be in the bss/common section.

   Long term no port should use those extensions.  But many still do.  */
#if !defined(INIT_SECTION_ASM_OP) && !defined(CTOR_LISTS_DEFINED_EXTERNALLY)
#if defined (ASM_OUTPUT_CONSTRUCTOR) || defined (USE_COLLECT2)
func_ptr __CTOR_LIST__[2] = {0, 0};
func_ptr __DTOR_LIST__[2] = {0, 0};
#else
func_ptr __CTOR_LIST__[2];
func_ptr __DTOR_LIST__[2];
#endif
#endif /* no INIT_SECTION_ASM_OP and not CTOR_LISTS_DEFINED_EXTERNALLY */
#endif /* L_ctors */

#ifdef L_exit

#include "gbl-ctors.h"

#ifdef NEED_ATEXIT

#ifndef ON_EXIT

# include <errno.h>

static func_ptr *atexit_chain = 0;
static long atexit_chain_length = 0;
static volatile long last_atexit_chain_slot = -1;

int
atexit (func_ptr func)
{
  if (++last_atexit_chain_slot == atexit_chain_length)
    {
      atexit_chain_length += 32;
      if (atexit_chain)
	atexit_chain = (func_ptr *) realloc (atexit_chain, atexit_chain_length
					     * sizeof (func_ptr));
      else
	atexit_chain = (func_ptr *) malloc (atexit_chain_length
					    * sizeof (func_ptr));
      if (! atexit_chain)
	{
	  atexit_chain_length = 0;
	  last_atexit_chain_slot = -1;
	  errno = ENOMEM;
	  return (-1);
	}
    }
  atexit_chain[last_atexit_chain_slot] = func;
  return (0);
}

extern void _cleanup (void);
extern void _exit (int) __attribute__ ((__noreturn__));

void 
exit (int status)
{
  if (atexit_chain)
    {
      for ( ; last_atexit_chain_slot-- >= 0; )
	{
	  (*atexit_chain[last_atexit_chain_slot + 1]) ();
	  atexit_chain[last_atexit_chain_slot + 1] = 0;
	}
      free (atexit_chain);
      atexit_chain = 0;
    }
#ifdef EXIT_BODY
  EXIT_BODY;
#else
  _cleanup ();
#endif
  _exit (status);
}

#else /* ON_EXIT */

/* Simple; we just need a wrapper for ON_EXIT.  */
int
atexit (func_ptr func)
{
  return ON_EXIT (func);
}

#endif /* ON_EXIT */
#endif /* NEED_ATEXIT */

#endif /* L_exit */

#ifdef L_eh

#include "gthr.h"

/* Shared exception handling support routines.  */

void
__default_terminate (void)
{
  abort ();
}

void (*__terminate_func)(void) __attribute__ ((__noreturn__)) =
  __default_terminate;

void
__terminate (void)
{
  (*__terminate_func)();
}

void *
__throw_type_match (void *catch_type, void *throw_type, void *obj)
{
#if 0
 printf ("__throw_type_match (): catch_type = %s, throw_type = %s\n",
	 catch_type, throw_type);
#endif
 if (strcmp ((const char *)catch_type, (const char *)throw_type) == 0)
   return obj;
 return 0;
}

void
__empty (void)
{
}


/* Include definitions of EH context and table layout */

#include "eh-common.h"
#ifndef inhibit_libc
#include <stdio.h>
#endif

/* Allocate and return a new EH context structure. */

#if __GTHREADS
static void *
new_eh_context (void)
{
  struct eh_full_context {
    struct eh_context c;
    void *top_elt[2];
  } *ehfc = (struct eh_full_context *) malloc (sizeof *ehfc);

  if (! ehfc)
    __terminate ();

  memset (ehfc, 0, sizeof *ehfc);

  ehfc->c.dynamic_handler_chain = (void **) ehfc->top_elt;

  /* This should optimize out entirely.  This should always be true,
     but just in case it ever isn't, don't allow bogus code to be
     generated.  */

  if ((void*)(&ehfc->c) != (void*)ehfc)
    __terminate ();

  return &ehfc->c;
}

static __gthread_key_t eh_context_key;

/* Destructor for struct eh_context. */
static void
eh_context_free (void *ptr)
{
  __gthread_key_dtor (eh_context_key, ptr);
  if (ptr)
    free (ptr);
}
#endif

/* Pointer to function to return EH context. */

static struct eh_context *eh_context_initialize (void);
static struct eh_context *eh_context_static (void);
#if __GTHREADS
static struct eh_context *eh_context_specific (void);
#endif

static struct eh_context *(*get_eh_context) (void) = &eh_context_initialize;

/* Routine to get EH context.
   This one will simply call the function pointer. */

void *
__get_eh_context (void)
{
  return (void *) (*get_eh_context) ();
}

/* Get and set the language specific info pointer. */

void **
__get_eh_info (void)
{
  struct eh_context *eh = (*get_eh_context) ();
  return &eh->info;
}

#ifdef DWARF2_UNWIND_INFO
static int dwarf_reg_size_table_initialized = 0;
static char dwarf_reg_size_table[DWARF_FRAME_REGISTERS];

static void
init_reg_size_table (void)
{
  __builtin_init_dwarf_reg_size_table (dwarf_reg_size_table);
  dwarf_reg_size_table_initialized = 1;
}
#endif

#if __GTHREADS
static void
eh_threads_initialize (void)
{
  /* Try to create the key.  If it fails, revert to static method,
     otherwise start using thread specific EH contexts. */
  if (__gthread_key_create (&eh_context_key, &eh_context_free) == 0)
    get_eh_context = &eh_context_specific;
  else
    get_eh_context = &eh_context_static;
}
#endif /* no __GTHREADS */

/* Initialize EH context.
   This will be called only once, since we change GET_EH_CONTEXT
   pointer to another routine. */

static struct eh_context *
eh_context_initialize (void)
{
#if __GTHREADS

  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  /* Make sure that get_eh_context does not point to us anymore.
     Some systems have dummy thread routines in their libc that
     return a success (Solaris 2.6 for example). */
  if (__gthread_once (&once, eh_threads_initialize) != 0
      || get_eh_context == &eh_context_initialize)
    {
      /* Use static version of EH context. */
      get_eh_context = &eh_context_static;
    }
#ifdef DWARF2_UNWIND_INFO
  {
    static __gthread_once_t once_regsizes = __GTHREAD_ONCE_INIT;
    if (__gthread_once (&once_regsizes, init_reg_size_table) != 0
	|| ! dwarf_reg_size_table_initialized)
      init_reg_size_table ();
  }
#endif

#else /* no __GTHREADS */

  /* Use static version of EH context. */
  get_eh_context = &eh_context_static;

#ifdef DWARF2_UNWIND_INFO
  init_reg_size_table ();
#endif

#endif /* no __GTHREADS */

  return (*get_eh_context) ();
}

/* Return a static EH context. */

static struct eh_context *
eh_context_static (void)
{
  static struct eh_context eh;
  static int initialized;
  static void *top_elt[2];

  if (! initialized)
    {
      initialized = 1;
      memset (&eh, 0, sizeof eh);
      eh.dynamic_handler_chain = top_elt;
    }
  return &eh;
}

#if __GTHREADS
/* Return a thread specific EH context. */

static struct eh_context *
eh_context_specific (void)
{
  struct eh_context *eh;
  eh = (struct eh_context *) __gthread_getspecific (eh_context_key);
  if (! eh)
    {
      eh = new_eh_context ();
      if (__gthread_setspecific (eh_context_key, (void *) eh) != 0)
	__terminate ();
    }

  return eh;
}
#endif __GTHREADS

/* Support routines for setjmp/longjmp exception handling.  */

/* Calls to __sjthrow are generated by the compiler when an exception
   is raised when using the setjmp/longjmp exception handling codegen
   method.  */

#ifdef DONT_USE_BUILTIN_SETJMP
extern void longjmp (void *, int);
#endif

/* Routine to get the head of the current thread's dynamic handler chain
   use for exception handling. */

void ***
__get_dynamic_handler_chain (void)
{
  struct eh_context *eh = (*get_eh_context) ();
  return &eh->dynamic_handler_chain;
}

/* This is used to throw an exception when the setjmp/longjmp codegen
   method is used for exception handling.

   We call __terminate if there are no handlers left.  Otherwise we run the
   cleanup actions off the dynamic cleanup stack, and pop the top of the
   dynamic handler chain, and use longjmp to transfer back to the associated
   handler.  */

void
__sjthrow (void)
{
  struct eh_context *eh = (*get_eh_context) ();
  void ***dhc = &eh->dynamic_handler_chain;
  void *jmpbuf;
  void (*func)(void *, int);
  void *arg;
  /* The cleanup chain is one word into the buffer.  Get the cleanup chain. */
  void ***cleanup = (void***)&(*dhc)[1];

  /* If there are any cleanups in the chain, run them now.  */
  if (cleanup[0])
    {
      double store[200];
      void **buf = (void**)store;
      buf[1] = 0;
      buf[0] = (*dhc);

      /* try { */
#ifdef DONT_USE_BUILTIN_SETJMP
      if (! setjmp (&buf[2]))
#else
      if (! __builtin_setjmp (&buf[2]))
#endif
	{
	  *dhc = buf;
	  while (cleanup[0])
	    {
	      func = (void(*)(void*, int))cleanup[0][1];
	      arg = (void*)cleanup[0][2];

	      /* Update this before running the cleanup.  */
	      cleanup[0] = (void **)cleanup[0][0];

	      (*func)(arg, 2);
	    }
	  *dhc = buf[0];
	}
      /* catch (...) */
      else
	{
	  __terminate ();
	}
    }
  
  /* We must call terminate if we try and rethrow an exception, when
     there is no exception currently active and when there are no
     handlers left.  */
  if (! eh->info || (*dhc)[0] == 0)
    __terminate ();
    
  /* Find the jmpbuf associated with the top element of the dynamic
     handler chain.  The jumpbuf starts two words into the buffer.  */
  jmpbuf = &(*dhc)[2];

  /* Then we pop the top element off the dynamic handler chain.  */
  *dhc = (void**)(*dhc)[0];

  /* And then we jump to the handler.  */

#ifdef DONT_USE_BUILTIN_SETJMP
  longjmp (jmpbuf, 1);
#else
  __builtin_longjmp (jmpbuf, 1);
#endif
}

/* Run cleanups on the dynamic cleanup stack for the current dynamic
   handler, then pop the handler off the dynamic handler stack, and
   then throw.  This is used to skip the first handler, and transfer
   control to the next handler in the dynamic handler stack.  */

void
__sjpopnthrow (void)
{
  struct eh_context *eh = (*get_eh_context) ();
  void ***dhc = &eh->dynamic_handler_chain;
  void (*func)(void *, int);
  void *arg;
  /* The cleanup chain is one word into the buffer.  Get the cleanup chain. */
  void ***cleanup = (void***)&(*dhc)[1];

  /* If there are any cleanups in the chain, run them now.  */
  if (cleanup[0])
    {
      double store[200];
      void **buf = (void**)store;
      buf[1] = 0;
      buf[0] = (*dhc);

      /* try { */
#ifdef DONT_USE_BUILTIN_SETJMP
      if (! setjmp (&buf[2]))
#else
      if (! __builtin_setjmp (&buf[2]))
#endif
	{
	  *dhc = buf;
	  while (cleanup[0])
	    {
	      func = (void(*)(void*, int))cleanup[0][1];
	      arg = (void*)cleanup[0][2];

	      /* Update this before running the cleanup.  */
	      cleanup[0] = (void **)cleanup[0][0];

	      (*func)(arg, 2);
	    }
	  *dhc = buf[0];
	}
      /* catch (...) */
      else
	{
	  __terminate ();
	}
    }

  /* Then we pop the top element off the dynamic handler chain.  */
  *dhc = (void**)(*dhc)[0];

  __sjthrow ();
}

/* Support code for all exception region-based exception handling.  */

int
__eh_rtime_match (void *rtime)
{
  void *info;
  __eh_matcher matcher;
  void *ret;

  info = *(__get_eh_info ());
  matcher = ((__eh_info *)info)->match_function;
  if (! matcher)
    {
#ifndef inhibit_libc
      fprintf (stderr, "Internal Compiler Bug: No runtime type matcher.");
#endif
      return 0;
    }
  ret = (*matcher) (info, rtime, (void *)0);
  return (ret != NULL);
}

/* This value identifies the place from which an exception is being
   thrown.  */

#ifdef EH_TABLE_LOOKUP

EH_TABLE_LOOKUP

#else

#ifdef DWARF2_UNWIND_INFO

/* Return the table version of an exception descriptor */

short 
__get_eh_table_version (exception_descriptor *table) 
{
  return table->lang.version;
}

/* Return the originating table language of an exception descriptor */

short 
__get_eh_table_language (exception_descriptor *table)
{
  return table->lang.language;
}

/* This routine takes a PC and a pointer to the exception region TABLE for
   its translation unit, and returns the address of the exception handler
   associated with the closest exception table handler entry associated
   with that PC, or 0 if there are no table entries the PC fits in.

   In the advent of a tie, we have to give the last entry, as it represents
   an inner block.  */

static void *
old_find_exception_handler (void *pc, old_exception_table *table)
{
  if (table)
    {
      int pos;
      int best = -1;

      /* We can't do a binary search because the table isn't guaranteed
         to be sorted from function to function.  */
      for (pos = 0; table[pos].start_region != (void *) -1; ++pos)
        {
          if (table[pos].start_region <= pc && table[pos].end_region > pc)
            {
              /* This can apply.  Make sure it is at least as small as
                 the previous best.  */
              if (best == -1 || (table[pos].end_region <= table[best].end_region
                        && table[pos].start_region >= table[best].start_region))
                best = pos;
            }
          /* But it is sorted by starting PC within a function.  */
          else if (best >= 0 && table[pos].start_region > pc)
            break;
        }
      if (best != -1)
        return table[best].exception_handler;
    }

  return (void *) 0;
}

/* find_exception_handler finds the correct handler, if there is one, to
   handle an exception.
   returns a pointer to the handler which controlled should be transferred
   to, or NULL if there is nothing left.
   Parameters:
   PC - pc where the exception originates. If this is a rethrow, 
        then this starts out as a pointer to the exception table
	entry we wish to rethrow out of.
   TABLE - exception table for the current module.
   EH_INFO - eh info pointer for this exception.
   RETHROW - 1 if this is a rethrow. (see incoming value of PC).
   CLEANUP - returned flag indicating whether this is a cleanup handler.
*/
static void *
find_exception_handler (void *pc, exception_descriptor *table, 
                        __eh_info *eh_info, int rethrow, int *cleanup)
{

  void *retval = NULL;
  *cleanup = 1;
  if (table)
    {
      int pos = 0;
      /* The new model assumed the table is sorted inner-most out so the
         first region we find which matches is the correct one */

      exception_table *tab = &(table->table[0]);

      /* Subtract 1 from the PC to avoid hitting the next region */
      if (rethrow) 
        {
          /* pc is actually the region table entry to rethrow out of */
          pos = ((exception_table *) pc) - tab;
          pc = ((exception_table *) pc)->end_region - 1;

          /* The label is always on the LAST handler entry for a region, 
             so we know the next entry is a different region, even if the
             addresses are the same. Make sure its not end of table tho. */
          if (tab[pos].start_region != (void *) -1)
            pos++;
        }
      else
        pc--;
      
      /* We can't do a binary search because the table is in inner-most
         to outermost address ranges within functions */
      for ( ; tab[pos].start_region != (void *) -1; pos++)
        { 
          if (tab[pos].start_region <= pc && tab[pos].end_region > pc)
            {
              if (tab[pos].match_info)
                {
                  __eh_matcher matcher = eh_info->match_function;
                  /* match info but no matcher is NOT a match */
                  if (matcher) 
                    {
                      void *ret = (*matcher)((void *) eh_info, 
                                             tab[pos].match_info, table);
                      if (ret) 
                        {
                          if (retval == NULL)
                            retval = tab[pos].exception_handler;
                          *cleanup = 0;
                          break;
                        }
                    }
                }
              else
                {
                  if (retval == NULL)
                    retval = tab[pos].exception_handler;
                }
            }
        }
    }
  return retval;
}
#endif /* DWARF2_UNWIND_INFO */
#endif /* EH_TABLE_LOOKUP */

#ifdef DWARF2_UNWIND_INFO
/* Support code for exception handling using static unwind information.  */

#include "frame.h"

/* This type is used in get_reg and put_reg to deal with ABIs where a void*
   is smaller than a word, such as the Irix 6 n32 ABI.  We cast twice to
   avoid a warning about casting between int and pointer of different
   sizes.  */

typedef int ptr_type __attribute__ ((mode (pointer)));

#ifdef INCOMING_REGNO
/* Is the saved value for register REG in frame UDATA stored in a register
   window in the previous frame?  */

/* ??? The Sparc INCOMING_REGNO references TARGET_FLAT.  This allows us
   to use the macro here.  One wonders, though, that perhaps TARGET_FLAT
   compiled functions won't work with the frame-unwind stuff here.  
   Perhaps the entireity of in_reg_window should be conditional on having
   seen a DW_CFA_GNU_window_save?  */
#define target_flags 0

static int
in_reg_window (int reg, frame_state *udata)
{
  if (udata->saved[reg] == REG_SAVED_REG)
    return INCOMING_REGNO (reg) == reg;
  if (udata->saved[reg] != REG_SAVED_OFFSET)
    return 0;

#ifdef STACK_GROWS_DOWNWARD
  return udata->reg_or_offset[reg] > 0;
#else
  return udata->reg_or_offset[reg] < 0;
#endif
}
#else
static inline int
in_reg_window (int reg __attribute__ ((__unused__)),
	       frame_state *udata __attribute__ ((__unused__)))
{
  return 0;
}
#endif /* INCOMING_REGNO */

/* Get the address of register REG as saved in UDATA, where SUB_UDATA is a
   frame called by UDATA or 0.  */

static word_type *
get_reg_addr (unsigned reg, frame_state *udata, frame_state *sub_udata)
{
  while (udata->saved[reg] == REG_SAVED_REG)
    {
      reg = udata->reg_or_offset[reg];
      if (in_reg_window (reg, udata))
	{
          udata = sub_udata;
	  sub_udata = NULL;
	}
    }
  if (udata->saved[reg] == REG_SAVED_OFFSET)
    return (word_type *)(udata->cfa + udata->reg_or_offset[reg]);
  else
    abort ();
}

/* Get the value of register REG as saved in UDATA, where SUB_UDATA is a
   frame called by UDATA or 0.  */

static inline void *
get_reg (unsigned reg, frame_state *udata, frame_state *sub_udata)
{
  return (void *)(ptr_type) *get_reg_addr (reg, udata, sub_udata);
}

/* Overwrite the saved value for register REG in frame UDATA with VAL.  */

static inline void
put_reg (unsigned reg, void *val, frame_state *udata)
{
  *get_reg_addr (reg, udata, NULL) = (word_type)(ptr_type) val;
}

/* Copy the saved value for register REG from frame UDATA to frame
   TARGET_UDATA.  Unlike the previous two functions, this can handle
   registers that are not one word large.  */

static void
copy_reg (unsigned reg, frame_state *udata, frame_state *target_udata)
{
  word_type *preg = get_reg_addr (reg, udata, NULL);
  word_type *ptreg = get_reg_addr (reg, target_udata, NULL);

  memcpy (ptreg, preg, dwarf_reg_size_table [reg]);
}

/* Retrieve the return address for frame UDATA.  */

static inline void *
get_return_addr (frame_state *udata, frame_state *sub_udata)
{
  return __builtin_extract_return_addr
    (get_reg (udata->retaddr_column, udata, sub_udata));
}

/* Overwrite the return address for frame UDATA with VAL.  */

static inline void
put_return_addr (void *val, frame_state *udata)
{
  val = __builtin_frob_return_addr (val);
  put_reg (udata->retaddr_column, val, udata);
}

/* Given the current frame UDATA and its return address PC, return the
   information about the calling frame in CALLER_UDATA.  */

static void *
next_stack_level (void *pc, frame_state *udata, frame_state *caller_udata)
{
  caller_udata = __frame_state_for (pc, caller_udata);
  if (! caller_udata)
    return 0;

  /* Now go back to our caller's stack frame.  If our caller's CFA register
     was saved in our stack frame, restore it; otherwise, assume the CFA
     register is SP and restore it to our CFA value.  */
  if (udata->saved[caller_udata->cfa_reg])
    caller_udata->cfa = get_reg (caller_udata->cfa_reg, udata, 0);
  else
    caller_udata->cfa = udata->cfa;
  caller_udata->cfa += caller_udata->cfa_offset;

  return caller_udata;
}

/* Hook to call before __terminate if only cleanup handlers remain. */
void 
__unwinding_cleanup (void)
{
}

/* throw_helper performs some of the common grunt work for a throw. This
   routine is called by throw and rethrows. This is pretty much split 
   out from the old __throw routine. An addition has been added which allows
   for a dummy call to a routine __unwinding_cleanup() when there are nothing
   but cleanups remaining. This allows a debugger to examine the state
   at which the throw was executed, before any cleanups, rather than
   at the terminate point after the stack has been unwound.

   EH is the current eh_context structure.
   PC is the address of the call to __throw.
   MY_UDATA is the unwind information for __throw.
   OFFSET_P is where we return the SP adjustment offset.  */

static void *
throw_helper (struct eh_context *eh, void *pc, frame_state *my_udata,
	      long *offset_p)
{
  frame_state ustruct2, *udata = &ustruct2;
  frame_state ustruct;
  frame_state *sub_udata = &ustruct;
  void *saved_pc = pc;
  void *handler;
  void *handler_p = 0;
  void *pc_p = 0;
  frame_state saved_ustruct;
  int new_eh_model;
  int cleanup = 0;
  int only_cleanup = 0;
  int rethrow = 0;
  int saved_state = 0;
  long args_size;
  __eh_info *eh_info = (__eh_info *)eh->info;

  /* Do we find a handler based on a re-throw PC? */
  if (eh->table_index != (void *) 0)
    rethrow = 1;

  memcpy (udata, my_udata, sizeof (*udata));

  handler = (void *) 0;
  for (;;)
    { 
      frame_state *p = udata;
      udata = next_stack_level (pc, udata, sub_udata);
      sub_udata = p;

      /* If we couldn't find the next frame, we lose.  */
      if (! udata)
	break;

      if (udata->eh_ptr == NULL)
        new_eh_model = 0;
      else
        new_eh_model = (((exception_descriptor *)(udata->eh_ptr))->
                                          runtime_id_field == NEW_EH_RUNTIME);

      if (rethrow) 
        {
          rethrow = 0;
          handler = find_exception_handler (eh->table_index, udata->eh_ptr, 
                                          eh_info, 1, &cleanup);
          eh->table_index = (void *)0;
        }
      else
        if (new_eh_model)
          handler = find_exception_handler (pc, udata->eh_ptr, eh_info, 
                                            0, &cleanup);
        else
          handler = old_find_exception_handler (pc, udata->eh_ptr);

      /* If we found one, we can stop searching, if its not a cleanup. 
         for cleanups, we save the state, and keep looking. This allows
         us to call a debug hook if there are nothing but cleanups left. */
      if (handler)
	{
	  if (cleanup)
	    {
	      if (!saved_state)
		{
		  saved_ustruct = *udata;
		  handler_p = handler;
		  pc_p = pc;
		  saved_state = 1;
		  only_cleanup = 1;
		}
	    }
	  else
	    {
	      only_cleanup = 0;
	      break;
	    }
	}

      /* Otherwise, we continue searching.  We subtract 1 from PC to avoid
	 hitting the beginning of the next region.  */
      pc = get_return_addr (udata, sub_udata) - 1;
    }

  if (saved_state) 
    {
      udata = &saved_ustruct;
      handler = handler_p;
      pc = pc_p;
      if (only_cleanup)
        __unwinding_cleanup ();
    }

  /* If we haven't found a handler by now, this is an unhandled
     exception.  */
  if (! handler) 
    __terminate();

  eh->handler_label = handler;

  args_size = udata->args_size;

  if (pc == saved_pc)
    /* We found a handler in the throw context, no need to unwind.  */
    udata = my_udata;
  else
    {
      int i;

      /* Unwind all the frames between this one and the handler by copying
	 their saved register values into our register save slots.  */

      /* Remember the PC where we found the handler.  */
      void *handler_pc = pc;

      /* Start from the throw context again.  */
      pc = saved_pc;
      memcpy (udata, my_udata, sizeof (*udata));

      while (pc != handler_pc)
	{
	  frame_state *p = udata;
	  udata = next_stack_level (pc, udata, sub_udata);
	  sub_udata = p;

	  for (i = 0; i < DWARF_FRAME_REGISTERS; ++i)
	    if (i != udata->retaddr_column && udata->saved[i])
	      {
		/* If you modify the saved value of the return address
		   register on the SPARC, you modify the return address for
		   your caller's frame.  Don't do that here, as it will
		   confuse get_return_addr.  */
		if (in_reg_window (i, udata)
		    && udata->saved[udata->retaddr_column] == REG_SAVED_REG
		    && udata->reg_or_offset[udata->retaddr_column] == i)
		  continue;
		copy_reg (i, udata, my_udata);
	      }

	  pc = get_return_addr (udata, sub_udata) - 1;
	}

      /* But we do need to update the saved return address register from
	 the last frame we unwind, or the handler frame will have the wrong
	 return address.  */
      if (udata->saved[udata->retaddr_column] == REG_SAVED_REG)
	{
	  i = udata->reg_or_offset[udata->retaddr_column];
	  if (in_reg_window (i, udata))
	    copy_reg (i, udata, my_udata);
	}
    }
  /* udata now refers to the frame called by the handler frame.  */

  /* We adjust SP by the difference between __throw's CFA and the CFA for
     the frame called by the handler frame, because those CFAs correspond
     to the SP values at the two call sites.  We need to further adjust by
     the args_size of the handler frame itself to get the handler frame's
     SP from before the args were pushed for that call.  */
#ifdef STACK_GROWS_DOWNWARD
  *offset_p = udata->cfa - my_udata->cfa + args_size;
#else
  *offset_p = my_udata->cfa - udata->cfa - args_size;
#endif
		       
  return handler;
}


/* We first search for an exception handler, and if we don't find
   it, we call __terminate on the current stack frame so that we may
   use the debugger to walk the stack and understand why no handler
   was found.

   If we find one, then we unwind the frames down to the one that
   has the handler and transfer control into the handler.  */

/*extern void __throw(void) __attribute__ ((__noreturn__));*/

void
__throw (void)
{
  struct eh_context *eh = (*get_eh_context) ();
  void *pc, *handler;
  long offset;

  /* XXX maybe make my_ustruct static so we don't have to look it up for
     each throw.  */
  frame_state my_ustruct, *my_udata = &my_ustruct;

  /* This is required for C++ semantics.  We must call terminate if we
     try and rethrow an exception, when there is no exception currently
     active.  */
  if (! eh->info)
    __terminate ();
    
  /* Start at our stack frame.  */
label:
  my_udata = __frame_state_for (&&label, my_udata);
  if (! my_udata)
    __terminate ();

  /* We need to get the value from the CFA register. */
  my_udata->cfa = __builtin_dwarf_cfa ();

  /* Do any necessary initialization to access arbitrary stack frames.
     On the SPARC, this means flushing the register windows.  */
  __builtin_unwind_init ();

  /* Now reset pc to the right throw point.  */
  pc = __builtin_extract_return_addr (__builtin_return_address (0)) - 1;

  handler = throw_helper (eh, pc, my_udata, &offset);

  /* Now go!  */

  __builtin_eh_return ((void *)eh, offset, handler);

  /* Epilogue:  restore the handler frame's register values and return
     to the stub.  */
}

/*extern void __rethrow(void *) __attribute__ ((__noreturn__));*/

void
__rethrow (void *index)
{
  struct eh_context *eh = (*get_eh_context) ();
  void *pc, *handler;
  long offset;

  /* XXX maybe make my_ustruct static so we don't have to look it up for
     each throw.  */
  frame_state my_ustruct, *my_udata = &my_ustruct;

  /* This is required for C++ semantics.  We must call terminate if we
     try and rethrow an exception, when there is no exception currently
     active.  */
  if (! eh->info)
    __terminate ();

  /* This is the table index we want to rethrow from. The value of
     the END_REGION label is used for the PC of the throw, and the
     search begins with the next table entry. */
  eh->table_index = index;
    
  /* Start at our stack frame.  */
label:
  my_udata = __frame_state_for (&&label, my_udata);
  if (! my_udata)
    __terminate ();

  /* We need to get the value from the CFA register. */
  my_udata->cfa = __builtin_dwarf_cfa ();

  /* Do any necessary initialization to access arbitrary stack frames.
     On the SPARC, this means flushing the register windows.  */
  __builtin_unwind_init ();

  /* Now reset pc to the right throw point.  */
  pc = __builtin_extract_return_addr (__builtin_return_address (0)) - 1;

  handler = throw_helper (eh, pc, my_udata, &offset);

  /* Now go!  */

  __builtin_eh_return ((void *)eh, offset, handler);

  /* Epilogue:  restore the handler frame's register values and return
     to the stub.  */
}
#endif /* DWARF2_UNWIND_INFO */

#endif /* L_eh */

#ifdef L_pure
#ifndef inhibit_libc
/* This gets us __GNU_LIBRARY__.  */
#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

#ifdef __GNU_LIBRARY__
  /* Avoid forcing the library's meaning of `write' on the user program
     by using the "internal" name (for use within the library)  */
#define write(fd, buf, n)	__write((fd), (buf), (n))
#endif
#endif /* inhibit_libc */

#define MESSAGE "pure virtual method called\n"

void
__pure_virtual (void)
{
#ifndef inhibit_libc
  write (2, MESSAGE, sizeof (MESSAGE) - 1);
#endif
  __terminate ();
}
#endif
