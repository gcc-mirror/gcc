/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "machmode.h"
#ifndef L_trampoline
#include "gstddef.h"
#endif

/* Don't use `fancy_abort' here even if config.h says to use it.  */
#ifdef abort
#undef abort
#endif

/* In the first part of this file, we are interfacing to calls generated
   by the compiler itself.  These calls pass values into these routines
   which have very specific modes (rather than very specific types), and
   these compiler-generated calls also expect any return values to have
   very specific modes (rather than very specific types).  Thus, we need
   to avoid using regular C language type names in this part of the file
   because the sizes for those types can be configured to be anything.
   Instead we use the following special type names.  */

typedef unsigned int UQItype	__attribute__ ((mode (QI)));
typedef 	 int SItype	__attribute__ ((mode (SI)));
typedef unsigned int USItype	__attribute__ ((mode (SI)));
typedef		 int DItype	__attribute__ ((mode (DI)));
typedef unsigned int UDItype	__attribute__ ((mode (DI)));
typedef 	float SFtype	__attribute__ ((mode (SF)));
typedef		float DFtype	__attribute__ ((mode (DF)));
#if LONG_DOUBLE_TYPE_SIZE == 96
typedef		float XFtype	__attribute__ ((mode (XF)));
#endif
#if LONG_DOUBLE_TYPE_SIZE == 128
typedef		float TFtype	__attribute__ ((mode (TF)));
#endif

#if BITS_PER_WORD==16
typedef int word_type __attribute__ ((mode (HI)));
#endif
#if BITS_PER_WORD==32
typedef int word_type __attribute__ ((mode (SI)));
#endif
#if BITS_PER_WORD==64
typedef int word_type __attribute__ ((mode (DI)));
#endif

/* Make sure that we don't accidentally use any normal C language built-in
   type names in the first part of this file.  Instead we want to use *only*
   the type names defined above.  The following macro definitions insure
   that if we *do* accidentally use some normal C language built-in type name,
   we will get a syntax error.  */

#define char bogus_type
#define short bogus_type
#define int bogus_type
#define long bogus_type
#define unsigned bogus_type
#define float bogus_type
#define double bogus_type

#define SI_TYPE_SIZE (sizeof (SItype) * BITS_PER_UNIT)

/* DIstructs are pairs of SItype values in the order determined by
   WORDS_BIG_ENDIAN.  */

#if WORDS_BIG_ENDIAN
  struct DIstruct {SItype high, low;};
#else
  struct DIstruct {SItype low, high;};
#endif

/* We need this union to unpack/pack DImode values, since we don't have
   any arithmetic yet.  Incoming DImode parameters are stored into the
   `ll' field, and the unpacked result is read from the struct `s'.  */

typedef union
{
  struct DIstruct s;
  DItype ll;
} DIunion;

#if defined (L_udivmoddi4) || defined (L_muldi3) || defined (L_udiv_w_sdiv)

#include "longlong.h"

#endif /* udiv or mul */

extern DItype __fixunssfdi (SFtype a);
extern DItype __fixunsdfdi (DFtype a);

#if defined (L_negdi2) || defined (L_divdi3) || defined (L_moddi3)
#if defined (L_divdi3) || defined (L_moddi3)
static inline
#endif
DItype
__negdi2 (u)
     DItype u;
{
  DIunion w;
  DIunion uu;

  uu.ll = u;

  w.s.low = -uu.s.low;
  w.s.high = -uu.s.high - ((USItype) w.s.low > 0);

  return w.ll;
}
#endif

#ifdef L_lshldi3
DItype
__lshldi3 (u, b)
     DItype u;
     SItype b;
{
  DIunion w;
  SItype bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (USItype)uu.s.low << -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.low >> bm;
      w.s.low = (USItype)uu.s.low << b;
      w.s.high = ((USItype)uu.s.high << b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_lshrdi3
DItype
__lshrdi3 (u, b)
     DItype u;
     SItype b;
{
  DIunion w;
  SItype bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (USItype)uu.s.high >> -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.high << bm;
      w.s.high = (USItype)uu.s.high >> b;
      w.s.low = ((USItype)uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashldi3
DItype
__ashldi3 (u, b)
     DItype u;
     SItype b;
{
  DIunion w;
  SItype bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (USItype)uu.s.low << -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.low >> bm;
      w.s.low = (USItype)uu.s.low << b;
      w.s.high = ((USItype)uu.s.high << b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashrdi3
DItype
__ashrdi3 (u, b)
     DItype u;
     SItype b;
{
  DIunion w;
  SItype bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      /* w.s.high = 1..1 or 0..0 */
      w.s.high = uu.s.high >> (sizeof (SItype) * BITS_PER_UNIT - 1);
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.high << bm;
      w.s.high = uu.s.high >> b;
      w.s.low = ((USItype)uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ffsdi2
DItype
__ffsdi2 (u)
     DItype u;
{
  DIunion uu, w;
  uu.ll = u;
  w.s.high = 0;
  w.s.low = ffs (uu.s.low);
  if (w.s.low != 0)
    return w.ll;
  w.s.low = ffs (uu.s.high);
  if (w.s.low != 0)
    {
      w.s.low += BITS_PER_UNIT * sizeof (SItype);
      return w.ll;
    }
  return w.ll;
}
#endif

#ifdef L_muldi3
DItype
__muldi3 (u, v)
     DItype u, v;
{
  DIunion w;
  DIunion uu, vv;

  uu.ll = u,
  vv.ll = v;

  w.ll = __umulsidi3 (uu.s.low, vv.s.low);
  w.s.high += ((USItype) uu.s.low * (USItype) vv.s.high
	       + (USItype) uu.s.high * (USItype) vv.s.low);

  return w.ll;
}
#endif

#ifdef L_udiv_w_sdiv
USItype
__udiv_w_sdiv (rp, a1, a0, d)
     USItype *rp, a1, a0, d;
{
  USItype q, r;
  USItype c0, c1, b1;

  if ((SItype) d >= 0)
    {
      if (a1 < d - a1 - (a0 >> (SI_TYPE_SIZE - 1)))
	{
	  /* dividend, divisor, and quotient are nonnegative */
	  sdiv_qrnnd (q, r, a1, a0, d);
	}
      else
	{
	  /* Compute c1*2^32 + c0 = a1*2^32 + a0 - 2^31*d */
	  sub_ddmmss (c1, c0, a1, a0, d >> 1, d << (SI_TYPE_SIZE - 1));
	  /* Divide (c1*2^32 + c0) by d */
	  sdiv_qrnnd (q, r, c1, c0, d);
	  /* Add 2^31 to quotient */
	  q += (USItype) 1 << (SI_TYPE_SIZE - 1);
	}
    }
  else
    {
      b1 = d >> 1;			/* d/2, between 2^30 and 2^31 - 1 */
      c1 = a1 >> 1;			/* A/2 */
      c0 = (a1 << (SI_TYPE_SIZE - 1)) + (a0 >> 1);

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

UDItype
__udivmoddi4 (n, d, rp)
     UDItype n, d;
     UDItype *rp;
{
  DIunion ww;
  DIunion nn, dd;
  DIunion rr;
  USItype d0, d1, n0, n1, n2;
  USItype q0, q1;
  USItype b, bm;

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
	      n1 = (n1 << bm) | (n0 >> (SI_TYPE_SIZE - bm));
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
		 (Shifts counts of SI_TYPE_SIZE are undefined.)  */

	      n1 -= d0;
	      q1 = 1;
	    }
	  else
	    {
	      /* Normalize.  */

	      b = SI_TYPE_SIZE - bm;

	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q1, n1, n2, n1, d0);
	    }

	  /* n1 != d0... */

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
	      USItype m1, m0;
	      /* Normalize.  */

	      b = SI_TYPE_SIZE - bm;

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
UDItype __udivmoddi4 ();
DItype
__divdi3 (u, v)
     DItype u, v;
{
  SItype c = 0;
  DIunion uu, vv;
  DItype w;

  uu.ll = u;
  vv.ll = v;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = __negdi2 (uu.ll);
  if (vv.s.high < 0)
    c = ~c,
    vv.ll = __negdi2 (vv.ll);

  w = __udivmoddi4 (uu.ll, vv.ll, (UDItype *) 0);
  if (c)
    w = __negdi2 (w);

  return w;
}
#endif

#ifdef L_moddi3
UDItype __udivmoddi4 ();
DItype
__moddi3 (u, v)
     DItype u, v;
{
  SItype c = 0;
  DIunion uu, vv;
  DItype w;

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
UDItype __udivmoddi4 ();
UDItype
__umoddi3 (u, v)
     UDItype u, v;
{
  DItype w;

  (void) __udivmoddi4 (u, v, &w);

  return w;
}
#endif

#ifdef L_udivdi3
UDItype __udivmoddi4 ();
UDItype
__udivdi3 (n, d)
     UDItype n, d;
{
  return __udivmoddi4 (n, d, (UDItype *) 0);
}
#endif

#ifdef L_cmpdi2
word_type
__cmpdi2 (a, b)
     DItype a, b;
{
  DIunion au, bu;

  au.ll = a, bu.ll = b;

  if (au.s.high < bu.s.high)
    return 0;
  else if (au.s.high > bu.s.high)
    return 2;
  if ((USItype) au.s.low < (USItype) bu.s.low)
    return 0;
  else if ((USItype) au.s.low > (USItype) bu.s.low)
    return 2;
  return 1;
}
#endif

#ifdef L_ucmpdi2
word_type
__ucmpdi2 (a, b)
     DItype a, b;
{
  DIunion au, bu;

  au.ll = a, bu.ll = b;

  if ((USItype) au.s.high < (USItype) bu.s.high)
    return 0;
  else if ((USItype) au.s.high > (USItype) bu.s.high)
    return 2;
  if ((USItype) au.s.low < (USItype) bu.s.low)
    return 0;
  else if ((USItype) au.s.low > (USItype) bu.s.low)
    return 2;
  return 1;
}
#endif

#if defined(L_fixunstfdi) && (LONG_DOUBLE_TYPE_SIZE == 128)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunstfdi (a)
     TFtype a;
{
  TFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the TFtype, leaving the low part as flonum.  */
  a -= (TFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#if defined(L_fixtfdi) && (LONG_DOUBLE_TYPE_SIZE == 128)
DItype
__fixtfdi (a)
     TFtype a;
{
  if (a < 0)
    return - __fixunstfdi (-a);
  return __fixunstfdi (a);
}
#endif

#if defined(L_fixunsxfdi) && (LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunsxfdi (a)
     XFtype a;
{
  XFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the XFtype, leaving the low part as flonum.  */
  a -= (XFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#if defined(L_fixxfdi) && (LONG_DOUBLE_TYPE_SIZE == 96)
DItype
__fixxfdi (a)
     XFtype a;
{
  if (a < 0)
    return - __fixunsxfdi (-a);
  return __fixunsxfdi (a);
}
#endif

#ifdef L_fixunsdfdi
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunsdfdi (a)
     DFtype a;
{
  DFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the DFtype, leaving the low part as flonum.  */
  a -= (DFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#ifdef L_fixdfdi
DItype
__fixdfdi (a)
     DFtype a;
{
  if (a < 0)
    return - __fixunsdfdi (-a);
  return __fixunsdfdi (a);
}
#endif

#ifdef L_fixunssfdi
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunssfdi (SFtype original_a)
{
  /* Convert the SFtype to a DFtype, because that is surely not going
     to lose any bits.  Some day someone else can write a faster version
     that avoids converting to DFtype, and verify it really works right.  */
  DFtype a = original_a;
  DFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the DFtype, leaving the low part as flonum.  */
  a -= (DFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#ifdef L_fixsfdi
DItype
__fixsfdi (SFtype a)
{
  if (a < 0)
    return - __fixunssfdi (-a);
  return __fixunssfdi (a);
}
#endif

#if defined(L_floatdixf) && (LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

XFtype
__floatdixf (u)
     DItype u;
{
  XFtype d;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  d = (USItype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (negate ? -d : d);
}
#endif

#if defined(L_floatditf) && (LONG_DOUBLE_TYPE_SIZE == 128)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

TFtype
__floatditf (u)
     DItype u;
{
  TFtype d;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  d = (USItype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (negate ? -d : d);
}
#endif

#ifdef L_floatdidf
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DFtype
__floatdidf (u)
     DItype u;
{
  DFtype d;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  d = (USItype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (negate ? -d : d);
}
#endif

#ifdef L_floatdisf
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

SFtype
__floatdisf (u)
     DItype u;
{
  /* Do the calculation in DFmode
     so that we don't lose any of the precision of the high word
     while multiplying it.  */
  DFtype f;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  f = (USItype) (u >> WORD_SIZE);
  f *= HIGH_HALFWORD_COEFF;
  f *= HIGH_HALFWORD_COEFF;
  f += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (SFtype) (negate ? -f : f);
}
#endif

#if defined(L_fixunsxfsi) && LONG_DOUBLE_TYPE_SIZE == 96
#include "glimits.h"

USItype
__fixunsxfsi (a)
     XFtype a;
{
  if (a >= - (DFtype) LONG_MIN)
    return (SItype) (a + LONG_MIN) - LONG_MIN;
  return (SItype) a;
}
#endif

#ifdef L_fixunsdfsi
#include "glimits.h"

USItype
__fixunsdfsi (a)
     DFtype a;
{
  if (a >= - (DFtype) LONG_MIN)
    return (SItype) (a + LONG_MIN) - LONG_MIN;
  return (SItype) a;
}
#endif

#ifdef L_fixunssfsi
#include "glimits.h"

USItype
__fixunssfsi (SFtype a)
{
  if (a >= - (SFtype) LONG_MIN)
    return (SItype) (a + LONG_MIN) - LONG_MIN;
  return (SItype) a;
}
#endif

/* From here on down, the routines use normal data types.  */

#define SItype bogus_type
#define USItype bogus_type
#define DItype bogus_type
#define UDItype bogus_type
#define SFtype bogus_type
#define DFtype bogus_type

#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double

#ifdef L__gcc_bcmp

/* Like bcmp except the sign is meaningful.
   Reult is negative if S1 is less than S2,
   positive if S1 is greater, 0 if S1 and S2 are equal.  */

int
__gcc_bcmp (s1, s2, size)
     unsigned char *s1, *s2;
     size_t size;
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
  asm ("	.ent __builtin_saveregs");
  asm ("	.globl __builtin_saveregs");
  asm ("__builtin_saveregs:");
  asm ("	sw	$4,0($30)");
  asm ("	sw	$5,4($30)");
  asm ("	sw	$6,8($30)");
  asm ("	sw	$7,12($30)");
  asm ("	j	$31");
  asm ("	.end __builtin_saveregs");
#else /* not __mips__, etc. */
__builtin_saveregs ()
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
__eprintf (string, expression, line, filename)
     const char *string;
     const char *expression;
     int line;
     const char *filename;
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}

#endif
#endif

#ifdef L_bb
/* Avoid warning from ranlib about empty object file.  */
void
__bb_avoid_warning ()
{}

#if defined (__sun__) && defined (__mc68000__)
struct bb
{
  int initialized;
  char *filename;
  int *counts;
  int ncounts;
  int zero_word;
  int *addresses;
};

extern int ___tcov_init;

__bb_init_func (blocks)
	struct bb *blocks;
{
  if (! ___tcov_init)
    ___tcov_init_func ();

  ___bb_link (blocks->filename, blocks->counts, blocks->ncounts);
}

#endif
#endif

/* frills for C++ */

#ifdef L_op_new
typedef void (*vfp)(void);

extern vfp __new_handler;

/* void * operator new (size_t sz) */
void *
__builtin_new (size_t sz)
{
  void *p;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = (void *) malloc (sz);
  if (p == 0)
    (*__new_handler) ();
  return p;
}
#endif /* L_op_new */

#ifdef L_new_handler

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

typedef void (*vfp)(void);

extern void *__builtin_new (size_t);
static void default_new_handler (void);

vfp __new_handler = default_new_handler;

vfp
__set_new_handler (handler)
     vfp handler;
{
  vfp prev_handler;

  prev_handler = __new_handler;
  if (handler == 0) handler = default_new_handler;
  __new_handler = handler;
  return prev_handler;
}

vfp
set_new_handler (handler)
     vfp handler;
{
  return __set_new_handler (handler);
}

#define MESSAGE "Virtual memory exceeded in `new'\n"

static void
default_new_handler ()
{
  /* don't use fprintf (stderr, ...) because it may need to call malloc.  */
  /* This should really print the name of the program, but that is hard to
     do.  We need a standard, clean way to get at the name.  */
  write (2, MESSAGE, sizeof (MESSAGE));
  /* don't call exit () because that may call global destructors which
     may cause a loop.  */
  _exit (-1);
}
#endif

#ifdef L_op_delete
/* void operator delete (void *ptr) */
void
__builtin_delete (void *ptr)
{
  if (ptr)
    free (ptr);
}
#endif

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
__clear_cache (beg, end)
     char *beg, *end;
{
#ifdef INSN_CACHE_SIZE
  static char array[INSN_CACHE_SIZE + INSN_CACHE_PLANE_SIZE + INSN_CACHE_LINE_WIDTH];
  static int initialized = 0;
  int offset;
  void *start_addr
  void *end_addr;
  typedef (*function_ptr) ();

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
      *(INSTRUCTION_TYPE *)(ptr - INSN_CACHE_LINE_WIDTH) = RETURN_INSTRUCTION;

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
}

#endif /* L_clear_cache */

#ifdef L_trampoline

/* Jump to a trampoline, loading the static chain address.  */

#ifdef TRANSFER_FROM_TRAMPOLINE 
TRANSFER_FROM_TRAMPOLINE 
#endif

#ifdef __convex__

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in convex.h.  */

#include <sys/mman.h>
#include <sys/vmparam.h>
#include <machine/machparam.h>

void
__enable_execute_stack ()
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

  /* Clear instruction cache in case an old trampoline is in it. */
  asm ("pich");
}
#endif /* __convex__ */

#ifdef __pyr__

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/vmmac.h>

/* Modified from the convex -code above.
   mremap promises to clear the i-cache. */

void
__enable_execute_stack ()
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
#endif /* L_trampoline */

#ifdef L__main

#include "gbl-ctors.h"

/* Run all the global destructors on exit from the program.  */

void
__do_global_dtors ()
{
#ifdef DO_GLOBAL_DTORS_BODY
  DO_GLOBAL_DTORS_BODY;
#else
  unsigned nptrs = (unsigned HOST_WIDE_INT) __DTOR_LIST__[0];
  unsigned i;

  /* Some systems place the number of pointers
     in the first word of the table.
     On other systems, that word is -1.
     In all cases, the table is null-terminated.  */

  /* If the length is not recorded, count up to the null.  */
  if (nptrs == -1)
    for (nptrs = 0; __DTOR_LIST__[nptrs + 1] != 0; nptrs++);

  /* GNU LD format.  */
  for (i = nptrs; i >= 1; i--)
    __DTOR_LIST__[i] ();
#endif
}

#ifndef INIT_SECTION_ASM_OP
/* Run all the global constructors on entry to the program.  */

#ifndef ON_EXIT
#define ON_EXIT(a, b)
#else
/* Make sure the exit routine is pulled in to define the globals as
   bss symbols, just in case the linker does not automatically pull
   bss definitions from the library.  */

extern int _exit_dummy_decl;
int *_exit_dummy_ref = &_exit_dummy_decl;
#endif /* ON_EXIT */

void
__do_global_ctors ()
{
  DO_GLOBAL_CTORS_BODY;
  ON_EXIT (__do_global_dtors, 0);
}
#endif /* no INIT_SECTION_ASM_OP */

#if !defined (INIT_SECTION_ASM_OP) || defined (INVOKE__main)
/* Subroutine called automatically by `main'.
   Compiling a global function named `main'
   produces an automatic call to this function at the beginning.

   For many systems, this routine calls __do_global_ctors.
   For systems which support a .init section we use the .init section
   to run __do_global_ctors, so we need not do anything here.  */

void
__main ()
{
  /* Support recursive calls to `main': run initializers just once.  */
  static int initialized = 0;
  if (! initialized)
    {
      initialized = 1;
      __do_global_ctors ();
    }
}
#endif /* no INIT_SECTION_ASM_OP or INVOKE__main */

#endif /* L__main */

#ifdef L_ctors

#include "gbl-ctors.h"

/* Provide default definitions for the lists of constructors and
   destructors, so that we don't get linker errors.  These symbols are
   intentionally bss symbols, so that gld and/or collect will provide
   the right values.  */

/* We declare the lists here with two elements each,
   so that they are valid empty lists if no other definition is loaded.  */
#if !defined(INIT_SECTION_ASM_OP) && !defined(CTOR_LISTS_DEFINED_EXTERNALLY)
#ifdef __NeXT__
/* After 2.3, try this definition on all systems.  */
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

#ifndef ON_EXIT

/* If we have no known way of registering our own __do_global_dtors
   routine so that it will be invoked at program exit time, then we
   have to define our own exit routine which will get this to happen.  */

extern void __do_global_dtors ();
extern void _cleanup ();
extern volatile void _exit ();

void 
exit (status)
     int status;
{
  __do_global_dtors ();
#ifdef EXIT_BODY
  EXIT_BODY;
#else
  _cleanup ();
#endif
  _exit (status);
}

#else
int _exit_dummy_decl = 0;	/* prevent compiler & linker warnings */
#endif

#endif /* L_exit */

/* In a.out systems, we need to have these dummy constructor and destructor
   lists in the library.

   When using `collect', the first link will resolve __CTOR_LIST__
   and __DTOR_LIST__ to these symbols.  We will then run "nm" on the
   result, build the correct __CTOR_LIST__ and __DTOR_LIST__, and relink.
   Since we don't do the second link if no constructors existed, these
   dummies must be fully functional empty lists.

   When using `gnu ld', these symbols will be used if there are no
   constructors.  If there are constructors, the N_SETV symbol defined
   by the linker from the N_SETT's in input files will define __CTOR_LIST__
   and __DTOR_LIST__ rather than its being allocated as common storage
   by the definitions below.

   When using a linker that supports constructor and destructor segments,
   these definitions will not be used, since crtbegin.o and crtend.o
   (from crtstuff.c) will have already defined __CTOR_LIST__ and
    __DTOR_LIST__.  The crt*.o files are passed directly to the linker
   on its command line, by gcc.  */

/* The list needs two elements:  one is ignored (the old count); the
   second is the terminating zero.  Since both values are zero, this
   declaration is not initialized, and it becomes `common'.  */

#ifdef L_ctor_list
#include "gbl-ctors.h"
func_ptr __CTOR_LIST__[2];
#endif

#ifdef L_dtor_list
#include "gbl-ctors.h"
func_ptr __DTOR_LIST__[2];
#endif
