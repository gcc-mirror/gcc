/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003  Free Software Foundation, Inc.

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


/* We include auto-host.h here to get HAVE_GAS_HIDDEN.  This is
   supposedly valid even though this is a "target" file.  */
#include "auto-host.h"

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */
#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"

/* Don't use `fancy_abort' here even if config.h says to use it.  */
#ifdef abort
#undef abort
#endif

#ifdef HAVE_GAS_HIDDEN
#define ATTRIBUTE_HIDDEN  __attribute__ ((__visibility__ ("hidden")))
#else
#define ATTRIBUTE_HIDDEN
#endif

#include "libgcc2.h"

#ifdef DECLARE_LIBRARY_RENAMES
  DECLARE_LIBRARY_RENAMES
#endif

#if defined (L_negdi2)
DWtype
__negdi2 (DWtype u)
{
  const DWunion uu = {.ll = u};
  const DWunion w = { {.low = -uu.s.low,
		       .high = -uu.s.high - ((UWtype) -uu.s.low > 0) } };

  return w.ll;
}
#endif

#ifdef L_addvsi3
Wtype
__addvSI3 (Wtype a, Wtype b)
{
  const Wtype w = a + b;

  if (b >= 0 ? w < a : w > a)
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__addvsi3 (SItype a, SItype b)
{
  const SItype w = a + b;

  if (b >= 0 ? w < a : w > a)
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_addvdi3
DWtype
__addvDI3 (DWtype a, DWtype b)
{
  const DWtype w = a + b;

  if (b >= 0 ? w < a : w > a)
    abort ();

  return w;
}
#endif

#ifdef L_subvsi3
Wtype
__subvSI3 (Wtype a, Wtype b)
{
  const Wtype w = a - b;

  if (b >= 0 ? w > a : w < a)
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__subvsi3 (SItype a, SItype b)
{
  const SItype w = a - b;

  if (b >= 0 ? w > a : w < a)
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_subvdi3
DWtype
__subvDI3 (DWtype a, DWtype b)
{
  const DWtype w = a - b;

  if (b >= 0 ? w > a : w < a)
    abort ();

  return w;
}
#endif

#ifdef L_mulvsi3
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
Wtype
__mulvSI3 (Wtype a, Wtype b)
{
  const DWtype w = (DWtype) a * (DWtype) b;

  if ((Wtype) (w >> WORD_SIZE) != (Wtype) w >> (WORD_SIZE - 1))
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
#undef WORD_SIZE
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
SItype
__mulvsi3 (SItype a, SItype b)
{
  const DItype w = (DItype) a * (DItype) b;

  if ((SItype) (w >> WORD_SIZE) != (SItype) w >> (WORD_SIZE-1))
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_negvsi2
Wtype
__negvSI2 (Wtype a)
{
  const Wtype w = -a;

  if (a >= 0 ? w > 0 : w < 0)
    abort ();

   return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__negvsi2 (SItype a)
{
  const SItype w = -a;

  if (a >= 0 ? w > 0 : w < 0)
    abort ();

   return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_negvdi2
DWtype
__negvDI2 (DWtype a)
{
  const DWtype w = -a;

  if (a >= 0 ? w > 0 : w < 0)
    abort ();

  return w;
}
#endif

#ifdef L_absvsi2
Wtype
__absvSI2 (Wtype a)
{
  Wtype w = a;

  if (a < 0)
#ifdef L_negvsi2
    w = __negvSI2 (a);
#else
    w = -a;

  if (w < 0)
    abort ();
#endif

   return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__absvsi2 (SItype a)
{
  SItype w = a;

  if (a < 0)
#ifdef L_negvsi2
    w = __negvsi2 (a);
#else
    w = -a;

  if (w < 0)
    abort ();
#endif

   return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_absvdi2
DWtype
__absvDI2 (DWtype a)
{
  DWtype w = a;

  if (a < 0)
#ifdef L_negvdi2
    w = __negvDI2 (a);
#else
    w = -a;

  if (w < 0)
    abort ();
#endif

  return w;
}
#endif

#ifdef L_mulvdi3
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
DWtype
__mulvDI3 (DWtype u, DWtype v)
{
  /* The unchecked multiplication needs 3 Wtype x Wtype multiplications,
     but the checked multiplication needs only two.  */
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};

  if (__builtin_expect (uu.s.high == uu.s.low >> (WORD_SIZE - 1), 1))
    {
      /* u fits in a single Wtype.  */
      if (__builtin_expect (vv.s.high == vv.s.low >> (WORD_SIZE - 1), 1))
	{
	  /* v fits in a single Wtype as well.  */
	  /* A single multiplication.  No overflow risk.  */
	  return (DWtype) uu.s.low * (DWtype) vv.s.low;
	}
      else
	{
	  /* Two multiplications.  */
	  DWunion w0 = {.ll = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.low};
	  DWunion w1 = {.ll = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.high};

	  if (vv.s.high < 0)
	    w1.s.high -= uu.s.low;
	  if (uu.s.low < 0)
	    w1.ll -= vv.ll;
	  w1.ll += (UWtype) w0.s.high;
	  if (__builtin_expect (w1.s.high == w1.s.low >> (WORD_SIZE - 1), 1))
	    {
	      w0.s.high = w1.s.low;
	      return w0.ll;
	    }
	}
    }
  else
    {
      if (__builtin_expect (vv.s.high == vv.s.low >> (WORD_SIZE - 1), 1))
	{
	  /* v fits into a single Wtype.  */
	  /* Two multiplications.  */
	  DWunion w0 = {.ll = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.low};
	  DWunion w1 = {.ll = (UDWtype) (UWtype) uu.s.high
			* (UDWtype) (UWtype) vv.s.low};

	  if (uu.s.high < 0)
	    w1.s.high -= vv.s.low;
	  if (vv.s.low < 0)
	    w1.ll -= uu.ll;
	  w1.ll += (UWtype) w0.s.high;
	  if (__builtin_expect (w1.s.high == w1.s.low >> (WORD_SIZE - 1), 1))
	    {
	      w0.s.high = w1.s.low;
	      return w0.ll;
	    }
	}
      else
	{
	  /* A few sign checks and a single multiplication.  */
	  if (uu.s.high >= 0)
	    {
	      if (vv.s.high >= 0)
		{
		  if (uu.s.high == 0 && vv.s.high == 0)
		    {
		      const DWtype w = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.low;
		      if (__builtin_expect (w >= 0, 1))
			return w;
		    }
		}
	      else
		{
		  if (uu.s.high == 0 && vv.s.high == (Wtype) -1)
		    {
		      DWunion ww = {.ll = (UDWtype) (UWtype) uu.s.low
				    * (UDWtype) (UWtype) vv.s.low};

		      ww.s.high -= uu.s.low;
		      if (__builtin_expect (ww.s.high < 0, 1))
			return ww.ll;
		    }
		}
	    }
	  else
	    {
	      if (vv.s.high >= 0)
		{
		  if (uu.s.high == (Wtype) -1 && vv.s.high == 0)
		    {
		      DWunion ww = {.ll = (UDWtype) (UWtype) uu.s.low
				    * (UDWtype) (UWtype) vv.s.low};

		      ww.s.high -= vv.s.low;
		      if (__builtin_expect (ww.s.high < 0, 1))
			return ww.ll;
		    }
		}
	      else
		{
		  if (uu.s.high == (Wtype) -1 && vv.s.high == (Wtype) - 1)
		    {
		      DWunion ww = {.ll = (UDWtype) (UWtype) uu.s.low
				    * (UDWtype) (UWtype) vv.s.low};

		      ww.s.high -= uu.s.low;
		      ww.s.high -= vv.s.low;
		      if (__builtin_expect (ww.s.high >= 0, 1))
			return ww.ll;
		    }
		}
	    }
	}
    }

  /* Overflow.  */
  abort ();
}
#endif


/* Unless shift functions are defined with full ANSI prototypes,
   parameter b will be promoted to int if word_type is smaller than an int.  */
#ifdef L_lshrdi3
DWtype
__lshrdi3 (DWtype u, word_type b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const word_type bm = (sizeof (Wtype) * BITS_PER_UNIT) - b;
  DWunion w;

  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (UWtype) uu.s.high >> -bm;
    }
  else
    {
      const UWtype carries = (UWtype) uu.s.high << bm;

      w.s.high = (UWtype) uu.s.high >> b;
      w.s.low = ((UWtype) uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashldi3
DWtype
__ashldi3 (DWtype u, word_type b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const word_type bm = (sizeof (Wtype) * BITS_PER_UNIT) - b;
  DWunion w;

  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (UWtype) uu.s.low << -bm;
    }
  else
    {
      const UWtype carries = (UWtype) uu.s.low >> bm;

      w.s.low = (UWtype) uu.s.low << b;
      w.s.high = ((UWtype) uu.s.high << b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashrdi3
DWtype
__ashrdi3 (DWtype u, word_type b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const word_type bm = (sizeof (Wtype) * BITS_PER_UNIT) - b;
  DWunion w;

  if (bm <= 0)
    {
      /* w.s.high = 1..1 or 0..0 */
      w.s.high = uu.s.high >> (sizeof (Wtype) * BITS_PER_UNIT - 1);
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      const UWtype carries = (UWtype) uu.s.high << bm;

      w.s.high = uu.s.high >> b;
      w.s.low = ((UWtype) uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ffssi2
#undef int
extern int __ffsSI2 (UWtype u);
int
__ffsSI2 (UWtype u)
{
  UWtype count;

  if (u == 0)
    return 0;

  count_trailing_zeros (count, u);
  return count + 1;
}
#endif

#ifdef L_ffsdi2
#undef int
extern int __ffsDI2 (DWtype u);
int
__ffsDI2 (DWtype u)
{
  const DWunion uu = {.ll = u};
  UWtype word, count, add;

  if (uu.s.low != 0)
    word = uu.s.low, add = 0;
  else if (uu.s.high != 0)
    word = uu.s.high, add = BITS_PER_UNIT * sizeof (Wtype);
  else
    return 0;

  count_trailing_zeros (count, word);
  return count + add + 1;
}
#endif

#ifdef L_muldi3
DWtype
__muldi3 (DWtype u, DWtype v)
{
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};
  DWunion w = {.ll = __umulsidi3 (uu.s.low, vv.s.low)};

  w.s.high += ((UWtype) uu.s.low * (UWtype) vv.s.high
	       + (UWtype) uu.s.high * (UWtype) vv.s.low);

  return w.ll;
}
#endif

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
#if defined (sdiv_qrnnd)
#define L_udiv_w_sdiv
#endif
#endif

#ifdef L_udiv_w_sdiv
#if defined (sdiv_qrnnd)
#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
static inline __attribute__ ((__always_inline__))
#endif
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

#ifdef L_clz
const UQItype __clz_tab[] =
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
#endif

#ifdef L_clzsi2
#undef int
extern int __clzSI2 (UWtype x);
int
__clzSI2 (UWtype x)
{
  Wtype ret;

  count_leading_zeros (ret, x);

  return ret;
}
#endif

#ifdef L_clzdi2
#undef int
extern int __clzDI2 (UDWtype x);
int
__clzDI2 (UDWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype word;
  Wtype ret, add;

  if (uu.s.high)
    word = uu.s.high, add = 0;
  else
    word = uu.s.low, add = W_TYPE_SIZE;

  count_leading_zeros (ret, word);
  return ret + add;
}
#endif

#ifdef L_ctzsi2
#undef int
extern int __ctzSI2 (UWtype x);
int
__ctzSI2 (UWtype x)
{
  Wtype ret;

  count_trailing_zeros (ret, x);

  return ret;
}
#endif

#ifdef L_ctzdi2
#undef int
extern int __ctzDI2 (UDWtype x);
int
__ctzDI2 (UDWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype word;
  Wtype ret, add;

  if (uu.s.low)
    word = uu.s.low, add = 0;
  else
    word = uu.s.high, add = W_TYPE_SIZE;

  count_trailing_zeros (ret, word);
  return ret + add;
}
#endif

#if (defined (L_popcountsi2) || defined (L_popcountdi2)	\
     || defined (L_popcount_tab))
extern const UQItype __popcount_tab[] ATTRIBUTE_HIDDEN;
#endif

#ifdef L_popcount_tab
const UQItype __popcount_tab[] =
{
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
};
#endif

#ifdef L_popcountsi2
#undef int
extern int __popcountSI2 (UWtype x);
int
__popcountSI2 (UWtype x)
{
  UWtype i, ret = 0;

  for (i = 0; i < W_TYPE_SIZE; i += 8)
    ret += __popcount_tab[(x >> i) & 0xff];

  return ret;
}
#endif

#ifdef L_popcountdi2
#undef int
extern int __popcountDI2 (UDWtype x);
int
__popcountDI2 (UDWtype x)
{
  UWtype i, ret = 0;

  for (i = 0; i < 2*W_TYPE_SIZE; i += 8)
    ret += __popcount_tab[(x >> i) & 0xff];

  return ret;
}
#endif

#ifdef L_paritysi2
#undef int
extern int __paritySI2 (UWtype x);
int
__paritySI2 (UWtype x)
{
#if W_TYPE_SIZE > 64
# error "fill out the table"
#endif
#if W_TYPE_SIZE > 32
  x ^= x >> 32;
#endif
#if W_TYPE_SIZE > 16
  x ^= x >> 16;
#endif
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0xf;
  return (0x6996 >> x) & 1;
}
#endif

#ifdef L_paritydi2
#undef int
extern int __parityDI2 (UDWtype x);
int
__parityDI2 (UDWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype nx = uu.s.low ^ uu.s.high;

#if W_TYPE_SIZE > 64
# error "fill out the table"
#endif
#if W_TYPE_SIZE > 32
  nx ^= nx >> 32;
#endif
#if W_TYPE_SIZE > 16
  nx ^= nx >> 16;
#endif
  nx ^= nx >> 8;
  nx ^= nx >> 4;
  nx &= 0xf;
  return (0x6996 >> nx) & 1;
}
#endif

#ifdef L_udivmoddi4

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
static inline __attribute__ ((__always_inline__))
#endif
UDWtype
__udivmoddi4 (UDWtype n, UDWtype d, UDWtype *rp)
{
  const DWunion nn = {.ll = n};
  const DWunion dd = {.ll = d};
  DWunion rr;
  UWtype d0, d1, n0, n1, n2;
  UWtype q0, q1;
  UWtype b, bm;

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

  const DWunion ww = {{.low = q0, .high = q1}};
  return ww.ll;
}
#endif

#ifdef L_divdi3
DWtype
__divdi3 (DWtype u, DWtype v)
{
  word_type c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    c = ~c,
    vv.ll = -vv.ll;

  w = __udivmoddi4 (uu.ll, vv.ll, (UDWtype *) 0);
  if (c)
    w = -w;

  return w;
}
#endif

#ifdef L_moddi3
DWtype
__moddi3 (DWtype u, DWtype v)
{
  word_type c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    vv.ll = -vv.ll;

  (void) __udivmoddi4 (uu.ll, vv.ll, &w);
  if (c)
    w = -w;

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
  const DWunion au = {.ll = a};
  const DWunion bu = {.ll = b};

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
  const DWunion au = {.ll = a};
  const DWunion bu = {.ll = b};

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
__fixunstfDI (TFtype a)
{
  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  const TFtype b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  UDWtype v = (UWtype) b;
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
    return - __fixunstfDI (-a);
  return __fixunstfDI (a);
}
#endif

#if defined(L_fixunsxfdi) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunsxfDI (XFtype a)
{
  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  const XFtype b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  UDWtype v = (UWtype) b;
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
    return - __fixunsxfDI (-a);
  return __fixunsxfDI (a);
}
#endif

#ifdef L_fixunsdfdi
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunsdfDI (DFtype a)
{
  /* Get high part of result.  The division here will just moves the radix
     point and will not cause any rounding.  Then the conversion to integral
     type chops result as desired.  */
  const UWtype hi = a / HIGH_WORD_COEFF;

  /* Get low part of result.  Convert `hi' to floating type and scale it back,
     then subtract this from the number being converted.  This leaves the low
     part.  Convert that to integral type.  */
  const UWtype lo = (a - ((DFtype) hi) * HIGH_WORD_COEFF);

  /* Assemble result from the two parts.  */
  return ((UDWtype) hi << WORD_SIZE) | lo;
}
#endif

#ifdef L_fixdfdi
DWtype
__fixdfdi (DFtype a)
{
  if (a < 0)
    return - __fixunsdfDI (-a);
  return __fixunsdfDI (a);
}
#endif

#ifdef L_fixunssfdi
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

DWtype
__fixunssfDI (SFtype original_a)
{
  /* Convert the SFtype to a DFtype, because that is surely not going
     to lose any bits.  Some day someone else can write a faster version
     that avoids converting to DFtype, and verify it really works right.  */
  const DFtype a = original_a;

  /* Get high part of result.  The division here will just moves the radix
     point and will not cause any rounding.  Then the conversion to integral
     type chops result as desired.  */
  const UWtype hi = a / HIGH_WORD_COEFF;

  /* Get low part of result.  Convert `hi' to floating type and scale it back,
     then subtract this from the number being converted.  This leaves the low
     part.  Convert that to integral type.  */
  const UWtype lo = (a - ((DFtype) hi) * HIGH_WORD_COEFF);

  /* Assemble result from the two parts.  */
  return ((UDWtype) hi << WORD_SIZE) | lo;
}
#endif

#ifdef L_fixsfdi
DWtype
__fixsfdi (SFtype a)
{
  if (a < 0)
    return - __fixunssfDI (-a);
  return __fixunssfDI (a);
}
#endif

#if defined(L_floatdixf) && (LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (Wtype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDWtype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDWtype) 1) << WORD_SIZE)

XFtype
__floatdixf (DWtype u)
{
  XFtype d = (Wtype) (u >> WORD_SIZE);
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
  TFtype d = (Wtype) (u >> WORD_SIZE);
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
  DFtype d = (Wtype) (u >> WORD_SIZE);
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
#define DF_SIZE DBL_MANT_DIG
#define SF_SIZE FLT_MANT_DIG

SFtype
__floatdisf (DWtype u)
{
  /* Protect against double-rounding error.
     Represent any low-order bits, that might be truncated in DFmode,
     by a bit that won't be lost.  The bit can go in anywhere below the
     rounding position of the SFmode.  A fixed mask and bit position
     handles all usual configurations.  It doesn't handle the case
     of 128-bit DImode, however.  */
  if (DF_SIZE < DI_SIZE
      && DF_SIZE > (DI_SIZE - DF_SIZE + SF_SIZE))
    {
#define REP_BIT ((UDWtype) 1 << (DI_SIZE - DF_SIZE))
      if (! (- ((DWtype) 1 << DF_SIZE) < u
	     && u < ((DWtype) 1 << DF_SIZE)))
	{
	  if ((UDWtype) u & (REP_BIT - 1))
	    {
	      u &= ~ (REP_BIT - 1);
	      u |= REP_BIT;
	    }
	}
    }
  /* Do the calculation in DFmode
     so that we don't lose any of the precision of the high word
     while multiplying it.  */
  DFtype f = (Wtype) (u >> WORD_SIZE);
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
__fixunsxfSI (XFtype a)
{
  if (a >= - (DFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
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
__fixunsdfSI (DFtype a)
{
  if (a >= - (DFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
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
__fixunssfSI (SFtype a)
{
  if (a >= - (SFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
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
      const unsigned char c1 = *s1++, c2 = *s2++;
      if (c1 != c2)
	return c1 - c2;
      size--;
    }
  return 0;
}

#endif

/* __eprintf used to be used by GCC's private version of <assert.h>.
   We no longer provide that header, but this routine remains in libgcc.a
   for binary backward compatibility.  Note that it is not included in
   the shared version of libgcc.  */
#ifdef L_eprintf
#ifndef inhibit_libc

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

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


#ifdef L_clear_cache
/* Clear part of an instruction cache.  */

void
__clear_cache (char *beg __attribute__((__unused__)),
	       char *end __attribute__((__unused__)))
{
#ifdef CLEAR_INSN_CACHE
  CLEAR_INSN_CACHE (beg, end);
#endif /* CLEAR_INSN_CACHE */
}

#endif /* L_clear_cache */

#ifdef L_enable_execute_stack
/* Attempt to turn on execute permission for the stack.  */

#ifdef ENABLE_EXECUTE_STACK
  ENABLE_EXECUTE_STACK
#else
void
__enable_execute_stack (void *addr __attribute__((__unused__)))
{}
#endif /* ENABLE_EXECUTE_STACK */

#endif /* L_enable_execute_stack */

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

#ifdef EH_FRAME_SECTION_NAME
#include "unwind-dw2-fde.h"
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
#if defined (EH_FRAME_SECTION_NAME) && !defined (HAS_INIT_SECTION)
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
#ifdef EH_FRAME_SECTION_NAME
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

extern void SYMBOL__MAIN (void);
void
SYMBOL__MAIN (void)
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
#if defined (TARGET_ASM_CONSTRUCTOR) || defined (USE_COLLECT2)
func_ptr __CTOR_LIST__[2] = {0, 0};
func_ptr __DTOR_LIST__[2] = {0, 0};
#else
func_ptr __CTOR_LIST__[2];
func_ptr __DTOR_LIST__[2];
#endif
#endif /* no INIT_SECTION_ASM_OP and not CTOR_LISTS_DEFINED_EXTERNALLY */
#endif /* L_ctors */

