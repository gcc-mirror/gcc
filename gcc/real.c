/* real.c - implementation of REAL_ARITHMETIC, REAL_VALUE_ATOF,
and support for XFmode IEEE extended real floating point arithmetic.
Contributed by Stephen L. Moshier (moshier@world.std.com).

   Copyright (C) 1993 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <errno.h>
#include "config.h"
#include "tree.h"

#ifndef errno
extern int errno;
#endif

/* To enable support of XFmode extended real floating point, define
LONG_DOUBLE_TYPE_SIZE 96 in the tm.h file (m68k.h or i386.h).

To support cross compilation between IEEE and VAX floating
point formats, define REAL_ARITHMETIC in the tm.h file.

In either case the machine files (tm.h) must not contain any code
that tries to use host floating point arithmetic to convert
REAL_VALUE_TYPEs from `double' to `float', pass them to fprintf,
etc.  In cross-compile situations a REAL_VALUE_TYPE may not
be intelligible to the host computer's native arithmetic.

The emulator defaults to the host's floating point format so that
its decimal conversion functions can be used if desired (see
real.h).

The first part of this file interfaces gcc to ieee.c, which is a
floating point arithmetic suite that was not written with gcc in
mind.  The interface is followed by ieee.c itself and related
items. Avoid changing ieee.c unless you have suitable test
programs available.  A special version of the PARANOIA floating
point arithmetic tester, modified for this purpose, can be found
on usc.edu : /pub/C-numanal/ieeetest.zoo.  Some tutorial
information on ieee.c is given in my book: S. L. Moshier,
_Methods and Programs for Mathematical Functions_, Prentice-Hall
or Simon & Schuster Int'l, 1989.  A library of XFmode elementary
transcendental functions can be obtained by ftp from
research.att.com: netlib/cephes/ldouble.shar.Z  */

/* Type of computer arithmetic.
 * Only one of DEC, MIEEE, IBMPC, or UNK should get defined.
 */

/* `MIEEE' refers generically to big-endian IEEE floating-point data
   structure.  This definition should work in SFmode `float' type and
   DFmode `double' type on virtually all big-endian IEEE machines.
   If LONG_DOUBLE_TYPE_SIZE has been defined to be 96, then MIEEE
   also invokes the particular XFmode (`long double' type) data
   structure used by the Motorola 680x0 series processors.

   `IBMPC' refers generally to little-endian IEEE machines. In this
   case, if LONG_DOUBLE_TYPE_SIZE has been defined to be 96, then
   IBMPC also invokes the particular XFmode `long double' data
   structure used by the Intel 80x86 series processors.

   `DEC' refers specifically to the Digital Equipment Corp PDP-11
   and VAX floating point data structure.  This model currently
   supports no type wider than DFmode.

   If LONG_DOUBLE_TYPE_SIZE = 64 (the default, unless tm.h defines it)
   then `long double' and `double' are both implemented, but they
   both mean DFmode.  In this case, the software floating-point
   support available here is activated by writing
      #define REAL_ARITHMETIC
   in tm.h. 

   The case LONG_DOUBLE_TYPE_SIZE = 128 activates TFmode support
   (Not Yet Implemented) and may deactivate XFmode since
   `long double' is used to refer to both modes.    */

/* The following converts gcc macros into the ones used by this file.  */

/* REAL_ARITHMETIC defined means that macros in real.h are
   defined to call emulator functions.  */
#ifdef REAL_ARITHMETIC

#if TARGET_FLOAT_FORMAT == VAX_FLOAT_FORMAT
/* PDP-11, Pro350, VAX: */
#define DEC 1
#else /* it's not VAX */
#if TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
#if WORDS_BIG_ENDIAN
/* Motorola IEEE, high order words come first (Sun workstation): */
#define MIEEE 1
#else /* not big-endian */
/* Intel IEEE, low order words come first:
 */
#define IBMPC 1
#endif /*  big-endian */
#else /* it's not IEEE either */
/* UNKnown arithmetic.  We don't support this and can't go on. */
unknown arithmetic type
#define UNK 1
#endif /* not IEEE */
#endif /* not VAX */

#else
/* REAL_ARITHMETIC not defined means that the *host's* data
   structure will be used.  It may differ by endian-ness from the
   target machine's structure and will get its ends swapped
   accordingly (but not here).  Probably only the decimal <-> binary
   functions in this file will actually be used in this case.  */
#if HOST_FLOAT_FORMAT == VAX_FLOAT_FORMAT
#define DEC 1
#else /* it's not VAX */
#if HOST_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
#ifdef HOST_WORDS_BIG_ENDIAN
#define MIEEE 1
#else /* not big-endian */
#define IBMPC 1
#endif /*  big-endian */
#else /* it's not IEEE either */
unknown arithmetic type
#define UNK 1
#endif /* not IEEE */
#endif /* not VAX */

#endif /* REAL_ARITHMETIC not defined */

/* Define INFINITY for support of infinity.
   Define NANS for support of Not-a-Number's (NaN's).  */
#ifndef DEC
#define INFINITY
#define NANS
#endif

/* Support of NaNs requires support of infinity. */
#ifdef NANS
#ifndef INFINITY
#define INFINITY
#endif
#endif

/* ehead.h
 *
 * Include file for extended precision arithmetic programs.
 */

/* Number of 16 bit words in external e type format */
#define NE 6

/* Number of 16 bit words in internal format */
#define NI (NE+3)

/* Array offset to exponent */
#define E 1

/* Array offset to high guard word */
#define M 2

/* Number of bits of precision */
#define NBITS ((NI-4)*16)

/* Maximum number of decimal digits in ASCII conversion
 * = NBITS*log10(2)
 */
#define NDEC (NBITS*8/27)

/* The exponent of 1.0 */
#define EXONE (0x3fff)

/* Find a host integer type that is at least 16 bits wide,
   and another type at least twice whatever that size is. */

#if HOST_BITS_PER_CHAR >= 16
#define EMUSHORT char
#define EMUSHORT_SIZE HOST_BITS_PER_CHAR
#define EMULONG_SIZE (2 * HOST_BITS_PER_CHAR)
#else
#if HOST_BITS_PER_SHORT >= 16
#define EMUSHORT short
#define EMUSHORT_SIZE HOST_BITS_PER_SHORT
#define EMULONG_SIZE (2 * HOST_BITS_PER_SHORT)
#else
#if HOST_BITS_PER_INT >= 16
#define EMUSHORT int
#define EMUSHORT_SIZE HOST_BITS_PER_INT
#define EMULONG_SIZE (2 * HOST_BITS_PER_INT)
#else
#if HOST_BITS_PER_LONG >= 16
#define EMUSHORT long
#define EMUSHORT_SIZE HOST_BITS_PER_LONG
#define EMULONG_SIZE (2 * HOST_BITS_PER_LONG)
#else
/*  You will have to modify this program to have a smaller unit size. */
#define EMU_NON_COMPILE
#endif
#endif
#endif
#endif

#if HOST_BITS_PER_SHORT >= EMULONG_SIZE
#define EMULONG short
#else
#if HOST_BITS_PER_INT >= EMULONG_SIZE
#define EMULONG int
#else
#if HOST_BITS_PER_LONG >= EMULONG_SIZE
#define EMULONG long
#else
#if HOST_BITS_PER_LONG_LONG >= EMULONG_SIZE
#define EMULONG long long int
#else
/*  You will have to modify this program to have a smaller unit size. */
#define EMU_NON_COMPILE
#endif
#endif
#endif
#endif


/* The host interface doesn't work if no 16-bit size exists. */
#if EMUSHORT_SIZE != 16
#define EMU_NON_COMPILE
#endif

/* OK to continue compilation. */
#ifndef EMU_NON_COMPILE

/* Construct macros to translate between REAL_VALUE_TYPE and e type.
   In GET_REAL and PUT_REAL, r and e are pointers.
   A REAL_VALUE_TYPE is guaranteed to occupy contiguous locations
   in memory, with no holes.  */

#if LONG_DOUBLE_TYPE_SIZE == 96
#define GET_REAL(r,e) bcopy (r, e, 2*NE)
#define PUT_REAL(e,r) bcopy (e, r, 2*NE)
#else /* no XFmode */

#ifdef REAL_ARITHMETIC
/* Emulator uses target format internally
   but host stores it in host endian-ness. */

#if defined (HOST_WORDS_BIG_ENDIAN) == WORDS_BIG_ENDIAN
#define GET_REAL(r,e) e53toe ((r), (e))
#define PUT_REAL(e,r) etoe53 ((e), (r))

#else /* endian-ness differs */
/* emulator uses target endian-ness internally */
#define GET_REAL(r,e)		\
do { EMUSHORT w[4];		\
 w[3] = ((EMUSHORT *) r)[0];	\
 w[2] = ((EMUSHORT *) r)[1];	\
 w[1] = ((EMUSHORT *) r)[2];	\
 w[0] = ((EMUSHORT *) r)[3];	\
 e53toe (w, (e)); } while (0)

#define PUT_REAL(e,r)		\
do { EMUSHORT w[4];		\
 etoe53 ((e), w);		\
 *((EMUSHORT *) r) = w[3];	\
 *((EMUSHORT *) r + 1) = w[2];	\
 *((EMUSHORT *) r + 2) = w[1];	\
 *((EMUSHORT *) r + 3) = w[0]; } while (0)

#endif /* endian-ness differs */

#else /* not REAL_ARITHMETIC */

/* emulator uses host format */
#define GET_REAL(r,e) e53toe ((r), (e))
#define PUT_REAL(e,r) etoe53 ((e), (r))

#endif /* not REAL_ARITHMETIC */
#endif /* no XFmode */

void warning ();
extern int extra_warnings;
int ecmp (), enormlz (), eshift ();
int eisneg (), eisinf (), eisnan (), eiisinf (), eiisnan ();
void eadd (), esub (), emul (), ediv ();
void eshup1 (), eshup8 (), eshup6 (), eshdn1 (), eshdn8 (), eshdn6 ();
void eabs (), eneg (), emov (), eclear (), einfin (), efloor ();
void eldexp (), efrexp (), eifrac (), euifrac (), ltoe (), ultoe ();
void eround (), ereal_to_decimal (), eiinfin (), einan ();
void esqrt (), elog (), eexp (), etanh (), epow ();
void asctoe (), asctoe24 (), asctoe53 (), asctoe64 ();
void etoasc (), e24toasc (), e53toasc (), e64toasc ();
void etoe64 (), etoe53 (), etoe24 (), e64toe (), e53toe (), e24toe ();
void mtherr (), make_nan ();
void enan ();
extern unsigned EMUSHORT ezero[], ehalf[], eone[], etwo[];
extern unsigned EMUSHORT elog2[], esqrt2[];

/* Pack output array with 32-bit numbers obtained from
   array containing 16-bit numbers, swapping ends if required. */
void 
endian (e, x, mode)
     unsigned EMUSHORT e[];
     long x[];
     enum machine_mode mode;
{
  unsigned long th, t;

#if WORDS_BIG_ENDIAN
  switch (mode)
    {

    case XFmode:

      /* Swap halfwords in the third long. */
      th = (unsigned long) e[4] & 0xffff;
      t = (unsigned long) e[5] & 0xffff;
      t |= th << 16;
      x[2] = (long) t;
      /* fall into the double case */

    case DFmode:

      /* swap halfwords in the second word */
      th = (unsigned long) e[2] & 0xffff;
      t = (unsigned long) e[3] & 0xffff;
      t |= th << 16;
      x[1] = (long) t;
      /* fall into the float case */

    case SFmode:

      /* swap halfwords in the first word */
      th = (unsigned long) e[0] & 0xffff;
      t = (unsigned long) e[1] & 0xffff;
      t |= th << 16;
      x[0] = t;
      break;

    default:
      abort ();
    }

#else

  /* Pack the output array without swapping. */

  switch (mode)
    {

    case XFmode:

      /* Pack the third long.
	 Each element of the input REAL_VALUE_TYPE array has 16 bit useful bits
	 in it.  */
      th = (unsigned long) e[5] & 0xffff;
      t = (unsigned long) e[4] & 0xffff;
      t |= th << 16;
      x[2] = (long) t;
      /* fall into the double case */

    case DFmode:

      /* pack the second long */
      th = (unsigned long) e[3] & 0xffff;
      t = (unsigned long) e[2] & 0xffff;
      t |= th << 16;
      x[1] = (long) t;
      /* fall into the float case */

    case SFmode:

      /* pack the first long */
      th = (unsigned long) e[1] & 0xffff;
      t = (unsigned long) e[0] & 0xffff;
      t |= th << 16;
      x[0] = t;
      break;

    default:
      abort ();
    }

#endif
}


/* This is the implementation of the REAL_ARITHMETIC macro.
 */
void 
earith (value, icode, r1, r2)
     REAL_VALUE_TYPE *value;
     int icode;
     REAL_VALUE_TYPE *r1;
     REAL_VALUE_TYPE *r2;
{
  unsigned EMUSHORT d1[NE], d2[NE], v[NE];
  enum tree_code code;

  GET_REAL (r1, d1);
  GET_REAL (r2, d2);
#ifdef NANS
/*  Return NaN input back to the caller. */
  if (eisnan (d1))
    {
      PUT_REAL (d1, value);
      return;
    }
  if (eisnan (d2))
    {
      PUT_REAL (d2, value);
      return;
    }
#endif
  code = (enum tree_code) icode;
  switch (code)
    {
    case PLUS_EXPR:
      eadd (d2, d1, v);
      break;

    case MINUS_EXPR:
      esub (d2, d1, v);		/* d1 - d2 */
      break;

    case MULT_EXPR:
      emul (d2, d1, v);
      break;

    case RDIV_EXPR:
#ifndef REAL_INFINITY
      if (ecmp (d2, ezero) == 0)
	{
#ifdef NANS
	enan (v);
	break;
#else
	abort ();
#endif
	}
#endif
      ediv (d2, d1, v);	/* d1/d2 */
      break;

    case MIN_EXPR:		/* min (d1,d2) */
      if (ecmp (d1, d2) < 0)
	emov (d1, v);
      else
	emov (d2, v);
      break;

    case MAX_EXPR:		/* max (d1,d2) */
      if (ecmp (d1, d2) > 0)
	emov (d1, v);
      else
	emov (d2, v);
      break;
    default:
      emov (ezero, v);
      break;
    }
PUT_REAL (v, value);
}


/* Truncate REAL_VALUE_TYPE toward zero to signed HOST_WIDE_INT
 * implements REAL_VALUE_RNDZINT (x) (etrunci (x))
 */
REAL_VALUE_TYPE 
etrunci (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  REAL_VALUE_TYPE r;
  long l;

  GET_REAL (&x, g);
#ifdef NANS
  if (eisnan (g))
    return (x);
#endif
  eifrac (g, &l, f);
  ltoe (&l, g);
  PUT_REAL (g, &r);
  return (r);
}


/* Truncate REAL_VALUE_TYPE toward zero to unsigned HOST_WIDE_INT
 * implements REAL_VALUE_UNSIGNED_RNDZINT (x) (etruncui (x))
 */
REAL_VALUE_TYPE 
etruncui (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  REAL_VALUE_TYPE r;
  unsigned long l;

  GET_REAL (&x, g);
#ifdef NANS
  if (eisnan (g))
    return (x);
#endif
  euifrac (g, &l, f);
  ultoe (&l, g);
  PUT_REAL (g, &r);
  return (r);
}


/* This is the REAL_VALUE_ATOF function.
 * It converts a decimal string to binary, rounding off
 * as indicated by the machine_mode argument.  Then it
 * promotes the rounded value to REAL_VALUE_TYPE.
 */
REAL_VALUE_TYPE 
ereal_atof (s, t)
     char *s;
     enum machine_mode t;
{
  unsigned EMUSHORT tem[NE], e[NE];
  REAL_VALUE_TYPE r;

  switch (t)
    {
    case SFmode:
      asctoe24 (s, tem);
      e24toe (tem, e);
      break;
    case DFmode:
      asctoe53 (s, tem);
      e53toe (tem, e);
      break;
    case XFmode:
      asctoe64 (s, tem);
      e64toe (tem, e);
      break;
    default:
      asctoe (s, e);
    }
  PUT_REAL (e, &r);
  return (r);
}


/* Expansion of REAL_NEGATE.
 */
REAL_VALUE_TYPE 
ereal_negate (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT e[NE];
  REAL_VALUE_TYPE r;

  GET_REAL (&x, e);
#ifdef NANS
  if (eisnan (e))
    return (x);
#endif
  eneg (e);
  PUT_REAL (e, &r);
  return (r);
}


/* Round real to int
 * implements REAL_VALUE_FIX (x) (eroundi (x))
 * The type of rounding is left unspecified by real.h.
 * It is implemented here as round to nearest (add .5 and chop).
 */
int 
eroundi (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  EMULONG l;

  GET_REAL (&x, f);
#ifdef NANS
  if (eisnan (f))
    {
      warning ("conversion from NaN to int");
      return (-1);
    }
#endif
  eround (f, g);
  eifrac (g, &l, f);
  return ((int) l);
}

/* Round real to nearest unsigned int
 * implements  REAL_VALUE_UNSIGNED_FIX (x) ((unsigned int) eroundi (x))
 * Negative input returns zero.
 * The type of rounding is left unspecified by real.h.
 * It is implemented here as round to nearest (add .5 and chop).
 */
unsigned int 
eroundui (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  unsigned EMULONG l;

  GET_REAL (&x, f);
#ifdef NANS
  if (eisnan (f))
    {
      warning ("conversion from NaN to unsigned int");
      return (-1);
    }
#endif
  eround (f, g);
  euifrac (g, &l, f);
  return ((unsigned int)l);
}


/* REAL_VALUE_FROM_INT macro.
 */
void 
ereal_from_int (d, i, j)
     REAL_VALUE_TYPE *d;
     long i, j;
{
  unsigned EMUSHORT df[NE], dg[NE];
  long low, high;
  int sign;

  sign = 0;
  low = i;
  if ((high = j) < 0)
    {
      sign = 1;
      /* complement and add 1 */
      high = ~high;
      if (low)
	low = -low;
      else
	high += 1;
    }
  eldexp (eone, HOST_BITS_PER_LONG, df);
  ultoe (&high, dg);
  emul (dg, df, dg);
  ultoe (&low, df);
  eadd (df, dg, dg);
  if (sign)
    eneg (dg);
  PUT_REAL (dg, d);
}


/* REAL_VALUE_FROM_UNSIGNED_INT macro.
 */
void 
ereal_from_uint (d, i, j)
     REAL_VALUE_TYPE *d;
     unsigned long i, j;
{
  unsigned EMUSHORT df[NE], dg[NE];
  unsigned long low, high;

  low = i;
  high = j;
  eldexp (eone, HOST_BITS_PER_LONG, df);
  ultoe (&high, dg);
  emul (dg, df, dg);
  ultoe (&low, df);
  eadd (df, dg, dg);
  PUT_REAL (dg, d);
}


/* REAL_VALUE_TO_INT macro
 */
void 
ereal_to_int (low, high, rr)
     long *low, *high;
     REAL_VALUE_TYPE rr;
{
  unsigned EMUSHORT d[NE], df[NE], dg[NE], dh[NE];
  int s;

  GET_REAL (&rr, d);
#ifdef NANS
  if (eisnan (d))
    {
      warning ("conversion from NaN to int");
      *low = -1;
      *high = -1;
      return;
    }
#endif
  /* convert positive value */
  s = 0;
  if (eisneg (d))
    {
      eneg (d);
      s = 1;
    }
  eldexp (eone, HOST_BITS_PER_LONG, df);
  ediv (df, d, dg);		/* dg = d / 2^32 is the high word */
  euifrac (dg, high, dh);
  emul (df, dh, dg);		/* fractional part is the low word */
  euifrac (dg, low, dh);
  if (s)
    {
      /* complement and add 1 */
      *high = ~(*high);
      if (*low)
	*low = -(*low);
      else
	*high += 1;
    }
}


/* REAL_VALUE_LDEXP macro.
 */
REAL_VALUE_TYPE
ereal_ldexp (x, n)
     REAL_VALUE_TYPE x;
     int n;
{
  unsigned EMUSHORT e[NE], y[NE];
  REAL_VALUE_TYPE r;

  GET_REAL (&x, e);
#ifdef NANS
  if (eisnan (e))
    return (x);
#endif
  eldexp (e, n, y);
  PUT_REAL (y, &r);
  return (r);
}

/* These routines are conditionally compiled because functions
 * of the same names may be defined in fold-const.c.  */
#ifdef REAL_ARITHMETIC

/* Check for infinity in a REAL_VALUE_TYPE. */
int
target_isinf (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT e[NE];

#ifdef INFINITY
  GET_REAL (&x, e);
  return (eisinf (e));
#else
  return 0;
#endif
}


/* Check whether a REAL_VALUE_TYPE item is a NaN. */

int
target_isnan (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT e[NE];

#ifdef NANS
  GET_REAL (&x, e);
  return (eisnan (e));
#else
  return (0);
#endif
}


/* Check for a negative REAL_VALUE_TYPE number.
 * this means strictly less than zero, not -0.
 */

int
target_negative (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT e[NE];

  GET_REAL (&x, e);
  if (ecmp (e, ezero) == -1)
    return (1);
  return (0);
}

/* Expansion of REAL_VALUE_TRUNCATE.
 * The result is in floating point, rounded to nearest or even.
 */
REAL_VALUE_TYPE
real_value_truncate (mode, arg)
     enum machine_mode mode;
     REAL_VALUE_TYPE arg;
{
  unsigned EMUSHORT e[NE], t[NE];
  REAL_VALUE_TYPE r;

  GET_REAL (&arg, e);
#ifdef NANS
  if (eisnan (e))
    return (arg);
#endif
  eclear (t);
  switch (mode)
    {
    case XFmode:
      etoe64 (e, t);
      e64toe (t, t);
      break;

    case DFmode:
      etoe53 (e, t);
      e53toe (t, t);
      break;

    case SFmode:
      etoe24 (e, t);
      e24toe (t, t);
      break;

    case SImode:
      r = etrunci (e);
      return (r);

    default:
      abort ();
    }
  PUT_REAL (t, &r);
  return (r);
}

#endif /* REAL_ARITHMETIC defined */

/* Target values are arrays of host longs. A long is guaranteed
   to be at least 32 bits wide. */
void 
etarldouble (r, l)
     REAL_VALUE_TYPE r;
     long l[];
{
  unsigned EMUSHORT e[NE];

  GET_REAL (&r, e);
  etoe64 (e, e);
  endian (e, l, XFmode);
}

void 
etardouble (r, l)
     REAL_VALUE_TYPE r;
     long l[];
{
  unsigned EMUSHORT e[NE];

  GET_REAL (&r, e);
  etoe53 (e, e);
  endian (e, l, DFmode);
}

long
etarsingle (r)
     REAL_VALUE_TYPE r;
{
  unsigned EMUSHORT e[NE];
  unsigned long l;

  GET_REAL (&r, e);
  etoe24 (e, e);
  endian (e, &l, SFmode);
  return ((long) l);
}

void
ereal_to_decimal (x, s)
     REAL_VALUE_TYPE x;
     char *s;
{
  unsigned EMUSHORT e[NE];

  GET_REAL (&x, e);
  etoasc (e, s, 20);
}

int
ereal_cmp (x, y)
     REAL_VALUE_TYPE x, y;
{
  unsigned EMUSHORT ex[NE], ey[NE];

  GET_REAL (&x, ex);
  GET_REAL (&y, ey);
  return (ecmp (ex, ey));
}

int
ereal_isneg (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT ex[NE];

  GET_REAL (&x, ex);
  return (eisneg (ex));
}

/* End of REAL_ARITHMETIC interface */

/*							ieee.c
 *
 *    Extended precision IEEE binary floating point arithmetic routines
 *
 * Numbers are stored in C language as arrays of 16-bit unsigned
 * short integers.  The arguments of the routines are pointers to
 * the arrays.
 *
 *
 * External e type data structure, simulates Intel 8087 chip
 * temporary real format but possibly with a larger significand:
 *
 *	NE-1 significand words	(least significant word first,
 *				 most significant bit is normally set)
 *	exponent		(value = EXONE for 1.0,
 *				top bit is the sign)
 *
 *
 * Internal data structure of a number (a "word" is 16 bits):
 *
 * ei[0]	sign word	(0 for positive, 0xffff for negative)
 * ei[1]	biased exponent	(value = EXONE for the number 1.0)
 * ei[2]	high guard word	(always zero after normalization)
 * ei[3]
 * to ei[NI-2]	significand	(NI-4 significand words,
 *				 most significant word first,
 *				 most significant bit is set)
 * ei[NI-1]	low guard word	(0x8000 bit is rounding place)
 *
 *
 *
 *		Routines for external format numbers
 *
 *	asctoe (string, e)	ASCII string to extended double e type
 *	asctoe64 (string, &d)	ASCII string to long double
 *	asctoe53 (string, &d)	ASCII string to double
 *	asctoe24 (string, &f)	ASCII string to single
 *	asctoeg (string, e, prec) ASCII string to specified precision
 *	e24toe (&f, e)		IEEE single precision to e type
 *	e53toe (&d, e)		IEEE double precision to e type
 *	e64toe (&d, e)		IEEE long double precision to e type
 *	eabs (e)			absolute value
 *	eadd (a, b, c)		c = b + a
 *	eclear (e)		e = 0
 *	ecmp (a, b)		Returns 1 if a > b, 0 if a == b,
 *				-1 if a < b, -2 if either a or b is a NaN.
 *	ediv (a, b, c)		c = b / a
 *	efloor (a, b)		truncate to integer, toward -infinity
 *	efrexp (a, exp, s)	extract exponent and significand
 *	eifrac (e, &l, frac)    e to long integer and e type fraction
 *	euifrac (e, &l, frac)   e to unsigned long integer and e type fraction
 *	einfin (e)		set e to infinity, leaving its sign alone
 *	eldexp (a, n, b)	multiply by 2**n
 *	emov (a, b)		b = a
 *	emul (a, b, c)		c = b * a
 *	eneg (e)			e = -e
 *	eround (a, b)		b = nearest integer value to a
 *	esub (a, b, c)		c = b - a
 *	e24toasc (&f, str, n)	single to ASCII string, n digits after decimal
 *	e53toasc (&d, str, n)	double to ASCII string, n digits after decimal
 *	e64toasc (&d, str, n)	long double to ASCII string
 *	etoasc (e, str, n)	e to ASCII string, n digits after decimal
 *	etoe24 (e, &f)		convert e type to IEEE single precision
 *	etoe53 (e, &d)		convert e type to IEEE double precision
 *	etoe64 (e, &d)		convert e type to IEEE long double precision
 *	ltoe (&l, e)		long (32 bit) integer to e type
 *	ultoe (&l, e)		unsigned long (32 bit) integer to e type
 *      eisneg (e)              1 if sign bit of e != 0, else 0
 *      eisinf (e)              1 if e has maximum exponent (non-IEEE)
 *				or is infinite (IEEE)
 *      eisnan (e)              1 if e is a NaN
 *
 *
 *		Routines for internal format numbers
 *
 *	eaddm (ai, bi)		add significands, bi = bi + ai
 *	ecleaz (ei)		ei = 0
 *	ecleazs (ei)		set ei = 0 but leave its sign alone
 *	ecmpm (ai, bi)		compare significands, return 1, 0, or -1
 *	edivm (ai, bi)		divide  significands, bi = bi / ai
 *	emdnorm (ai,l,s,exp)	normalize and round off
 *	emovi (a, ai)		convert external a to internal ai
 *	emovo (ai, a)		convert internal ai to external a
 *	emovz (ai, bi)		bi = ai, low guard word of bi = 0
 *	emulm (ai, bi)		multiply significands, bi = bi * ai
 *	enormlz (ei)		left-justify the significand
 *	eshdn1 (ai)		shift significand and guards down 1 bit
 *	eshdn8 (ai)		shift down 8 bits
 *	eshdn6 (ai)		shift down 16 bits
 *	eshift (ai, n)		shift ai n bits up (or down if n < 0)
 *	eshup1 (ai)		shift significand and guards up 1 bit
 *	eshup8 (ai)		shift up 8 bits
 *	eshup6 (ai)		shift up 16 bits
 *	esubm (ai, bi)		subtract significands, bi = bi - ai
 *      eiisinf (ai)            1 if infinite
 *      eiisnan (ai)            1 if a NaN
 *      einan (ai)              set ai = NaN
 *      eiinfin (ai)            set ai = infinity
 *
 *
 * The result is always normalized and rounded to NI-4 word precision
 * after each arithmetic operation.
 *
 * Exception flags are NOT fully supported.
 *
 * Signaling NaN's are NOT supported; they are treated the same
 * as quiet NaN's.
 *
 * Define INFINITY for support of infinity; otherwise a
 * saturation arithmetic is implemented.
 *
 * Define NANS for support of Not-a-Number items; otherwise the
 * arithmetic will never produce a NaN output, and might be confused
 * by a NaN input.
 * If NaN's are supported, the output of `ecmp (a,b)' is -2 if
 * either a or b is a NaN. This means asking `if (ecmp (a,b) < 0)'
 * may not be legitimate. Use `if (ecmp (a,b) == -1)' for `less than'
 * if in doubt.
 *
 * Denormals are always supported here where appropriate (e.g., not
 * for conversion to DEC numbers).
 *
 */


/*							mconf.h
 *
 *	Common include file for math routines
 *
 *
 *
 * SYNOPSIS:
 *
 * #include "mconf.h"
 *
 *
 *
 * DESCRIPTION:
 *
 * This file contains definitions for error codes that are
 * passed to the common error handling routine mtherr
 * (which see).
 *
 * The file also includes a conditional assembly definition
 * for the type of computer arithmetic (Intel IEEE, DEC, Motorola
 * IEEE, or UNKnown).
 *
 * For Digital Equipment PDP-11 and VAX computers, certain
 * IBM systems, and others that use numbers with a 56-bit
 * significand, the symbol DEC should be defined.  In this
 * mode, most floating point constants are given as arrays
 * of octal integers to eliminate decimal to binary conversion
 * errors that might be introduced by the compiler.
 *
 * For computers, such as IBM PC, that follow the IEEE
 * Standard for Binary Floating Point Arithmetic (ANSI/IEEE
 * Std 754-1985), the symbol IBMPC or MIEEE should be defined.
 * These numbers have 53-bit significands.  In this mode, constants
 * are provided as arrays of hexadecimal 16 bit integers.
 *
 * To accommodate other types of computer arithmetic, all
 * constants are also provided in a normal decimal radix
 * which one can hope are correctly converted to a suitable
 * format by the available C language compiler.  To invoke
 * this mode, the symbol UNK is defined.
 *
 * An important difference among these modes is a predefined
 * set of machine arithmetic constants for each.  The numbers
 * MACHEP (the machine roundoff error), MAXNUM (largest number
 * represented), and several other parameters are preset by
 * the configuration symbol.  Check the file const.c to
 * ensure that these values are correct for your computer.
 *
 * For ANSI C compatibility, define ANSIC equal to 1.  Currently
 * this affects only the atan2 function and others that use it.
 */

/* Constant definitions for math error conditions.  */

#define DOMAIN		1	/* argument domain error */
#define SING		2	/* argument singularity */
#define OVERFLOW	3	/* overflow range error */
#define UNDERFLOW	4	/* underflow range error */
#define TLOSS		5	/* total loss of precision */
#define PLOSS		6	/* partial loss of precision */
#define INVALID		7	/* NaN-producing operation */

/*  e type constants used by high precision check routines */

/*include "ehead.h"*/
/* 0.0 */
unsigned EMUSHORT ezero[NE] =
{
  0, 0000000, 0000000, 0000000, 0000000, 0000000,};
extern unsigned EMUSHORT ezero[];

/* 5.0E-1 */
unsigned EMUSHORT ehalf[NE] =
{
  0, 0000000, 0000000, 0000000, 0100000, 0x3ffe,};
extern unsigned EMUSHORT ehalf[];

/* 1.0E0 */
unsigned EMUSHORT eone[NE] =
{
  0, 0000000, 0000000, 0000000, 0100000, 0x3fff,};
extern unsigned EMUSHORT eone[];

/* 2.0E0 */
unsigned EMUSHORT etwo[NE] =
{
  0, 0000000, 0000000, 0000000, 0100000, 0040000,};
extern unsigned EMUSHORT etwo[];

/* 3.2E1 */
unsigned EMUSHORT e32[NE] =
{
  0, 0000000, 0000000, 0000000, 0100000, 0040004,};
extern unsigned EMUSHORT e32[];

/* 6.93147180559945309417232121458176568075500134360255E-1 */
unsigned EMUSHORT elog2[NE] =
{
  0xc9e4, 0x79ab, 0150717, 0013767, 0130562, 0x3ffe,};
extern unsigned EMUSHORT elog2[];

/* 1.41421356237309504880168872420969807856967187537695E0 */
unsigned EMUSHORT esqrt2[NE] =
{
  0x597e, 0x6484, 0174736, 0171463, 0132404, 0x3fff,};
extern unsigned EMUSHORT esqrt2[];

/* 2/sqrt (PI) =
 * 1.12837916709551257389615890312154517168810125865800E0 */
unsigned EMUSHORT eoneopi[NE] =
{
  0x71d5, 0x688d, 0012333, 0135202, 0110156, 0x3fff,};
extern unsigned EMUSHORT eoneopi[];

/* 3.14159265358979323846264338327950288419716939937511E0 */
unsigned EMUSHORT epi[NE] =
{
  0xc4c6, 0xc234, 0020550, 0155242, 0144417, 0040000,};
extern unsigned EMUSHORT epi[];

/* 5.7721566490153286060651209008240243104215933593992E-1 */
unsigned EMUSHORT eeul[NE] =
{
  0xd1be, 0xc7a4, 0076660, 0063743, 0111704, 0x3ffe,};
extern unsigned EMUSHORT eeul[];

/*
include "ehead.h"
include "mconf.h"
*/



/* Control register for rounding precision.
 * This can be set to 80 (if NE=6), 64, 56, 53, or 24 bits.
 */
int rndprc = NBITS;
extern int rndprc;

void eaddm (), esubm (), emdnorm (), asctoeg ();
static void toe24 (), toe53 (), toe64 ();
void eremain (), einit (), eiremain ();
int ecmpm (), edivm (), emulm ();
void emovi (), emovo (), emovz (), ecleaz (), ecleazs (), eadd1 ();
void etodec (), todec (), dectoe ();




void 
einit ()
{
}

/*
; Clear out entire external format number.
;
; unsigned EMUSHORT x[];
; eclear (x);
*/

void 
eclear (x)
     register unsigned EMUSHORT *x;
{
  register int i;

  for (i = 0; i < NE; i++)
    *x++ = 0;
}



/* Move external format number from a to b.
 *
 * emov (a, b);
 */

void 
emov (a, b)
     register unsigned EMUSHORT *a, *b;
{
  register int i;

  for (i = 0; i < NE; i++)
    *b++ = *a++;
}


/*
;	Absolute value of external format number
;
;	EMUSHORT x[NE];
;	eabs (x);
*/

void 
eabs (x)
     unsigned EMUSHORT x[];	/* x is the memory address of a short */
{

  x[NE - 1] &= 0x7fff;		/* sign is top bit of last word of external format */
}




/*
;	Negate external format number
;
;	unsigned EMUSHORT x[NE];
;	eneg (x);
*/

void 
eneg (x)
     unsigned EMUSHORT x[];
{

#ifdef NANS
  if (eisnan (x))
    return;
#endif
  x[NE - 1] ^= 0x8000;		/* Toggle the sign bit */
}



/* Return 1 if external format number is negative,
 * else return zero, including when it is a NaN.
 */
int 
eisneg (x)
     unsigned EMUSHORT x[];
{

#ifdef NANS
  if (eisnan (x))
    return (0);
#endif
  if (x[NE - 1] & 0x8000)
    return (1);
  else
    return (0);
}


/* Return 1 if external format number is infinity.
 * else return zero.
 */
int 
eisinf (x)
     unsigned EMUSHORT x[];
{

#ifdef NANS
  if (eisnan (x))
    return (0);
#endif
  if ((x[NE - 1] & 0x7fff) == 0x7fff)
    return (1);
  else
    return (0);
}


/* Check if e-type number is not a number.
   The bit pattern is one that we defined, so we know for sure how to
   detect it.  */

int 
eisnan (x)
     unsigned EMUSHORT x[];
{

#ifdef NANS
  int i;
/* NaN has maximum exponent */
  if ((x[NE - 1] & 0x7fff) != 0x7fff)
    return (0);
/* ... and non-zero significand field. */
  for (i = 0; i < NE - 1; i++)
    {
      if (*x++ != 0)
        return (1);
    }
#endif
  return (0);
}

/*  Fill external format number with infinity pattern (IEEE)
    or largest possible number (non-IEEE).
    Before calling einfin, you should either call eclear 
    or set up the sign bit by hand.  */

void 
einfin (x)
     register unsigned EMUSHORT *x;
{
  register int i;

#ifdef INFINITY
  for (i = 0; i < NE - 1; i++)
    *x++ = 0;
  *x |= 32767;
#else
  for (i = 0; i < NE - 1; i++)
    *x++ = 0xffff;
  *x |= 32766;
  if (rndprc < NBITS)
    {
      if (rndprc == 64)
	{
	  *(x - 5) = 0;
	}
      if (rndprc == 53)
	{
	  *(x - 4) = 0xf800;
	}
      else
	{
	  *(x - 4) = 0;
	  *(x - 3) = 0;
	  *(x - 2) = 0xff00;
	}
    }
#endif
}


/* Output an e-type NaN.
   This generates Intel's quiet NaN pattern for extended real.
   The exponent is 7fff, the leading mantissa word is c000.  */

void 
enan (x)
     register unsigned EMUSHORT *x;
{
  register int i;

  for (i = 0; i < NE - 2; i++)
    *x++ = 0;
  *x++ = 0xc000;
  *x = 0x7fff;
}


/* Move in external format number,
 * converting it to internal format.
 */
void 
emovi (a, b)
     unsigned EMUSHORT *a, *b;
{
  register unsigned EMUSHORT *p, *q;
  int i;

  q = b;
  p = a + (NE - 1);		/* point to last word of external number */
  /* get the sign bit */
  if (*p & 0x8000)
    *q++ = 0xffff;
  else
    *q++ = 0;
  /* get the exponent */
  *q = *p--;
  *q++ &= 0x7fff;		/* delete the sign bit */
#ifdef INFINITY
  if ((*(q - 1) & 0x7fff) == 0x7fff)
    {
#ifdef NANS
      if (eisnan (a))
	{
	  *q++ = 0;
	  for (i = 3; i < NI; i++)
	    *q++ = *p--;
	  return;
	}
#endif
      for (i = 2; i < NI; i++)
	*q++ = 0;
      return;
    }
#endif
  /* clear high guard word */
  *q++ = 0;
  /* move in the significand */
  for (i = 0; i < NE - 1; i++)
    *q++ = *p--;
  /* clear low guard word */
  *q = 0;
}


/* Move internal format number out,
 * converting it to external format.
 */
void 
emovo (a, b)
     unsigned EMUSHORT *a, *b;
{
  register unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT i;

  p = a;
  q = b + (NE - 1);		/* point to output exponent */
  /* combine sign and exponent */
  i = *p++;
  if (i)
    *q-- = *p++ | 0x8000;
  else
    *q-- = *p++;
#ifdef INFINITY
  if (*(p - 1) == 0x7fff)
    {
#ifdef NANS
      if (eiisnan (a))
	{
	  enan (b);
	  return;
	}
#endif
      einfin (b);
      return;
    }
#endif
  /* skip over guard word */
  ++p;
  /* move the significand */
  for (i = 0; i < NE - 1; i++)
    *q-- = *p++;
}




/* Clear out internal format number.
 */

void 
ecleaz (xi)
     register unsigned EMUSHORT *xi;
{
  register int i;

  for (i = 0; i < NI; i++)
    *xi++ = 0;
}


/* same, but don't touch the sign. */

void 
ecleazs (xi)
     register unsigned EMUSHORT *xi;
{
  register int i;

  ++xi;
  for (i = 0; i < NI - 1; i++)
    *xi++ = 0;
}



/* Move internal format number from a to b.
 */
void 
emovz (a, b)
     register unsigned EMUSHORT *a, *b;
{
  register int i;

  for (i = 0; i < NI - 1; i++)
    *b++ = *a++;
  /* clear low guard word */
  *b = 0;
}

/* Generate internal format NaN.
   The explicit pattern for this is maximum exponent and
   top two significand bits set.  */

void
einan (x)
     unsigned EMUSHORT x[];
{

  ecleaz (x);
  x[E] = 0x7fff;
  x[M + 1] = 0xc000;
}

/* Return nonzero if internal format number is a NaN. */

int 
eiisnan (x)
     unsigned EMUSHORT x[];
{
  int i;

  if ((x[E] & 0x7fff) == 0x7fff)
    {
      for (i = M + 1; i < NI; i++)
	{
	  if (x[i] != 0)
	    return (1);
	}
    }
  return (0);
}

/* Fill internal format number with infinity pattern.
   This has maximum exponent and significand all zeros.  */

void
eiinfin (x)
     unsigned EMUSHORT x[];
{

  ecleaz (x);
  x[E] = 0x7fff;
}

/* Return nonzero if internal format number is infinite. */

int 
eiisinf (x)
     unsigned EMUSHORT x[];
{

#ifdef NANS
  if (eiisnan (x))
    return (0);
#endif
  if ((x[E] & 0x7fff) == 0x7fff)
    return (1);
  return (0);
}


/*
;	Compare significands of numbers in internal format.
;	Guard words are included in the comparison.
;
;	unsigned EMUSHORT a[NI], b[NI];
;	cmpm (a, b);
;
;	for the significands:
;	returns	+1 if a > b
;		 0 if a == b
;		-1 if a < b
*/
int
ecmpm (a, b)
     register unsigned EMUSHORT *a, *b;
{
  int i;

  a += M;			/* skip up to significand area */
  b += M;
  for (i = M; i < NI; i++)
    {
      if (*a++ != *b++)
	goto difrnt;
    }
  return (0);

 difrnt:
  if (*(--a) > *(--b))
    return (1);
  else
    return (-1);
}


/*
;	Shift significand down by 1 bit
*/

void 
eshdn1 (x)
     register unsigned EMUSHORT *x;
{
  register unsigned EMUSHORT bits;
  int i;

  x += M;			/* point to significand area */

  bits = 0;
  for (i = M; i < NI; i++)
    {
      if (*x & 1)
	bits |= 1;
      *x >>= 1;
      if (bits & 2)
	*x |= 0x8000;
      bits <<= 1;
      ++x;
    }
}



/*
;	Shift significand up by 1 bit
*/

void 
eshup1 (x)
     register unsigned EMUSHORT *x;
{
  register unsigned EMUSHORT bits;
  int i;

  x += NI - 1;
  bits = 0;

  for (i = M; i < NI; i++)
    {
      if (*x & 0x8000)
	bits |= 1;
      *x <<= 1;
      if (bits & 2)
	*x |= 1;
      bits <<= 1;
      --x;
    }
}



/*
;	Shift significand down by 8 bits
*/

void 
eshdn8 (x)
     register unsigned EMUSHORT *x;
{
  register unsigned EMUSHORT newbyt, oldbyt;
  int i;

  x += M;
  oldbyt = 0;
  for (i = M; i < NI; i++)
    {
      newbyt = *x << 8;
      *x >>= 8;
      *x |= oldbyt;
      oldbyt = newbyt;
      ++x;
    }
}

/*
;	Shift significand up by 8 bits
*/

void 
eshup8 (x)
     register unsigned EMUSHORT *x;
{
  int i;
  register unsigned EMUSHORT newbyt, oldbyt;

  x += NI - 1;
  oldbyt = 0;

  for (i = M; i < NI; i++)
    {
      newbyt = *x >> 8;
      *x <<= 8;
      *x |= oldbyt;
      oldbyt = newbyt;
      --x;
    }
}

/*
;	Shift significand up by 16 bits
*/

void 
eshup6 (x)
     register unsigned EMUSHORT *x;
{
  int i;
  register unsigned EMUSHORT *p;

  p = x + M;
  x += M + 1;

  for (i = M; i < NI - 1; i++)
    *p++ = *x++;

  *p = 0;
}

/*
;	Shift significand down by 16 bits
*/

void 
eshdn6 (x)
     register unsigned EMUSHORT *x;
{
  int i;
  register unsigned EMUSHORT *p;

  x += NI - 1;
  p = x + 1;

  for (i = M; i < NI - 1; i++)
    *(--p) = *(--x);

  *(--p) = 0;
}

/*
;	Add significands
;	x + y replaces y
*/

void 
eaddm (x, y)
     unsigned EMUSHORT *x, *y;
{
  register unsigned EMULONG a;
  int i;
  unsigned int carry;

  x += NI - 1;
  y += NI - 1;
  carry = 0;
  for (i = M; i < NI; i++)
    {
      a = (unsigned EMULONG) (*x) + (unsigned EMULONG) (*y) + carry;
      if (a & 0x10000)
	carry = 1;
      else
	carry = 0;
      *y = (unsigned EMUSHORT) a;
      --x;
      --y;
    }
}

/*
;	Subtract significands
;	y - x replaces y
*/

void 
esubm (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMULONG a;
  int i;
  unsigned int carry;

  x += NI - 1;
  y += NI - 1;
  carry = 0;
  for (i = M; i < NI; i++)
    {
      a = (unsigned EMULONG) (*y) - (unsigned EMULONG) (*x) - carry;
      if (a & 0x10000)
	carry = 1;
      else
	carry = 0;
      *y = (unsigned EMUSHORT) a;
      --x;
      --y;
    }
}


/* Divide significands */

static unsigned EMUSHORT equot[NI];

int 
edivm (den, num)
     unsigned EMUSHORT den[], num[];
{
  int i;
  register unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT j;

  p = &equot[0];
  *p++ = num[0];
  *p++ = num[1];

  for (i = M; i < NI; i++)
    {
      *p++ = 0;
    }

  /* Use faster compare and subtraction if denominator
   * has only 15 bits of significance.
   */
  p = &den[M + 2];
  if (*p++ == 0)
    {
      for (i = M + 3; i < NI; i++)
	{
	  if (*p++ != 0)
	    goto fulldiv;
	}
      if ((den[M + 1] & 1) != 0)
	goto fulldiv;
      eshdn1 (num);
      eshdn1 (den);

      p = &den[M + 1];
      q = &num[M + 1];

      for (i = 0; i < NBITS + 2; i++)
	{
	  if (*p <= *q)
	    {
	      *q -= *p;
	      j = 1;
	    }
	  else
	    {
	      j = 0;
	    }
	  eshup1 (equot);
	  equot[NI - 2] |= j;
	  eshup1 (num);
	}
      goto divdon;
    }

  /* The number of quotient bits to calculate is
   * NBITS + 1 scaling guard bit + 1 roundoff bit.
   */
 fulldiv:

  p = &equot[NI - 2];
  for (i = 0; i < NBITS + 2; i++)
    {
      if (ecmpm (den, num) <= 0)
	{
	  esubm (den, num);
	  j = 1;		/* quotient bit = 1 */
	}
      else
	j = 0;
      eshup1 (equot);
      *p |= j;
      eshup1 (num);
    }

 divdon:

  eshdn1 (equot);
  eshdn1 (equot);

  /* test for nonzero remainder after roundoff bit */
  p = &num[M];
  j = 0;
  for (i = M; i < NI; i++)
    {
      j |= *p++;
    }
  if (j)
    j = 1;


  for (i = 0; i < NI; i++)
    num[i] = equot[i];
  return ((int) j);
}


/* Multiply significands */
int 
emulm (a, b)
     unsigned EMUSHORT a[], b[];
{
  unsigned EMUSHORT *p, *q;
  int i, j, k;

  equot[0] = b[0];
  equot[1] = b[1];
  for (i = M; i < NI; i++)
    equot[i] = 0;

  p = &a[NI - 2];
  k = NBITS;
  while (*p == 0)		/* significand is not supposed to be all zero */
    {
      eshdn6 (a);
      k -= 16;
    }
  if ((*p & 0xff) == 0)
    {
      eshdn8 (a);
      k -= 8;
    }

  q = &equot[NI - 1];
  j = 0;
  for (i = 0; i < k; i++)
    {
      if (*p & 1)
	eaddm (b, equot);
      /* remember if there were any nonzero bits shifted out */
      if (*q & 1)
	j |= 1;
      eshdn1 (a);
      eshdn1 (equot);
    }

  for (i = 0; i < NI; i++)
    b[i] = equot[i];

  /* return flag for lost nonzero bits */
  return (j);
}



/*
 * Normalize and round off.
 *
 * The internal format number to be rounded is "s".
 * Input "lost" indicates whether or not the number is exact.
 * This is the so-called sticky bit.
 *
 * Input "subflg" indicates whether the number was obtained
 * by a subtraction operation.  In that case if lost is nonzero
 * then the number is slightly smaller than indicated.
 *
 * Input "exp" is the biased exponent, which may be negative.
 * the exponent field of "s" is ignored but is replaced by
 * "exp" as adjusted by normalization and rounding.
 *
 * Input "rcntrl" is the rounding control.
 */

static int rlast = -1;
static int rw = 0;
static unsigned EMUSHORT rmsk = 0;
static unsigned EMUSHORT rmbit = 0;
static unsigned EMUSHORT rebit = 0;
static int re = 0;
static unsigned EMUSHORT rbit[NI];

void 
emdnorm (s, lost, subflg, exp, rcntrl)
     unsigned EMUSHORT s[];
     int lost;
     int subflg;
     EMULONG exp;
     int rcntrl;
{
  int i, j;
  unsigned EMUSHORT r;

  /* Normalize */
  j = enormlz (s);

  /* a blank significand could mean either zero or infinity. */
#ifndef INFINITY
  if (j > NBITS)
    {
      ecleazs (s);
      return;
    }
#endif
  exp -= j;
#ifndef INFINITY
  if (exp >= 32767L)
    goto overf;
#else
  if ((j > NBITS) && (exp < 32767))
    {
      ecleazs (s);
      return;
    }
#endif
  if (exp < 0L)
    {
      if (exp > (EMULONG) (-NBITS - 1))
	{
	  j = (int) exp;
	  i = eshift (s, j);
	  if (i)
	    lost = 1;
	}
      else
	{
	  ecleazs (s);
	  return;
	}
    }
  /* Round off, unless told not to by rcntrl. */
  if (rcntrl == 0)
    goto mdfin;
  /* Set up rounding parameters if the control register changed. */
  if (rndprc != rlast)
    {
      ecleaz (rbit);
      switch (rndprc)
	{
	default:
	case NBITS:
	  rw = NI - 1;		/* low guard word */
	  rmsk = 0xffff;
	  rmbit = 0x8000;
	  rbit[rw - 1] = 1;
	  re = NI - 2;
	  rebit = 1;
	  break;
	case 64:
	  rw = 7;
	  rmsk = 0xffff;
	  rmbit = 0x8000;
	  rbit[rw - 1] = 1;
	  re = rw - 1;
	  rebit = 1;
	  break;
	  /* For DEC arithmetic */
	case 56:
	  rw = 6;
	  rmsk = 0xff;
	  rmbit = 0x80;
	  rbit[rw] = 0x100;
	  re = rw;
	  rebit = 0x100;
	  break;
	case 53:
	  rw = 6;
	  rmsk = 0x7ff;
	  rmbit = 0x0400;
	  rbit[rw] = 0x800;
	  re = rw;
	  rebit = 0x800;
	  break;
	case 24:
	  rw = 4;
	  rmsk = 0xff;
	  rmbit = 0x80;
	  rbit[rw] = 0x100;
	  re = rw;
	  rebit = 0x100;
	  break;
	}
      rlast = rndprc;
    }

  if (rndprc >= 64)
    {
      r = s[rw] & rmsk;
      if (rndprc == 64)
	{
	  i = rw + 1;
	  while (i < NI)
	    {
	      if (s[i])
		r |= 1;
	      s[i] = 0;
	      ++i;
	    }
	}
    }
  else
    {
      if (exp <= 0)
	eshdn1 (s);
      r = s[rw] & rmsk;
      /* These tests assume NI = 8 */
      i = rw + 1;
      while (i < NI)
	{
	  if (s[i])
	    r |= 1;
	  s[i] = 0;
	  ++i;
	}
      /*
	 if (rndprc == 24)
	 {
	 if (s[5] || s[6])
	 r |= 1;
	 s[5] = 0;
	 s[6] = 0;
	 }
	 */
    }
  s[rw] &= ~rmsk;
  if ((r & rmbit) != 0)
    {
      if (r == rmbit)
	{
	  if (lost == 0)
	    {			/* round to even */
	      if ((s[re] & rebit) == 0)
		goto mddone;
	    }
	  else
	    {
	      if (subflg != 0)
		goto mddone;
	    }
	}
      eaddm (rbit, s);
    }
 mddone:
  if ((rndprc < 64) && (exp <= 0))
    {
      eshup1 (s);
    }
  if (s[2] != 0)
    {				/* overflow on roundoff */
      eshdn1 (s);
      exp += 1;
    }
 mdfin:
  s[NI - 1] = 0;
  if (exp >= 32767L)
    {
#ifndef INFINITY
    overf:
#endif
#ifdef INFINITY
      s[1] = 32767;
      for (i = 2; i < NI - 1; i++)
	s[i] = 0;
      if (extra_warnings)
	warning ("floating point overflow");
#else
      s[1] = 32766;
      s[2] = 0;
      for (i = M + 1; i < NI - 1; i++)
	s[i] = 0xffff;
      s[NI - 1] = 0;
      if (rndprc < 64)
	{
	  s[rw] &= ~rmsk;
	  if (rndprc == 24)
	    {
	      s[5] = 0;
	      s[6] = 0;
	    }
	}
#endif
      return;
    }
  if (exp < 0)
    s[1] = 0;
  else
    s[1] = (unsigned EMUSHORT) exp;
}



/*
;	Subtract external format numbers.
;
;	unsigned EMUSHORT a[NE], b[NE], c[NE];
;	esub (a, b, c);	 c = b - a
*/

static int subflg = 0;

void 
esub (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{

#ifdef NANS
  if (eisnan (a))
    {
      emov (a, c);
      return;
    }
  if (eisnan (b))
    {
      emov (b, c);
      return;
    }
/* Infinity minus infinity is a NaN.
   Test for subtracting infinities of the same sign. */
  if (eisinf (a) && eisinf (b)
      && ((eisneg (a) ^ eisneg (b)) == 0))
    {
      mtherr ("esub", INVALID);
      enan (c);
      return;
    }
#endif
  subflg = 1;
  eadd1 (a, b, c);
}


/*
;	Add.
;
;	unsigned EMUSHORT a[NE], b[NE], c[NE];
;	eadd (a, b, c);	 c = b + a
*/
void 
eadd (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{

#ifdef NANS
/* NaN plus anything is a NaN. */
  if (eisnan (a))
    {
      emov (a, c);
      return;
    }
  if (eisnan (b))
    {
      emov (b, c);
      return;
    }
/* Infinity minus infinity is a NaN.
   Test for adding infinities of opposite signs. */
  if (eisinf (a) && eisinf (b)
      && ((eisneg (a) ^ eisneg (b)) != 0))
    {
      mtherr ("esub", INVALID);
      enan (c);
      return;
    }
#endif
  subflg = 0;
  eadd1 (a, b, c);
}

void 
eadd1 (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{
  unsigned EMUSHORT ai[NI], bi[NI], ci[NI];
  int i, lost, j, k;
  EMULONG lt, lta, ltb;

#ifdef INFINITY
  if (eisinf (a))
    {
      emov (a, c);
      if (subflg)
	eneg (c);
      return;
    }
  if (eisinf (b))
    {
      emov (b, c);
      return;
    }
#endif
  emovi (a, ai);
  emovi (b, bi);
  if (subflg)
    ai[0] = ~ai[0];

  /* compare exponents */
  lta = ai[E];
  ltb = bi[E];
  lt = lta - ltb;
  if (lt > 0L)
    {				/* put the larger number in bi */
      emovz (bi, ci);
      emovz (ai, bi);
      emovz (ci, ai);
      ltb = bi[E];
      lt = -lt;
    }
  lost = 0;
  if (lt != 0L)
    {
      if (lt < (EMULONG) (-NBITS - 1))
	goto done;		/* answer same as larger addend */
      k = (int) lt;
      lost = eshift (ai, k);	/* shift the smaller number down */
    }
  else
    {
      /* exponents were the same, so must compare significands */
      i = ecmpm (ai, bi);
      if (i == 0)
	{			/* the numbers are identical in magnitude */
	  /* if different signs, result is zero */
	  if (ai[0] != bi[0])
	    {
	      eclear (c);
	      return;
	    }
	  /* if same sign, result is double */
	  /* double denomalized tiny number */
	  if ((bi[E] == 0) && ((bi[3] & 0x8000) == 0))
	    {
	      eshup1 (bi);
	      goto done;
	    }
	  /* add 1 to exponent unless both are zero! */
	  for (j = 1; j < NI - 1; j++)
	    {
	      if (bi[j] != 0)
		{
		  /* This could overflow, but let emovo take care of that. */
		  ltb += 1;
		  break;
		}
	    }
	  bi[E] = (unsigned EMUSHORT) ltb;
	  goto done;
	}
      if (i > 0)
	{			/* put the larger number in bi */
	  emovz (bi, ci);
	  emovz (ai, bi);
	  emovz (ci, ai);
	}
    }
  if (ai[0] == bi[0])
    {
      eaddm (ai, bi);
      subflg = 0;
    }
  else
    {
      esubm (ai, bi);
      subflg = 1;
    }
  emdnorm (bi, lost, subflg, ltb, 64);

 done:
  emovo (bi, c);
}



/*
;	Divide.
;
;	unsigned EMUSHORT a[NE], b[NE], c[NE];
;	ediv (a, b, c);	c = b / a
*/
void 
ediv (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{
  unsigned EMUSHORT ai[NI], bi[NI];
  int i;
  EMULONG lt, lta, ltb;

#ifdef NANS
/* Return any NaN input. */
  if (eisnan (a))
    {
    emov (a, c);
    return;
    }
  if (eisnan (b))
    {
    emov (b, c);
    return;
    }
/* Zero over zero, or infinity over infinity, is a NaN. */
  if (((ecmp (a, ezero) == 0) && (ecmp (b, ezero) == 0))
      || (eisinf (a) && eisinf (b)))
    {
    mtherr ("ediv", INVALID);
    enan (c);
    return;
    }
#endif
/* Infinity over anything else is infinity. */
#ifdef INFINITY
  if (eisinf (b))
    {
      if (eisneg (a) ^ eisneg (b))
	*(c + (NE - 1)) = 0x8000;
      else
	*(c + (NE - 1)) = 0;
      einfin (c);
      return;
    }
/* Anything else over infinity is zero. */
  if (eisinf (a))
    {
      eclear (c);
      return;
    }
#endif
  emovi (a, ai);
  emovi (b, bi);
  lta = ai[E];
  ltb = bi[E];
  if (bi[E] == 0)
    {				/* See if numerator is zero. */
      for (i = 1; i < NI - 1; i++)
	{
	  if (bi[i] != 0)
	    {
	      ltb -= enormlz (bi);
	      goto dnzro1;
	    }
	}
      eclear (c);
      return;
    }
 dnzro1:

  if (ai[E] == 0)
    {				/* possible divide by zero */
      for (i = 1; i < NI - 1; i++)
	{
	  if (ai[i] != 0)
	    {
	      lta -= enormlz (ai);
	      goto dnzro2;
	    }
	}
      if (ai[0] == bi[0])
	*(c + (NE - 1)) = 0;
      else
	*(c + (NE - 1)) = 0x8000;
/* Divide by zero is not an invalid operation.
   It is a divide-by-zero operation!   */
      einfin (c);
      mtherr ("ediv", SING);
      return;
    }
 dnzro2:

  i = edivm (ai, bi);
  /* calculate exponent */
  lt = ltb - lta + EXONE;
  emdnorm (bi, i, 0, lt, 64);
  /* set the sign */
  if (ai[0] == bi[0])
    bi[0] = 0;
  else
    bi[0] = 0Xffff;
  emovo (bi, c);
}



/*
;	Multiply.
;
;	unsigned EMUSHORT a[NE], b[NE], c[NE];
;	emul (a, b, c);	c = b * a
*/
void 
emul (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{
  unsigned EMUSHORT ai[NI], bi[NI];
  int i, j;
  EMULONG lt, lta, ltb;

#ifdef NANS
/* NaN times anything is the same NaN. */
  if (eisnan (a))
    {
    emov (a, c);
    return;
    }
  if (eisnan (b))
    {
    emov (b, c);
    return;
    }
/* Zero times infinity is a NaN. */
  if ((eisinf (a) && (ecmp (b, ezero) == 0))
      || (eisinf (b) && (ecmp (a, ezero) == 0)))
    {
    mtherr ("emul", INVALID);
    enan (c);
    return;
    }
#endif
/* Infinity times anything else is infinity. */
#ifdef INFINITY
  if (eisinf (a) || eisinf (b))
    {
      if (eisneg (a) ^ eisneg (b))
	*(c + (NE - 1)) = 0x8000;
      else
	*(c + (NE - 1)) = 0;
      einfin (c);
      return;
    }
#endif
  emovi (a, ai);
  emovi (b, bi);
  lta = ai[E];
  ltb = bi[E];
  if (ai[E] == 0)
    {
      for (i = 1; i < NI - 1; i++)
	{
	  if (ai[i] != 0)
	    {
	      lta -= enormlz (ai);
	      goto mnzer1;
	    }
	}
      eclear (c);
      return;
    }
 mnzer1:

  if (bi[E] == 0)
    {
      for (i = 1; i < NI - 1; i++)
	{
	  if (bi[i] != 0)
	    {
	      ltb -= enormlz (bi);
	      goto mnzer2;
	    }
	}
      eclear (c);
      return;
    }
 mnzer2:

  /* Multiply significands */
  j = emulm (ai, bi);
  /* calculate exponent */
  lt = lta + ltb - (EXONE - 1);
  emdnorm (bi, j, 0, lt, 64);
  /* calculate sign of product */
  if (ai[0] == bi[0])
    bi[0] = 0;
  else
    bi[0] = 0xffff;
  emovo (bi, c);
}




/*
; Convert IEEE double precision to e type
;	double d;
;	unsigned EMUSHORT x[N+2];
;	e53toe (&d, x);
*/
void 
e53toe (pe, y)
     unsigned EMUSHORT *pe, *y;
{
#ifdef DEC

  dectoe (pe, y);		/* see etodec.c */

#else

  register unsigned EMUSHORT r;
  register unsigned EMUSHORT *e, *p;
  unsigned EMUSHORT yy[NI];
  int denorm, k;

  e = pe;
  denorm = 0;			/* flag if denormalized number */
  ecleaz (yy);
#ifdef IBMPC
  e += 3;
#endif
  r = *e;
  yy[0] = 0;
  if (r & 0x8000)
    yy[0] = 0xffff;
  yy[M] = (r & 0x0f) | 0x10;
  r &= ~0x800f;			/* strip sign and 4 significand bits */
#ifdef INFINITY
  if (r == 0x7ff0)
    {
#ifdef NANS
#ifdef IBMPC
      if (((pe[3] & 0xf) != 0) || (pe[2] != 0)
	  || (pe[1] != 0) || (pe[0] != 0))
	{
	  enan (y);
	  return;
	}
#else
      if (((pe[0] & 0xf) != 0) || (pe[1] != 0)
	  || (pe[2] != 0) || (pe[3] != 0))
	{
	  enan (y);
	  return;
	}
#endif
#endif  /* NANS */
      eclear (y);
      einfin (y);
      if (yy[0])
	eneg (y);
      return;
    }
#endif  /* INFINITY */
  r >>= 4;
  /* If zero exponent, then the significand is denormalized.
   * So, take back the understood high significand bit. */
  if (r == 0)
    {
      denorm = 1;
      yy[M] &= ~0x10;
    }
  r += EXONE - 01777;
  yy[E] = r;
  p = &yy[M + 1];
#ifdef IBMPC
  *p++ = *(--e);
  *p++ = *(--e);
  *p++ = *(--e);
#endif
#ifdef MIEEE
  ++e;
  *p++ = *e++;
  *p++ = *e++;
  *p++ = *e++;
#endif
  eshift (yy, -5);
  if (denorm)
    {				/* if zero exponent, then normalize the significand */
      if ((k = enormlz (yy)) > NBITS)
	ecleazs (yy);
      else
	yy[E] -= (unsigned EMUSHORT) (k - 1);
    }
  emovo (yy, y);
#endif /* not DEC */
}

void 
e64toe (pe, y)
     unsigned EMUSHORT *pe, *y;
{
  unsigned EMUSHORT yy[NI];
  unsigned EMUSHORT *e, *p, *q;
  int i;

  e = pe;
  p = yy;
  for (i = 0; i < NE - 5; i++)
    *p++ = 0;
#ifdef IBMPC
  for (i = 0; i < 5; i++)
    *p++ = *e++;
#endif
#ifdef DEC
  for (i = 0; i < 5; i++)
    *p++ = *e++;
#endif
#ifdef MIEEE
  p = &yy[0] + (NE - 1);
  *p-- = *e++;
  ++e;
  for (i = 0; i < 4; i++)
    *p-- = *e++;
#endif
  p = yy;
  q = y;
#ifdef INFINITY
  if (*p == 0x7fff)
    {
#ifdef NANS
#ifdef IBMPC
      for (i = 0; i < 4; i++)
	{
	  if (pe[i] != 0)
	    {
	      enan (y);
	      return;
	    }
	}
#else
      for (i = 1; i <= 4; i++)
	{
	  if (pe[i] != 0)
	    {
	      enan (y);
	      return;
	    }
	}
#endif
#endif /* NANS */
      eclear (y);
      einfin (y);
      if (*p & 0x8000)
	eneg (y);
      return;
    }
#endif  /* INFINITY */
  for (i = 0; i < NE; i++)
    *q++ = *p++;
}


/*
; Convert IEEE single precision to e type
;	float d;
;	unsigned EMUSHORT x[N+2];
;	dtox (&d, x);
*/
void 
e24toe (pe, y)
     unsigned EMUSHORT *pe, *y;
{
  register unsigned EMUSHORT r;
  register unsigned EMUSHORT *e, *p;
  unsigned EMUSHORT yy[NI];
  int denorm, k;

  e = pe;
  denorm = 0;			/* flag if denormalized number */
  ecleaz (yy);
#ifdef IBMPC
  e += 1;
#endif
#ifdef DEC
  e += 1;
#endif
  r = *e;
  yy[0] = 0;
  if (r & 0x8000)
    yy[0] = 0xffff;
  yy[M] = (r & 0x7f) | 0200;
  r &= ~0x807f;			/* strip sign and 7 significand bits */
#ifdef INFINITY
  if (r == 0x7f80)
    {
#ifdef NANS
#ifdef MIEEE
      if (((pe[0] & 0x7f) != 0) || (pe[1] != 0))
	{
	  enan (y);
	  return;
	}
#else
      if (((pe[1] & 0x7f) != 0) || (pe[0] != 0))
	{
	  enan (y);
	  return;
	}
#endif
#endif  /* NANS */
      eclear (y);
      einfin (y);
      if (yy[0])
	eneg (y);
      return;
    }
#endif  /* INFINITY */
  r >>= 7;
  /* If zero exponent, then the significand is denormalized.
   * So, take back the understood high significand bit. */
  if (r == 0)
    {
      denorm = 1;
      yy[M] &= ~0200;
    }
  r += EXONE - 0177;
  yy[E] = r;
  p = &yy[M + 1];
#ifdef IBMPC
  *p++ = *(--e);
#endif
#ifdef DEC
  *p++ = *(--e);
#endif
#ifdef MIEEE
  ++e;
  *p++ = *e++;
#endif
  eshift (yy, -8);
  if (denorm)
    {				/* if zero exponent, then normalize the significand */
      if ((k = enormlz (yy)) > NBITS)
	ecleazs (yy);
      else
	yy[E] -= (unsigned EMUSHORT) (k - 1);
    }
  emovo (yy, y);
}


void 
etoe64 (x, e)
     unsigned EMUSHORT *x, *e;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, XFmode);
      return;
    }
#endif
  emovi (x, xi);
  /* adjust exponent for offset */
  exp = (EMULONG) xi[E];
#ifdef INFINITY
  if (eisinf (x))
    goto nonorm;
#endif
  /* round off to nearest or even */
  rndsav = rndprc;
  rndprc = 64;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
 nonorm:
  toe64 (xi, e);
}

/* move out internal format to ieee long double */
static void 
toe64 (a, b)
     unsigned EMUSHORT *a, *b;
{
  register unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT i;

#ifdef NANS
  if (eiisnan (a))
    {
      make_nan (b, XFmode);
      return;
    }
#endif
  p = a;
#ifdef MIEEE
  q = b;
#else
  q = b + 4;			/* point to output exponent */
#if LONG_DOUBLE_TYPE_SIZE == 96
  /* Clear the last two bytes of 12-byte Intel format */
  *(q+1) = 0;
#endif
#endif

  /* combine sign and exponent */
  i = *p++;
#ifdef MIEEE
  if (i)
    *q++ = *p++ | 0x8000;
  else
    *q++ = *p++;
  *q++ = 0;
#else
  if (i)
    *q-- = *p++ | 0x8000;
  else
    *q-- = *p++;
#endif
  /* skip over guard word */
  ++p;
  /* move the significand */
#ifdef MIEEE
  for (i = 0; i < 4; i++)
    *q++ = *p++;
#else
  for (i = 0; i < 4; i++)
    *q-- = *p++;
#endif
}


/*
; e type to IEEE double precision
;	double d;
;	unsigned EMUSHORT x[NE];
;	etoe53 (x, &d);
*/

#ifdef DEC

void 
etoe53 (x, e)
     unsigned EMUSHORT *x, *e;
{
  etodec (x, e);		/* see etodec.c */
}

static void 
toe53 (x, y)
     unsigned EMUSHORT *x, *y;
{
  todec (x, y);
}

#else

void 
etoe53 (x, e)
     unsigned EMUSHORT *x, *e;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, DFmode);
      return;
    }
#endif
  emovi (x, xi);
  /* adjust exponent for offsets */
  exp = (EMULONG) xi[E] - (EXONE - 0x3ff);
#ifdef INFINITY
  if (eisinf (x))
    goto nonorm;
#endif
  /* round off to nearest or even */
  rndsav = rndprc;
  rndprc = 53;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
 nonorm:
  toe53 (xi, e);
}


static void 
toe53 (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMUSHORT i;
  unsigned EMUSHORT *p;

#ifdef NANS
  if (eiisnan (x))
    {
      make_nan (y, DFmode);
      return;
    }
#endif
  p = &x[0];
#ifdef IBMPC
  y += 3;
#endif
  *y = 0;			/* output high order */
  if (*p++)
    *y = 0x8000;		/* output sign bit */

  i = *p++;
  if (i >= (unsigned int) 2047)
    {				/* Saturate at largest number less than infinity. */
#ifdef INFINITY
      *y |= 0x7ff0;
#ifdef IBMPC
      *(--y) = 0;
      *(--y) = 0;
      *(--y) = 0;
#endif
#ifdef MIEEE
      ++y;
      *y++ = 0;
      *y++ = 0;
      *y++ = 0;
#endif
#else
      *y |= (unsigned EMUSHORT) 0x7fef;
#ifdef IBMPC
      *(--y) = 0xffff;
      *(--y) = 0xffff;
      *(--y) = 0xffff;
#endif
#ifdef MIEEE
      ++y;
      *y++ = 0xffff;
      *y++ = 0xffff;
      *y++ = 0xffff;
#endif
#endif
      return;
    }
  if (i == 0)
    {
      eshift (x, 4);
    }
  else
    {
      i <<= 4;
      eshift (x, 5);
    }
  i |= *p++ & (unsigned EMUSHORT) 0x0f;	/* *p = xi[M] */
  *y |= (unsigned EMUSHORT) i;	/* high order output already has sign bit set */
#ifdef IBMPC
  *(--y) = *p++;
  *(--y) = *p++;
  *(--y) = *p;
#endif
#ifdef MIEEE
  ++y;
  *y++ = *p++;
  *y++ = *p++;
  *y++ = *p++;
#endif
}

#endif /* not DEC */



/*
; e type to IEEE single precision
;	float d;
;	unsigned EMUSHORT x[N+2];
;	xtod (x, &d);
*/
void 
etoe24 (x, e)
     unsigned EMUSHORT *x, *e;
{
  EMULONG exp;
  unsigned EMUSHORT xi[NI];
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, SFmode);
      return;
    }
#endif
  emovi (x, xi);
  /* adjust exponent for offsets */
  exp = (EMULONG) xi[E] - (EXONE - 0177);
#ifdef INFINITY
  if (eisinf (x))
    goto nonorm;
#endif
  /* round off to nearest or even */
  rndsav = rndprc;
  rndprc = 24;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
 nonorm:
  toe24 (xi, e);
}

static void 
toe24 (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMUSHORT i;
  unsigned EMUSHORT *p;

#ifdef NANS
  if (eiisnan (x))
    {
      make_nan (y, SFmode);
      return;
    }
#endif
  p = &x[0];
#ifdef IBMPC
  y += 1;
#endif
#ifdef DEC
  y += 1;
#endif
  *y = 0;			/* output high order */
  if (*p++)
    *y = 0x8000;		/* output sign bit */

  i = *p++;
/* Handle overflow cases. */
  if (i >= 255)
    {
#ifdef INFINITY
      *y |= (unsigned EMUSHORT) 0x7f80;
#ifdef IBMPC
      *(--y) = 0;
#endif
#ifdef DEC
      *(--y) = 0;
#endif
#ifdef MIEEE
      ++y;
      *y = 0;
#endif
#else  /* no INFINITY */
      *y |= (unsigned EMUSHORT) 0x7f7f;
#ifdef IBMPC
      *(--y) = 0xffff;
#endif
#ifdef DEC
      *(--y) = 0xffff;
#endif
#ifdef MIEEE
      ++y;
      *y = 0xffff;
#endif
#ifdef ERANGE
      errno = ERANGE;
#endif
#endif  /* no INFINITY */
      return;
    }
  if (i == 0)
    {
      eshift (x, 7);
    }
  else
    {
      i <<= 7;
      eshift (x, 8);
    }
  i |= *p++ & (unsigned EMUSHORT) 0x7f;	/* *p = xi[M] */
  *y |= i;			/* high order output already has sign bit set */
#ifdef IBMPC
  *(--y) = *p;
#endif
#ifdef DEC
  *(--y) = *p;
#endif
#ifdef MIEEE
  ++y;
  *y = *p;
#endif
}


/* Compare two e type numbers.
 *
 * unsigned EMUSHORT a[NE], b[NE];
 * ecmp (a, b);
 *
 *  returns +1 if a > b
 *           0 if a == b
 *          -1 if a < b
 *          -2 if either a or b is a NaN.
 */
int 
ecmp (a, b)
     unsigned EMUSHORT *a, *b;
{
  unsigned EMUSHORT ai[NI], bi[NI];
  register unsigned EMUSHORT *p, *q;
  register int i;
  int msign;

#ifdef NANS
  if (eisnan (a)  || eisnan (b))
      return (-2);
#endif
  emovi (a, ai);
  p = ai;
  emovi (b, bi);
  q = bi;

  if (*p != *q)
    {				/* the signs are different */
      /* -0 equals + 0 */
      for (i = 1; i < NI - 1; i++)
	{
	  if (ai[i] != 0)
	    goto nzro;
	  if (bi[i] != 0)
	    goto nzro;
	}
      return (0);
    nzro:
      if (*p == 0)
	return (1);
      else
	return (-1);
    }
  /* both are the same sign */
  if (*p == 0)
    msign = 1;
  else
    msign = -1;
  i = NI - 1;
  do
    {
      if (*p++ != *q++)
	{
	  goto diff;
	}
    }
  while (--i > 0);

  return (0);			/* equality */



 diff:

  if (*(--p) > *(--q))
    return (msign);		/* p is bigger */
  else
    return (-msign);		/* p is littler */
}




/* Find nearest integer to x = floor (x + 0.5)
 *
 * unsigned EMUSHORT x[NE], y[NE]
 * eround (x, y);
 */
void 
eround (x, y)
     unsigned EMUSHORT *x, *y;
{
  eadd (ehalf, x, y);
  efloor (y, y);
}




/*
; convert long integer to e type
;
;	long l;
;	unsigned EMUSHORT x[NE];
;	ltoe (&l, x);
; note &l is the memory address of l
*/
void 
ltoe (lp, y)
     long *lp;			/* lp is the memory address of a long integer */
     unsigned EMUSHORT *y;		/* y is the address of a short */
{
  unsigned EMUSHORT yi[NI];
  unsigned long ll;
  int k;

  ecleaz (yi);
  if (*lp < 0)
    {
      /* make it positive */
      ll = (unsigned long) (-(*lp));
      yi[0] = 0xffff;		/* put correct sign in the e type number */
    }
  else
    {
      ll = (unsigned long) (*lp);
    }
  /* move the long integer to yi significand area */
  yi[M] = (unsigned EMUSHORT) (ll >> 16);
  yi[M + 1] = (unsigned EMUSHORT) ll;

  yi[E] = EXONE + 15;		/* exponent if normalize shift count were 0 */
  if ((k = enormlz (yi)) > NBITS)/* normalize the significand */
    ecleaz (yi);		/* it was zero */
  else
    yi[E] -= (unsigned EMUSHORT) k;/* subtract shift count from exponent */
  emovo (yi, y);		/* output the answer */
}

/*
; convert unsigned long integer to e type
;
;	unsigned long l;
;	unsigned EMUSHORT x[NE];
;	ltox (&l, x);
; note &l is the memory address of l
*/
void 
ultoe (lp, y)
     unsigned long *lp;		/* lp is the memory address of a long integer */
     unsigned EMUSHORT *y;		/* y is the address of a short */
{
  unsigned EMUSHORT yi[NI];
  unsigned long ll;
  int k;

  ecleaz (yi);
  ll = *lp;

  /* move the long integer to ayi significand area */
  yi[M] = (unsigned EMUSHORT) (ll >> 16);
  yi[M + 1] = (unsigned EMUSHORT) ll;

  yi[E] = EXONE + 15;		/* exponent if normalize shift count were 0 */
  if ((k = enormlz (yi)) > NBITS)/* normalize the significand */
    ecleaz (yi);		/* it was zero */
  else
    yi[E] -= (unsigned EMUSHORT) k;  /* subtract shift count from exponent */
  emovo (yi, y);		/* output the answer */
}


/*
;	Find long integer and fractional parts

;	long i;
;	unsigned EMUSHORT x[NE], frac[NE];
;	xifrac (x, &i, frac);

  The integer output has the sign of the input.  The fraction is
the positive fractional part of abs (x).
*/
void 
eifrac (x, i, frac)
     unsigned EMUSHORT *x;
     long *i;
     unsigned EMUSHORT *frac;
{
  unsigned EMUSHORT xi[NI];
  int k;

  emovi (x, xi);
  k = (int) xi[E] - (EXONE - 1);
  if (k <= 0)
    {
      /* if exponent <= 0, integer = 0 and real output is fraction */
      *i = 0L;
      emovo (xi, frac);
      return;
    }
  if (k > (HOST_BITS_PER_LONG - 1))
    {
      /*
	 ;	long integer overflow: output large integer
	 ;	and correct fraction
	 */
      if (xi[0])
	*i = ((unsigned long) 1) << (HOST_BITS_PER_LONG - 1);
      else
	*i = (((unsigned long) 1) << (HOST_BITS_PER_LONG - 1)) - 1;
      eshift (xi, k);
      if (extra_warnings)
	warning ("overflow on truncation to integer");
      goto lab11;
    }

  if (k > 16)
    {
      /*
	 ; shift more than 16 bits: shift up k-16, output the integer,
	 ; then complete the shift to get the fraction.
	 */
      k -= 16;
      eshift (xi, k);

      *i = (long) (((unsigned long) xi[M] << 16) | xi[M + 1]);
      eshup6 (xi);
      goto lab10;
    }

  /* shift not more than 16 bits */
  eshift (xi, k);
  *i = (long) xi[M] & 0xffff;

 lab10:

  if (xi[0])
    *i = -(*i);
 lab11:

  xi[0] = 0;
  xi[E] = EXONE - 1;
  xi[M] = 0;
  if ((k = enormlz (xi)) > NBITS)
    ecleaz (xi);
  else
    xi[E] -= (unsigned EMUSHORT) k;

  emovo (xi, frac);
}


/*
;	Find unsigned long integer and fractional parts

;	unsigned long i;
;	unsigned EMUSHORT x[NE], frac[NE];
;	xifrac (x, &i, frac);

  A negative e type input yields integer output = 0
  but correct fraction.
*/
void 
euifrac (x, i, frac)
     unsigned EMUSHORT *x;
     long *i;
     unsigned EMUSHORT *frac;
{
  unsigned EMUSHORT xi[NI];
  int k;

  emovi (x, xi);
  k = (int) xi[E] - (EXONE - 1);
  if (k <= 0)
    {
      /* if exponent <= 0, integer = 0 and argument is fraction */
      *i = 0L;
      emovo (xi, frac);
      return;
    }
  if (k > 32)
    {
      /*
	 ;	long integer overflow: output large integer
	 ;	and correct fraction
	 */
      *i = ~(0L);
      eshift (xi, k);
      if (extra_warnings)
	warning ("overflow on truncation to unsigned integer");
      goto lab10;
    }

  if (k > 16)
    {
      /*
	 ; shift more than 16 bits: shift up k-16, output the integer,
	 ; then complete the shift to get the fraction.
	 */
      k -= 16;
      eshift (xi, k);

      *i = (long) (((unsigned long) xi[M] << 16) | xi[M + 1]);
      eshup6 (xi);
      goto lab10;
    }

  /* shift not more than 16 bits */
  eshift (xi, k);
  *i = (long) xi[M] & 0xffff;

 lab10:

  if (xi[0])
    *i = 0L;

  xi[0] = 0;
  xi[E] = EXONE - 1;
  xi[M] = 0;
  if ((k = enormlz (xi)) > NBITS)
    ecleaz (xi);
  else
    xi[E] -= (unsigned EMUSHORT) k;

  emovo (xi, frac);
}



/*
;	Shift significand
;
;	Shifts significand area up or down by the number of bits
;	given by the variable sc.
*/
int 
eshift (x, sc)
     unsigned EMUSHORT *x;
     int sc;
{
  unsigned EMUSHORT lost;
  unsigned EMUSHORT *p;

  if (sc == 0)
    return (0);

  lost = 0;
  p = x + NI - 1;

  if (sc < 0)
    {
      sc = -sc;
      while (sc >= 16)
	{
	  lost |= *p;		/* remember lost bits */
	  eshdn6 (x);
	  sc -= 16;
	}

      while (sc >= 8)
	{
	  lost |= *p & 0xff;
	  eshdn8 (x);
	  sc -= 8;
	}

      while (sc > 0)
	{
	  lost |= *p & 1;
	  eshdn1 (x);
	  sc -= 1;
	}
    }
  else
    {
      while (sc >= 16)
	{
	  eshup6 (x);
	  sc -= 16;
	}

      while (sc >= 8)
	{
	  eshup8 (x);
	  sc -= 8;
	}

      while (sc > 0)
	{
	  eshup1 (x);
	  sc -= 1;
	}
    }
  if (lost)
    lost = 1;
  return ((int) lost);
}



/*
;	normalize
;
; Shift normalizes the significand area pointed to by argument
; shift count (up = positive) is returned.
*/
int 
enormlz (x)
     unsigned EMUSHORT x[];
{
  register unsigned EMUSHORT *p;
  int sc;

  sc = 0;
  p = &x[M];
  if (*p != 0)
    goto normdn;
  ++p;
  if (*p & 0x8000)
    return (0);			/* already normalized */
  while (*p == 0)
    {
      eshup6 (x);
      sc += 16;
      /* With guard word, there are NBITS+16 bits available.
       * return true if all are zero.
       */
      if (sc > NBITS)
	return (sc);
    }
  /* see if high byte is zero */
  while ((*p & 0xff00) == 0)
    {
      eshup8 (x);
      sc += 8;
    }
  /* now shift 1 bit at a time */
  while ((*p & 0x8000) == 0)
    {
      eshup1 (x);
      sc += 1;
      if (sc > NBITS)
	{
	  mtherr ("enormlz", UNDERFLOW);
	  return (sc);
	}
    }
  return (sc);

  /* Normalize by shifting down out of the high guard word
     of the significand */
 normdn:

  if (*p & 0xff00)
    {
      eshdn8 (x);
      sc -= 8;
    }
  while (*p != 0)
    {
      eshdn1 (x);
      sc -= 1;

      if (sc < -NBITS)
	{
	  mtherr ("enormlz", OVERFLOW);
	  return (sc);
	}
    }
  return (sc);
}




/* Convert e type number to decimal format ASCII string.
 * The constants are for 64 bit precision.
 */

#define NTEN 12
#define MAXP 4096

static unsigned EMUSHORT etens[NTEN + 1][NE] =
{
  {0xc94c, 0x979a, 0x8a20, 0x5202, 0xc460, 0x7525,},	/* 10**4096 */
  {0xa74d, 0x5de4, 0xc53d, 0x3b5d, 0x9e8b, 0x5a92,},	/* 10**2048 */
  {0x650d, 0x0c17, 0x8175, 0x7586, 0xc976, 0x4d48,},
  {0xcc65, 0x91c6, 0xa60e, 0xa0ae, 0xe319, 0x46a3,},
  {0xddbc, 0xde8d, 0x9df9, 0xebfb, 0xaa7e, 0x4351,},
  {0xc66f, 0x8cdf, 0x80e9, 0x47c9, 0x93ba, 0x41a8,},
  {0x3cbf, 0xa6d5, 0xffcf, 0x1f49, 0xc278, 0x40d3,},
  {0xf020, 0xb59d, 0x2b70, 0xada8, 0x9dc5, 0x4069,},
  {0x0000, 0x0000, 0x0400, 0xc9bf, 0x8e1b, 0x4034,},
  {0x0000, 0x0000, 0x0000, 0x2000, 0xbebc, 0x4019,},
  {0x0000, 0x0000, 0x0000, 0x0000, 0x9c40, 0x400c,},
  {0x0000, 0x0000, 0x0000, 0x0000, 0xc800, 0x4005,},
  {0x0000, 0x0000, 0x0000, 0x0000, 0xa000, 0x4002,},	/* 10**1 */
};

static unsigned EMUSHORT emtens[NTEN + 1][NE] =
{
  {0x2de4, 0x9fde, 0xd2ce, 0x04c8, 0xa6dd, 0x0ad8,},	/* 10**-4096 */
  {0x4925, 0x2de4, 0x3436, 0x534f, 0xceae, 0x256b,},	/* 10**-2048 */
  {0x87a6, 0xc0bd, 0xda57, 0x82a5, 0xa2a6, 0x32b5,},
  {0x7133, 0xd21c, 0xdb23, 0xee32, 0x9049, 0x395a,},
  {0xfa91, 0x1939, 0x637a, 0x4325, 0xc031, 0x3cac,},
  {0xac7d, 0xe4a0, 0x64bc, 0x467c, 0xddd0, 0x3e55,},
  {0x3f24, 0xe9a5, 0xa539, 0xea27, 0xa87f, 0x3f2a,},
  {0x67de, 0x94ba, 0x4539, 0x1ead, 0xcfb1, 0x3f94,},
  {0x4c2f, 0xe15b, 0xc44d, 0x94be, 0xe695, 0x3fc9,},
  {0xfdc2, 0xcefc, 0x8461, 0x7711, 0xabcc, 0x3fe4,},
  {0xd3c3, 0x652b, 0xe219, 0x1758, 0xd1b7, 0x3ff1,},
  {0x3d71, 0xd70a, 0x70a3, 0x0a3d, 0xa3d7, 0x3ff8,},
  {0xcccd, 0xcccc, 0xcccc, 0xcccc, 0xcccc, 0x3ffb,},	/* 10**-1 */
};

void 
e24toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e24toe (x, w);
  etoasc (w, string, ndigs);
}


void 
e53toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e53toe (x, w);
  etoasc (w, string, ndigs);
}


void 
e64toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e64toe (x, w);
  etoasc (w, string, ndigs);
}


static char wstring[80];	/* working storage for ASCII output */

void 
etoasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  EMUSHORT digit;
  unsigned EMUSHORT y[NI], t[NI], u[NI], w[NI];
  unsigned EMUSHORT *p, *r, *ten;
  unsigned EMUSHORT sign;
  int i, j, k, expon, rndsav;
  char *s, *ss;
  unsigned EMUSHORT m;


  rndsav = rndprc;
  ss = string;
  s = wstring;
  *ss = '\0';
  *s = '\0';
#ifdef NANS
  if (eisnan (x))
    {
      sprintf (wstring, " NaN ");
      goto bxit;
    }
#endif
  rndprc = NBITS;		/* set to full precision */
  emov (x, y);			/* retain external format */
  if (y[NE - 1] & 0x8000)
    {
      sign = 0xffff;
      y[NE - 1] &= 0x7fff;
    }
  else
    {
      sign = 0;
    }
  expon = 0;
  ten = &etens[NTEN][0];
  emov (eone, t);
  /* Test for zero exponent */
  if (y[NE - 1] == 0)
    {
      for (k = 0; k < NE - 1; k++)
	{
	  if (y[k] != 0)
	    goto tnzro;		/* denormalized number */
	}
      goto isone;		/* legal all zeros */
    }
 tnzro:

  /* Test for infinity. */
  if (y[NE - 1] == 0x7fff)
    {
      if (sign)
	sprintf (wstring, " -Infinity ");
      else
	sprintf (wstring, " Infinity ");
      goto bxit;
    }

  /* Test for exponent nonzero but significand denormalized.
   * This is an error condition.
   */
  if ((y[NE - 1] != 0) && ((y[NE - 2] & 0x8000) == 0))
    {
      mtherr ("etoasc", DOMAIN);
      sprintf (wstring, "NaN");
      goto bxit;
    }

  /* Compare to 1.0 */
  i = ecmp (eone, y);
  if (i == 0)
    goto isone;

  if (i == -2)
    abort ();

  if (i < 0)
    {				/* Number is greater than 1 */
      /* Convert significand to an integer and strip trailing decimal zeros. */
      emov (y, u);
      u[NE - 1] = EXONE + NBITS - 1;

      p = &etens[NTEN - 4][0];
      m = 16;
      do
	{
	  ediv (p, u, t);
	  efloor (t, w);
	  for (j = 0; j < NE - 1; j++)
	    {
	      if (t[j] != w[j])
		goto noint;
	    }
	  emov (t, u);
	  expon += (int) m;
	noint:
	  p += NE;
	  m >>= 1;
	}
      while (m != 0);

      /* Rescale from integer significand */
      u[NE - 1] += y[NE - 1] - (unsigned int) (EXONE + NBITS - 1);
      emov (u, y);
      /* Find power of 10 */
      emov (eone, t);
      m = MAXP;
      p = &etens[0][0];
      /* An unordered compare result shouldn't happen here. */
      while (ecmp (ten, u) <= 0)
	{
	  if (ecmp (p, u) <= 0)
	    {
	      ediv (p, u, u);
	      emul (p, t, t);
	      expon += (int) m;
	    }
	  m >>= 1;
	  if (m == 0)
	    break;
	  p += NE;
	}
    }
  else
    {				/* Number is less than 1.0 */
      /* Pad significand with trailing decimal zeros. */
      if (y[NE - 1] == 0)
	{
	  while ((y[NE - 2] & 0x8000) == 0)
	    {
	      emul (ten, y, y);
	      expon -= 1;
	    }
	}
      else
	{
	  emovi (y, w);
	  for (i = 0; i < NDEC + 1; i++)
	    {
	      if ((w[NI - 1] & 0x7) != 0)
		break;
	      /* multiply by 10 */
	      emovz (w, u);
	      eshdn1 (u);
	      eshdn1 (u);
	      eaddm (w, u);
	      u[1] += 3;
	      while (u[2] != 0)
		{
		  eshdn1 (u);
		  u[1] += 1;
		}
	      if (u[NI - 1] != 0)
		break;
	      if (eone[NE - 1] <= u[1])
		break;
	      emovz (u, w);
	      expon -= 1;
	    }
	  emovo (w, y);
	}
      k = -MAXP;
      p = &emtens[0][0];
      r = &etens[0][0];
      emov (y, w);
      emov (eone, t);
      while (ecmp (eone, w) > 0)
	{
	  if (ecmp (p, w) >= 0)
	    {
	      emul (r, w, w);
	      emul (r, t, t);
	      expon += k;
	    }
	  k /= 2;
	  if (k == 0)
	    break;
	  p += NE;
	  r += NE;
	}
      ediv (t, eone, t);
    }
 isone:
  /* Find the first (leading) digit. */
  emovi (t, w);
  emovz (w, t);
  emovi (y, w);
  emovz (w, y);
  eiremain (t, y);
  digit = equot[NI - 1];
  while ((digit == 0) && (ecmp (y, ezero) != 0))
    {
      eshup1 (y);
      emovz (y, u);
      eshup1 (u);
      eshup1 (u);
      eaddm (u, y);
      eiremain (t, y);
      digit = equot[NI - 1];
      expon -= 1;
    }
  s = wstring;
  if (sign)
    *s++ = '-';
  else
    *s++ = ' ';
  /* Examine number of digits requested by caller. */
  if (ndigs < 0)
    ndigs = 0;
  if (ndigs > NDEC)
    ndigs = NDEC;
  if (digit == 10)
    {
      *s++ = '1';
      *s++ = '.';
      if (ndigs > 0)
	{
	  *s++ = '0';
	  ndigs -= 1;
	}
      expon += 1;
    }
  else
    {
      *s++ = (char )digit + '0';
      *s++ = '.';
    }
  /* Generate digits after the decimal point. */
  for (k = 0; k <= ndigs; k++)
    {
      /* multiply current number by 10, without normalizing */
      eshup1 (y);
      emovz (y, u);
      eshup1 (u);
      eshup1 (u);
      eaddm (u, y);
      eiremain (t, y);
      *s++ = (char) equot[NI - 1] + '0';
    }
  digit = equot[NI - 1];
  --s;
  ss = s;
  /* round off the ASCII string */
  if (digit > 4)
    {
      /* Test for critical rounding case in ASCII output. */
      if (digit == 5)
	{
	  emovo (y, t);
	  if (ecmp (t, ezero) != 0)
	    goto roun;		/* round to nearest */
	  if ((*(s - 1) & 1) == 0)
	    goto doexp;		/* round to even */
	}
      /* Round up and propagate carry-outs */
    roun:
      --s;
      k = *s & 0x7f;
      /* Carry out to most significant digit? */
      if (k == '.')
	{
	  --s;
	  k = *s;
	  k += 1;
	  *s = (char) k;
	  /* Most significant digit carries to 10? */
	  if (k > '9')
	    {
	      expon += 1;
	      *s = '1';
	    }
	  goto doexp;
	}
      /* Round up and carry out from less significant digits */
      k += 1;
      *s = (char) k;
      if (k > '9')
	{
	  *s = '0';
	  goto roun;
	}
    }
 doexp:
  /*
     if (expon >= 0)
     sprintf (ss, "e+%d", expon);
     else
     sprintf (ss, "e%d", expon);
     */
  sprintf (ss, "e%d", expon);
 bxit:
  rndprc = rndsav;
  /* copy out the working string */
  s = string;
  ss = wstring;
  while (*ss == ' ')		/* strip possible leading space */
    ++ss;
  while ((*s++ = *ss++) != '\0')
    ;
}




/*
;								ASCTOQ
;		ASCTOQ.MAC		LATEST REV: 11 JAN 84
;					SLM, 3 JAN 78
;
;	Convert ASCII string to quadruple precision floating point
;
;		Numeric input is free field decimal number
;		with max of 15 digits with or without
;		decimal point entered as ASCII from teletype.
;	Entering E after the number followed by a second
;	number causes the second number to be interpreted
;	as a power of 10 to be multiplied by the first number
;	(i.e., "scientific" notation).
;
;	Usage:
;		asctoq (string, q);
*/

/* ASCII to single */
void 
asctoe24 (s, y)
     char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, 24);
}


/* ASCII to double */
void 
asctoe53 (s, y)
     char *s;
     unsigned EMUSHORT *y;
{
#ifdef DEC
  asctoeg (s, y, 56);
#else
  asctoeg (s, y, 53);
#endif
}


/* ASCII to long double */
void 
asctoe64 (s, y)
     char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, 64);
}

/* ASCII to super double */
void 
asctoe (s, y)
     char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, NBITS);
}

/* Space to make a copy of the input string: */
static char lstr[82];

void 
asctoeg (ss, y, oprec)
     char *ss;
     unsigned EMUSHORT *y;
     int oprec;
{
  unsigned EMUSHORT yy[NI], xt[NI], tt[NI];
  int esign, decflg, sgnflg, nexp, exp, prec, lost;
  int k, trail, c, rndsav;
  EMULONG lexp;
  unsigned EMUSHORT nsign, *p;
  char *sp, *s;

  /* Copy the input string. */
  s = ss;
  while (*s == ' ')		/* skip leading spaces */
    ++s;
  sp = lstr;
  for (k = 0; k < 79; k++)
    {
      if ((*sp++ = *s++) == '\0')
	break;
    }
  *sp = '\0';
  s = lstr;

  rndsav = rndprc;
  rndprc = NBITS;		/* Set to full precision */
  lost = 0;
  nsign = 0;
  decflg = 0;
  sgnflg = 0;
  nexp = 0;
  exp = 0;
  prec = 0;
  ecleaz (yy);
  trail = 0;

 nxtcom:
  k = *s - '0';
  if ((k >= 0) && (k <= 9))
    {
      /* Ignore leading zeros */
      if ((prec == 0) && (decflg == 0) && (k == 0))
	goto donchr;
      /* Identify and strip trailing zeros after the decimal point. */
      if ((trail == 0) && (decflg != 0))
	{
	  sp = s;
	  while ((*sp >= '0') && (*sp <= '9'))
	    ++sp;
	  /* Check for syntax error */
	  c = *sp & 0x7f;
	  if ((c != 'e') && (c != 'E') && (c != '\0')
	      && (c != '\n') && (c != '\r') && (c != ' ')
	      && (c != ','))
	    goto error;
	  --sp;
	  while (*sp == '0')
	    *sp-- = 'z';
	  trail = 1;
	  if (*s == 'z')
	    goto donchr;
	}
      /* If enough digits were given to more than fill up the yy register,
       * continuing until overflow into the high guard word yy[2]
       * guarantees that there will be a roundoff bit at the top
       * of the low guard word after normalization.
       */
      if (yy[2] == 0)
	{
	  if (decflg)
	    nexp += 1;		/* count digits after decimal point */
	  eshup1 (yy);		/* multiply current number by 10 */
	  emovz (yy, xt);
	  eshup1 (xt);
	  eshup1 (xt);
	  eaddm (xt, yy);
	  ecleaz (xt);
	  xt[NI - 2] = (unsigned EMUSHORT) k;
	  eaddm (xt, yy);
	}
      else
	{
	  lost |= k;
	}
      prec += 1;
      goto donchr;
    }

  switch (*s)
    {
    case 'z':
      break;
    case 'E':
    case 'e':
      goto expnt;
    case '.':			/* decimal point */
      if (decflg)
	goto error;
      ++decflg;
      break;
    case '-':
      nsign = 0xffff;
      if (sgnflg)
	goto error;
      ++sgnflg;
      break;
    case '+':
      if (sgnflg)
	goto error;
      ++sgnflg;
      break;
    case ',':
    case ' ':
    case '\0':
    case '\n':
    case '\r':
      goto daldone;
    case 'i':
    case 'I':
      goto infinite;
    default:
    error:
#ifdef NANS
      einan (yy);
#else
      mtherr ("asctoe", DOMAIN);
      eclear (yy);
#endif
      goto aexit;
    }
 donchr:
  ++s;
  goto nxtcom;

  /* Exponent interpretation */
 expnt:

  esign = 1;
  exp = 0;
  ++s;
  /* check for + or - */
  if (*s == '-')
    {
      esign = -1;
      ++s;
    }
  if (*s == '+')
    ++s;
  while ((*s >= '0') && (*s <= '9'))
    {
      exp *= 10;
      exp += *s++ - '0';
      if (exp > 4956)
	{
	  if (esign < 0)
	    goto zero;
	  else
	    goto infinite;
	}
    }
  if (esign < 0)
    exp = -exp;
  if (exp > 4932)
    {
 infinite:
      ecleaz (yy);
      yy[E] = 0x7fff;		/* infinity */
      goto aexit;
    }
  if (exp < -4956)
    {
 zero:
      ecleaz (yy);
      goto aexit;
    }

 daldone:
  nexp = exp - nexp;
  /* Pad trailing zeros to minimize power of 10, per IEEE spec. */
  while ((nexp > 0) && (yy[2] == 0))
    {
      emovz (yy, xt);
      eshup1 (xt);
      eshup1 (xt);
      eaddm (yy, xt);
      eshup1 (xt);
      if (xt[2] != 0)
	break;
      nexp -= 1;
      emovz (xt, yy);
    }
  if ((k = enormlz (yy)) > NBITS)
    {
      ecleaz (yy);
      goto aexit;
    }
  lexp = (EXONE - 1 + NBITS) - k;
  emdnorm (yy, lost, 0, lexp, 64);
  /* convert to external format */


  /* Multiply by 10**nexp.  If precision is 64 bits,
   * the maximum relative error incurred in forming 10**n
   * for 0 <= n <= 324 is 8.2e-20, at 10**180.
   * For 0 <= n <= 999, the peak relative error is 1.4e-19 at 10**947.
   * For 0 >= n >= -999, it is -1.55e-19 at 10**-435.
   */
  lexp = yy[E];
  if (nexp == 0)
    {
      k = 0;
      goto expdon;
    }
  esign = 1;
  if (nexp < 0)
    {
      nexp = -nexp;
      esign = -1;
      if (nexp > 4096)
	{			/* Punt.  Can't handle this without 2 divides. */
	  emovi (etens[0], tt);
	  lexp -= tt[E];
	  k = edivm (tt, yy);
	  lexp += EXONE;
	  nexp -= 4096;
	}
    }
  p = &etens[NTEN][0];
  emov (eone, xt);
  exp = 1;
  do
    {
      if (exp & nexp)
	emul (p, xt, xt);
      p -= NE;
      exp = exp + exp;
    }
  while (exp <= MAXP);

  emovi (xt, tt);
  if (esign < 0)
    {
      lexp -= tt[E];
      k = edivm (tt, yy);
      lexp += EXONE;
    }
  else
    {
      lexp += tt[E];
      k = emulm (tt, yy);
      lexp -= EXONE - 1;
    }

 expdon:

  /* Round and convert directly to the destination type */
  if (oprec == 53)
    lexp -= EXONE - 0x3ff;
  else if (oprec == 24)
    lexp -= EXONE - 0177;
#ifdef DEC
  else if (oprec == 56)
    lexp -= EXONE - 0201;
#endif
  rndprc = oprec;
  emdnorm (yy, k, 0, lexp, 64);

 aexit:

  rndprc = rndsav;
  yy[0] = nsign;
  switch (oprec)
    {
#ifdef DEC
    case 56:
      todec (yy, y);		/* see etodec.c */
      break;
#endif
    case 53:
      toe53 (yy, y);
      break;
    case 24:
      toe24 (yy, y);
      break;
    case 64:
      toe64 (yy, y);
      break;
    case NBITS:
      emovo (yy, y);
      break;
    }
}



/* y = largest integer not greater than x
 * (truncated toward minus infinity)
 *
 * unsigned EMUSHORT x[NE], y[NE]
 *
 * efloor (x, y);
 */
static unsigned EMUSHORT bmask[] =
{
  0xffff,
  0xfffe,
  0xfffc,
  0xfff8,
  0xfff0,
  0xffe0,
  0xffc0,
  0xff80,
  0xff00,
  0xfe00,
  0xfc00,
  0xf800,
  0xf000,
  0xe000,
  0xc000,
  0x8000,
  0x0000,
};

void 
efloor (x, y)
     unsigned EMUSHORT x[], y[];
{
  register unsigned EMUSHORT *p;
  int e, expon, i;
  unsigned EMUSHORT f[NE];

  emov (x, f);			/* leave in external format */
  expon = (int) f[NE - 1];
  e = (expon & 0x7fff) - (EXONE - 1);
  if (e <= 0)
    {
      eclear (y);
      goto isitneg;
    }
  /* number of bits to clear out */
  e = NBITS - e;
  emov (f, y);
  if (e <= 0)
    return;

  p = &y[0];
  while (e >= 16)
    {
      *p++ = 0;
      e -= 16;
    }
  /* clear the remaining bits */
  *p &= bmask[e];
  /* truncate negatives toward minus infinity */
 isitneg:

  if ((unsigned EMUSHORT) expon & (unsigned EMUSHORT) 0x8000)
    {
      for (i = 0; i < NE - 1; i++)
	{
	  if (f[i] != y[i])
	    {
	      esub (eone, y, y);
	      break;
	    }
	}
    }
}


/* unsigned EMUSHORT x[], s[];
 * int *exp;
 *
 * efrexp (x, exp, s);
 *
 * Returns s and exp such that  s * 2**exp = x and .5 <= s < 1.
 * For example, 1.1 = 0.55 * 2**1
 * Handles denormalized numbers properly using long integer exp.
 */
void 
efrexp (x, exp, s)
     unsigned EMUSHORT x[];
     int *exp;
     unsigned EMUSHORT s[];
{
  unsigned EMUSHORT xi[NI];
  EMULONG li;

  emovi (x, xi);
  li = (EMULONG) ((EMUSHORT) xi[1]);

  if (li == 0)
    {
      li -= enormlz (xi);
    }
  xi[1] = 0x3ffe;
  emovo (xi, s);
  *exp = (int) (li - 0x3ffe);
}



/* unsigned EMUSHORT x[], y[];
 * long pwr2;
 *
 * eldexp (x, pwr2, y);
 *
 * Returns y = x * 2**pwr2.
 */
void 
eldexp (x, pwr2, y)
     unsigned EMUSHORT x[];
     int pwr2;
     unsigned EMUSHORT y[];
{
  unsigned EMUSHORT xi[NI];
  EMULONG li;
  int i;

  emovi (x, xi);
  li = xi[1];
  li += pwr2;
  i = 0;
  emdnorm (xi, i, i, li, 64);
  emovo (xi, y);
}


/* c = remainder after dividing b by a
 * Least significant integer quotient bits left in equot[].
 */
void 
eremain (a, b, c)
     unsigned EMUSHORT a[], b[], c[];
{
  unsigned EMUSHORT den[NI], num[NI];

#ifdef NANS
  if ( eisinf (b)
       || (ecmp (a, ezero) == 0)
       || eisnan (a)
       || eisnan (b))
    {
      enan (c);
      return;
    }
#endif
  if (ecmp (a, ezero) == 0)
    {
      mtherr ("eremain", SING);
      eclear (c);
      return;
    }
  emovi (a, den);
  emovi (b, num);
  eiremain (den, num);
  /* Sign of remainder = sign of quotient */
  if (a[0] == b[0])
    num[0] = 0;
  else
    num[0] = 0xffff;
  emovo (num, c);
}

void 
eiremain (den, num)
     unsigned EMUSHORT den[], num[];
{
  EMULONG ld, ln;
  unsigned EMUSHORT j;

  ld = den[E];
  ld -= enormlz (den);
  ln = num[E];
  ln -= enormlz (num);
  ecleaz (equot);
  while (ln >= ld)
    {
      if (ecmpm (den, num) <= 0)
	{
	  esubm (den, num);
	  j = 1;
	}
      else
	{
	  j = 0;
	}
      eshup1 (equot);
      equot[NI - 1] |= j;
      eshup1 (num);
      ln -= 1;
    }
  emdnorm (num, 0, 0, ln, 0);
}

/*							mtherr.c
 *
 *	Library common error handling routine
 *
 *
 *
 * SYNOPSIS:
 *
 * char *fctnam;
 * int code;
 * void mtherr ();
 *
 * mtherr (fctnam, code);
 *
 *
 *
 * DESCRIPTION:
 *
 * This routine may be called to report one of the following
 * error conditions (in the include file mconf.h).
 *
 *   Mnemonic        Value          Significance
 *
 *    DOMAIN            1       argument domain error
 *    SING              2       function singularity
 *    OVERFLOW          3       overflow range error
 *    UNDERFLOW         4       underflow range error
 *    TLOSS             5       total loss of precision
 *    PLOSS             6       partial loss of precision
 *    INVALID           7       NaN - producing operation
 *    EDOM             33       Unix domain error code
 *    ERANGE           34       Unix range error code
 *
 * The default version of the file prints the function name,
 * passed to it by the pointer fctnam, followed by the
 * error condition.  The display is directed to the standard
 * output device.  The routine then returns to the calling
 * program.  Users may wish to modify the program to abort by
 * calling exit under severe error conditions such as domain
 * errors.
 *
 * Since all error conditions pass control to this function,
 * the display may be easily changed, eliminated, or directed
 * to an error logging device.
 *
 * SEE ALSO:
 *
 * mconf.h
 *
 */

/*
Cephes Math Library Release 2.0:  April, 1987
Copyright 1984, 1987 by Stephen L. Moshier
Direct inquiries to 30 Frost Street, Cambridge, MA 02140
*/

/* include "mconf.h" */

/* Notice: the order of appearance of the following
 * messages is bound to the error codes defined
 * in mconf.h.
 */
#define NMSGS 8
static char *ermsg[NMSGS] =
{
  "unknown",			/* error code 0 */
  "domain",			/* error code 1 */
  "singularity",		/* et seq.      */
  "overflow",
  "underflow",
  "total loss of precision",
  "partial loss of precision",
  "invalid operation"
};

int merror = 0;
extern int merror;

void 
mtherr (name, code)
     char *name;
     int code;
{
  char errstr[80];

  /* Display string passed by calling program,
   * which is supposed to be the name of the
   * function in which the error occurred.
   */

  /* Display error message defined
   * by the code argument.
   */
  if ((code <= 0) || (code >= NMSGS))
    code = 0;
  sprintf (errstr, " %s %s error", name, ermsg[code]);
  if (extra_warnings)
    warning (errstr);
  /* Set global error message word */
  merror = code + 1;

  /* Return to calling
   * program
   */
}

/* Here is etodec.c .
 *
 */

/*
;	convert DEC double precision to e type
;	double d;
;	EMUSHORT e[NE];
;	dectoe (&d, e);
*/
void 
dectoe (d, e)
     unsigned EMUSHORT *d;
     unsigned EMUSHORT *e;
{
  unsigned EMUSHORT y[NI];
  register unsigned EMUSHORT r, *p;

  ecleaz (y);			/* start with a zero */
  p = y;			/* point to our number */
  r = *d;			/* get DEC exponent word */
  if (*d & (unsigned int) 0x8000)
    *p = 0xffff;		/* fill in our sign */
  ++p;				/* bump pointer to our exponent word */
  r &= 0x7fff;			/* strip the sign bit */
  if (r == 0)			/* answer = 0 if high order DEC word = 0 */
    goto done;


  r >>= 7;			/* shift exponent word down 7 bits */
  r += EXONE - 0201;		/* subtract DEC exponent offset */
  /* add our e type exponent offset */
  *p++ = r;			/* to form our exponent */

  r = *d++;			/* now do the high order mantissa */
  r &= 0177;			/* strip off the DEC exponent and sign bits */
  r |= 0200;			/* the DEC understood high order mantissa bit */
  *p++ = r;			/* put result in our high guard word */

  *p++ = *d++;			/* fill in the rest of our mantissa */
  *p++ = *d++;
  *p = *d;

  eshdn8 (y);			/* shift our mantissa down 8 bits */
 done:
  emovo (y, e);
}



/*
;	convert e type to DEC double precision
;	double d;
;	EMUSHORT e[NE];
;	etodec (e, &d);
*/
#if 0
static unsigned EMUSHORT decbit[NI] = {0, 0, 0, 0, 0, 0, 0200, 0};

void 
etodec (x, d)
     unsigned EMUSHORT *x, *d;
{
  unsigned EMUSHORT xi[NI];
  register unsigned EMUSHORT r;
  int i, j;

  emovi (x, xi);
  *d = 0;
  if (xi[0] != 0)
    *d = 0100000;
  r = xi[E];
  if (r < (EXONE - 128))
    goto zout;
  i = xi[M + 4];
  if ((i & 0200) != 0)
    {
      if ((i & 0377) == 0200)
	{
	  if ((i & 0400) != 0)
	    {
	      /* check all less significant bits */
	      for (j = M + 5; j < NI; j++)
		{
		  if (xi[j] != 0)
		    goto yesrnd;
		}
	    }
	  goto nornd;
	}
    yesrnd:
      eaddm (decbit, xi);
      r -= enormlz (xi);
    }

 nornd:

  r -= EXONE;
  r += 0201;
  if (r < 0)
    {
    zout:
      *d++ = 0;
      *d++ = 0;
      *d++ = 0;
      *d++ = 0;
      return;
    }
  if (r >= 0377)
    {
      *d++ = 077777;
      *d++ = -1;
      *d++ = -1;
      *d++ = -1;
      return;
    }
  r &= 0377;
  r <<= 7;
  eshup8 (xi);
  xi[M] &= 0177;
  r |= xi[M];
  *d++ |= r;
  *d++ = xi[M + 1];
  *d++ = xi[M + 2];
  *d++ = xi[M + 3];
}

#else

void 
etodec (x, d)
     unsigned EMUSHORT *x, *d;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

  emovi (x, xi);
  exp = (EMULONG) xi[E] - (EXONE - 0201);	/* adjust exponent for offsets */
/* round off to nearest or even */
  rndsav = rndprc;
  rndprc = 56;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
  todec (xi, d);
}

void 
todec (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMUSHORT i;
  unsigned EMUSHORT *p;

  p = x;
  *y = 0;
  if (*p++)
    *y = 0100000;
  i = *p++;
  if (i == 0)
    {
      *y++ = 0;
      *y++ = 0;
      *y++ = 0;
      *y++ = 0;
      return;
    }
  if (i > 0377)
    {
      *y++ |= 077777;
      *y++ = 0xffff;
      *y++ = 0xffff;
      *y++ = 0xffff;
#ifdef ERANGE
      errno = ERANGE;
#endif
      return;
    }
  i &= 0377;
  i <<= 7;
  eshup8 (x);
  x[M] &= 0177;
  i |= x[M];
  *y++ |= i;
  *y++ = x[M + 1];
  *y++ = x[M + 2];
  *y++ = x[M + 3];
}

#endif /* not 0 */


/* Output a binary NaN bit pattern in the target machine's format.  */

/* If special NaN bit patterns are required, define them in tm.h
   as arrays of unsigned 16-bit shorts.  Otherwise, use the default
   patterns here. */
#ifndef TFMODE_NAN
#ifdef MIEEE
unsigned EMUSHORT TFnan[8] =
 {0x7fff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff};
#endif
#ifdef IBMPC
unsigned EMUSHORT TFnan[8] = {0, 0, 0, 0, 0, 0, 0x8000, 0xffff};
#endif
#endif

#ifndef XFMODE_NAN
#ifdef MIEEE
unsigned EMUSHORT XFnan[6] = {0x7fff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff};
#endif
#ifdef IBMPC
unsigned EMUSHORT XFnan[6] = {0, 0, 0, 0xc000, 0xffff, 0};
#endif
#endif

#ifndef DFMODE_NAN
#ifdef MIEEE
unsigned EMUSHORT DFnan[4] = {0x7fff, 0xffff, 0xffff, 0xffff};
#endif
#ifdef IBMPC
unsigned EMUSHORT DFnan[4] = {0, 0, 0, 0xfff8};
#endif
#endif

#ifndef SFMODE_NAN
#ifdef MIEEE
unsigned EMUSHORT SFnan[2] = {0x7fff, 0xffff};
#endif
#ifdef IBMPC
unsigned EMUSHORT SFnan[2] = {0, 0xffc0};
#endif
#endif


void
make_nan (nan, mode)
unsigned EMUSHORT *nan;
enum machine_mode mode;
{
  int i, n;
  unsigned EMUSHORT *p;

  switch (mode)
    {
/* Possibly the `reserved operand' patterns on a VAX can be
   used like NaN's, but probably not in the same way as IEEE. */
#ifndef DEC
    case TFmode:
      n = 8;
      p = TFnan;
      break;
    case XFmode:
      n = 6;
      p = XFnan;
      break;
    case DFmode:
      n = 4;
      p = DFnan;
      break;
    case SFmode:
      n = 2;
      p = SFnan;
      break;
#endif
    default:
      abort ();
    }
  for (i=0; i < n; i++)
    *nan++ = *p++;
}

#endif /* EMU_NON_COMPILE not defined */
