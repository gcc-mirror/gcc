/* real.c - implementation of REAL_ARITHMETIC, REAL_VALUE_ATOF,
   and support for XFmode IEEE extended real floating point arithmetic.
   Copyright (C) 1993, 94-98, 1999 Free Software Foundation, Inc.
   Contributed by Stephen L. Moshier (moshier@world.std.com).

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "toplev.h"

/* To enable support of XFmode extended real floating point, define
LONG_DOUBLE_TYPE_SIZE 96 in the tm.h file (m68k.h or i386.h).

To support cross compilation between IEEE, VAX and IBM floating
point formats, define REAL_ARITHMETIC in the tm.h file.

In either case the machine files (tm.h) must not contain any code
that tries to use host floating point arithmetic to convert
REAL_VALUE_TYPEs from `double' to `float', pass them to fprintf,
etc.  In cross-compile situations a REAL_VALUE_TYPE may not
be intelligible to the host computer's native arithmetic.

The emulator defaults to the host's floating point format so that
its decimal conversion functions can be used if desired (see
real.h).

The first part of this file interfaces gcc to a floating point
arithmetic suite that was not written with gcc in mind.  Avoid
changing the low-level arithmetic routines unless you have suitable
test programs available.  A special version of the PARANOIA floating
point arithmetic tester, modified for this purpose, can be found on
usc.edu: /pub/C-numanal/ieeetest.zoo.  Other tests, and libraries of
XFmode and TFmode transcendental functions, can be obtained by ftp from
netlib.att.com: netlib/cephes.   */

/* Type of computer arithmetic.
   Only one of DEC, IBM, IEEE, C4X, or UNK should get defined.

   `IEEE', when REAL_WORDS_BIG_ENDIAN is non-zero, refers generically
   to big-endian IEEE floating-point data structure.  This definition
   should work in SFmode `float' type and DFmode `double' type on
   virtually all big-endian IEEE machines.  If LONG_DOUBLE_TYPE_SIZE
   has been defined to be 96, then IEEE also invokes the particular
   XFmode (`long double' type) data structure used by the Motorola
   680x0 series processors.

   `IEEE', when REAL_WORDS_BIG_ENDIAN is zero, refers generally to
   little-endian IEEE machines. In this case, if LONG_DOUBLE_TYPE_SIZE
   has been defined to be 96, then IEEE also invokes the particular
   XFmode `long double' data structure used by the Intel 80x86 series
   processors.

   `DEC' refers specifically to the Digital Equipment Corp PDP-11
   and VAX floating point data structure.  This model currently
   supports no type wider than DFmode.

   `IBM' refers specifically to the IBM System/370 and compatible
   floating point data structure.  This model currently supports
   no type wider than DFmode.  The IBM conversions were contributed by
   frank@atom.ansto.gov.au (Frank Crawford).

   `C4X' refers specifically to the floating point format used on
   Texas Instruments TMS320C3x and TMS320C4x digital signal
   processors.  This supports QFmode (32-bit float, double) and HFmode
   (40-bit long double) where BITS_PER_BYTE is 32. Unlike IEEE
   floats, C4x floats are not rounded to be even. The C4x conversions
   were contributed by m.hayes@elec.canterbury.ac.nz (Michael Hayes) and
   Haj.Ten.Brugge@net.HCC.nl (Herman ten Brugge).

   If LONG_DOUBLE_TYPE_SIZE = 64 (the default, unless tm.h defines it)
   then `long double' and `double' are both implemented, but they
   both mean DFmode.  In this case, the software floating-point
   support available here is activated by writing
      #define REAL_ARITHMETIC
   in tm.h.

   The case LONG_DOUBLE_TYPE_SIZE = 128 activates TFmode support
   and may deactivate XFmode since `long double' is used to refer
   to both modes.

   The macros FLOAT_WORDS_BIG_ENDIAN, HOST_FLOAT_WORDS_BIG_ENDIAN,
   contributed by Richard Earnshaw <Richard.Earnshaw@cl.cam.ac.uk>,
   separate the floating point unit's endian-ness from that of
   the integer addressing.  This permits one to define a big-endian
   FPU on a little-endian machine (e.g., ARM).  An extension to
   BYTES_BIG_ENDIAN may be required for some machines in the future.
   These optional macros may be defined in tm.h.  In real.h, they
   default to WORDS_BIG_ENDIAN, etc., so there is no need to define
   them for any normal host or target machine on which the floats
   and the integers have the same endian-ness.   */


/* The following converts gcc macros into the ones used by this file.  */

/* REAL_ARITHMETIC defined means that macros in real.h are
   defined to call emulator functions.  */
#ifdef REAL_ARITHMETIC

#if TARGET_FLOAT_FORMAT == VAX_FLOAT_FORMAT
/* PDP-11, Pro350, VAX: */
#define DEC 1
#else /* it's not VAX */
#if TARGET_FLOAT_FORMAT == IBM_FLOAT_FORMAT
/* IBM System/370 style */
#define IBM 1
#else /* it's also not an IBM */
#if TARGET_FLOAT_FORMAT == C4X_FLOAT_FORMAT
/* TMS320C3x/C4x style */
#define C4X 1
#else /* it's also not a C4X */
#if TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
#define IEEE
#else /* it's not IEEE either */
/* UNKnown arithmetic.  We don't support this and can't go on.  */
unknown arithmetic type
#define UNK 1
#endif /* not IEEE */
#endif /* not C4X */
#endif /* not IBM */
#endif /* not VAX */

#define REAL_WORDS_BIG_ENDIAN FLOAT_WORDS_BIG_ENDIAN

#else
/* REAL_ARITHMETIC not defined means that the *host's* data
   structure will be used.  It may differ by endian-ness from the
   target machine's structure and will get its ends swapped
   accordingly (but not here).  Probably only the decimal <-> binary
   functions in this file will actually be used in this case.  */

#if HOST_FLOAT_FORMAT == VAX_FLOAT_FORMAT
#define DEC 1
#else /* it's not VAX */
#if HOST_FLOAT_FORMAT == IBM_FLOAT_FORMAT
/* IBM System/370 style */
#define IBM 1
#else /* it's also not an IBM */
#if HOST_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
#define IEEE
#else /* it's not IEEE either */
unknown arithmetic type
#define UNK 1
#endif /* not IEEE */
#endif /* not IBM */
#endif /* not VAX */

#define REAL_WORDS_BIG_ENDIAN HOST_FLOAT_WORDS_BIG_ENDIAN

#endif /* REAL_ARITHMETIC not defined */

/* Define INFINITY for support of infinity.
   Define NANS for support of Not-a-Number's (NaN's).  */
#if !defined(DEC) && !defined(IBM) && !defined(C4X)
#define INFINITY
#define NANS
#endif

/* Support of NaNs requires support of infinity.  */
#ifdef NANS
#ifndef INFINITY
#define INFINITY
#endif
#endif

/* Find a host integer type that is at least 16 bits wide,
   and another type at least twice whatever that size is.  */

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
/*  You will have to modify this program to have a smaller unit size.  */
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
#if HOST_BITS_PER_LONGLONG >= EMULONG_SIZE
#define EMULONG long long int
#else
/*  You will have to modify this program to have a smaller unit size.  */
#define EMU_NON_COMPILE
#endif
#endif
#endif
#endif


/* The host interface doesn't work if no 16-bit size exists.  */
#if EMUSHORT_SIZE != 16
#define EMU_NON_COMPILE
#endif

/* OK to continue compilation.  */
#ifndef EMU_NON_COMPILE

/* Construct macros to translate between REAL_VALUE_TYPE and e type.
   In GET_REAL and PUT_REAL, r and e are pointers.
   A REAL_VALUE_TYPE is guaranteed to occupy contiguous locations
   in memory, with no holes.  */

#if LONG_DOUBLE_TYPE_SIZE == 96
/* Number of 16 bit words in external e type format */
#define NE 6
#define MAXDECEXP 4932
#define MINDECEXP -4956
#define GET_REAL(r,e) bcopy ((char *) r, (char *) e, 2*NE)
#define PUT_REAL(e,r)				\
do {						\
  if (2*NE < sizeof(*r))			\
    bzero((char *)r, sizeof(*r));		\
  bcopy ((char *) e, (char *) r, 2*NE);		\
} while (0)
#else /* no XFmode */
#if LONG_DOUBLE_TYPE_SIZE == 128
#define NE 10
#define MAXDECEXP 4932
#define MINDECEXP -4977
#define GET_REAL(r,e) bcopy ((char *) r, (char *) e, 2*NE)
#define PUT_REAL(e,r) bcopy ((char *) e, (char *) r, 2*NE)
#else
#define NE 6
#define MAXDECEXP 4932
#define MINDECEXP -4956
#ifdef REAL_ARITHMETIC
/* Emulator uses target format internally
   but host stores it in host endian-ness.  */

#define GET_REAL(r,e)						\
do {								\
     if (HOST_FLOAT_WORDS_BIG_ENDIAN == REAL_WORDS_BIG_ENDIAN)	\
       e53toe ((unsigned EMUSHORT *) (r), (e));			\
     else							\
       {							\
	 unsigned EMUSHORT w[4];				\
	 w[3] = ((EMUSHORT *) r)[0];				\
	 w[2] = ((EMUSHORT *) r)[1];				\
	 w[1] = ((EMUSHORT *) r)[2];				\
	 w[0] = ((EMUSHORT *) r)[3];				\
	 e53toe (w, (e));					\
       }							\
   } while (0)

#define PUT_REAL(e,r)						\
do {								\
     if (HOST_FLOAT_WORDS_BIG_ENDIAN == REAL_WORDS_BIG_ENDIAN)	\
       etoe53 ((e), (unsigned EMUSHORT *) (r));			\
     else							\
       {							\
	 unsigned EMUSHORT w[4];				\
	 etoe53 ((e), w);					\
	 *((EMUSHORT *) r) = w[3];				\
	 *((EMUSHORT *) r + 1) = w[2];				\
	 *((EMUSHORT *) r + 2) = w[1];				\
	 *((EMUSHORT *) r + 3) = w[0];				\
       }							\
   } while (0)

#else /* not REAL_ARITHMETIC */

/* emulator uses host format */
#define GET_REAL(r,e) e53toe ((unsigned EMUSHORT *) (r), (e))
#define PUT_REAL(e,r) etoe53 ((e), (unsigned EMUSHORT *) (r))

#endif /* not REAL_ARITHMETIC */
#endif /* not TFmode */
#endif /* not XFmode */


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

extern int extra_warnings;
extern unsigned EMUSHORT ezero[], ehalf[], eone[], etwo[];
extern unsigned EMUSHORT elog2[], esqrt2[];

static void endian	PROTO((unsigned EMUSHORT *, long *,
			       enum machine_mode));
static void eclear	PROTO((unsigned EMUSHORT *));
static void emov	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
#if 0
static void eabs	PROTO((unsigned EMUSHORT *));
#endif
static void eneg	PROTO((unsigned EMUSHORT *));
static int eisneg	PROTO((unsigned EMUSHORT *));
static int eisinf	PROTO((unsigned EMUSHORT *));
static int eisnan	PROTO((unsigned EMUSHORT *));
static void einfin	PROTO((unsigned EMUSHORT *));
static void enan	PROTO((unsigned EMUSHORT *, int));
static void emovi	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void emovo	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void ecleaz	PROTO((unsigned EMUSHORT *));
static void ecleazs	PROTO((unsigned EMUSHORT *));
static void emovz	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void einan	PROTO((unsigned EMUSHORT *));
static int eiisnan	PROTO((unsigned EMUSHORT *));
static int eiisneg	PROTO((unsigned EMUSHORT *));
#if 0
static void eiinfin	PROTO((unsigned EMUSHORT *));
#endif
static int eiisinf	PROTO((unsigned EMUSHORT *));
static int ecmpm	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void eshdn1	PROTO((unsigned EMUSHORT *));
static void eshup1	PROTO((unsigned EMUSHORT *));
static void eshdn8	PROTO((unsigned EMUSHORT *));
static void eshup8	PROTO((unsigned EMUSHORT *));
static void eshup6	PROTO((unsigned EMUSHORT *));
static void eshdn6	PROTO((unsigned EMUSHORT *));
static void eaddm	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void esubm	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void m16m	PROTO((unsigned int, unsigned short *,
			       unsigned short *));
static int edivm	PROTO((unsigned short *, unsigned short *));
static int emulm	PROTO((unsigned short *, unsigned short *));
static void emdnorm	PROTO((unsigned EMUSHORT *, int, int, EMULONG, int));
static void esub	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       unsigned EMUSHORT *));
static void eadd	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       unsigned EMUSHORT *));
static void eadd1	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       unsigned EMUSHORT *));
static void ediv	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       unsigned EMUSHORT *));
static void emul	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       unsigned EMUSHORT *));
static void e53toe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void e64toe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void e113toe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void e24toe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etoe113	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void toe113	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etoe64	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void toe64	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etoe53	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void toe53	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etoe24	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void toe24	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static int ecmp		PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
#if 0
static void eround	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
#endif
static void ltoe	PROTO((HOST_WIDE_INT *, unsigned EMUSHORT *));
static void ultoe	PROTO((unsigned HOST_WIDE_INT *, unsigned EMUSHORT *));
static void eifrac	PROTO((unsigned EMUSHORT *, HOST_WIDE_INT *,
			       unsigned EMUSHORT *));
static void euifrac	PROTO((unsigned EMUSHORT *, unsigned HOST_WIDE_INT *,
			       unsigned EMUSHORT *));
static int eshift	PROTO((unsigned EMUSHORT *, int));
static int enormlz	PROTO((unsigned EMUSHORT *));
#if 0
static void e24toasc	PROTO((unsigned EMUSHORT *, char *, int));
static void e53toasc	PROTO((unsigned EMUSHORT *, char *, int));
static void e64toasc	PROTO((unsigned EMUSHORT *, char *, int));
static void e113toasc	PROTO((unsigned EMUSHORT *, char *, int));
#endif /* 0 */
static void etoasc	PROTO((unsigned EMUSHORT *, char *, int));
static void asctoe24	PROTO((const char *, unsigned EMUSHORT *));
static void asctoe53	PROTO((const char *, unsigned EMUSHORT *));
static void asctoe64	PROTO((const char *, unsigned EMUSHORT *));
static void asctoe113	PROTO((const char *, unsigned EMUSHORT *));
static void asctoe	PROTO((const char *, unsigned EMUSHORT *));
static void asctoeg	PROTO((const char *, unsigned EMUSHORT *, int));
static void efloor	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
#if 0
static void efrexp	PROTO((unsigned EMUSHORT *, int *,
			       unsigned EMUSHORT *));
#endif
static void eldexp	PROTO((unsigned EMUSHORT *, int, unsigned EMUSHORT *));
#if 0
static void eremain	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       unsigned EMUSHORT *));
#endif
static void eiremain	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void mtherr	PROTO((const char *, int));
#ifdef DEC
static void dectoe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etodec	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void todec	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
#endif
#ifdef IBM
static void ibmtoe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       enum machine_mode));
static void etoibm	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       enum machine_mode));
static void toibm	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
			       enum machine_mode));
#endif
#ifdef C4X
static void c4xtoe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
 			       enum machine_mode));
static void etoc4x	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
 			       enum machine_mode));
static void toc4x	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *,
 			       enum machine_mode));
#endif
static void make_nan	PROTO((unsigned EMUSHORT *, int, enum machine_mode));
#if 0
static void uditoe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void ditoe	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etoudi	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void etodi	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
static void esqrt	PROTO((unsigned EMUSHORT *, unsigned EMUSHORT *));
#endif

/* Copy 32-bit numbers obtained from array containing 16-bit numbers,
   swapping ends if required, into output array of longs.  The
   result is normally passed to fprintf by the ASM_OUTPUT_ macros.   */

static void
endian (e, x, mode)
     unsigned EMUSHORT e[];
     long x[];
     enum machine_mode mode;
{
  unsigned long th, t;

  if (REAL_WORDS_BIG_ENDIAN)
    {
      switch (mode)
	{
	case TFmode:
	  /* Swap halfwords in the fourth long.  */
	  th = (unsigned long) e[6] & 0xffff;
	  t = (unsigned long) e[7] & 0xffff;
	  t |= th << 16;
	  x[3] = (long) t;

	case XFmode:
	  /* Swap halfwords in the third long.  */
	  th = (unsigned long) e[4] & 0xffff;
	  t = (unsigned long) e[5] & 0xffff;
	  t |= th << 16;
	  x[2] = (long) t;
	  /* fall into the double case */

	case DFmode:
	  /* Swap halfwords in the second word.  */
	  th = (unsigned long) e[2] & 0xffff;
	  t = (unsigned long) e[3] & 0xffff;
	  t |= th << 16;
	  x[1] = (long) t;
	  /* fall into the float case */

	case SFmode:
	case HFmode:
	  /* Swap halfwords in the first word.  */
	  th = (unsigned long) e[0] & 0xffff;
	  t = (unsigned long) e[1] & 0xffff;
	  t |= th << 16;
	  x[0] = (long) t;
	  break;

	default:
	  abort ();
	}
    }
  else
    {
      /* Pack the output array without swapping.  */

      switch (mode)
	{
	case TFmode:
	  /* Pack the fourth long.  */
	  th = (unsigned long) e[7] & 0xffff;
	  t = (unsigned long) e[6] & 0xffff;
	  t |= th << 16;
	  x[3] = (long) t;

	case XFmode:
	  /* Pack the third long.
	     Each element of the input REAL_VALUE_TYPE array has 16 useful bits
	     in it.  */
	  th = (unsigned long) e[5] & 0xffff;
	  t = (unsigned long) e[4] & 0xffff;
	  t |= th << 16;
	  x[2] = (long) t;
	  /* fall into the double case */

	case DFmode:
	  /* Pack the second long */
	  th = (unsigned long) e[3] & 0xffff;
	  t = (unsigned long) e[2] & 0xffff;
	  t |= th << 16;
	  x[1] = (long) t;
	  /* fall into the float case */

	case SFmode:
	case HFmode:
	  /* Pack the first long */
	  th = (unsigned long) e[1] & 0xffff;
	  t = (unsigned long) e[0] & 0xffff;
	  t |= th << 16;
	  x[0] = (long) t;
	  break;

	default:
	  abort ();
	}
    }
}


/* This is the implementation of the REAL_ARITHMETIC macro.  */

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
/*  Return NaN input back to the caller.  */
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
	enan (v, eisneg (d1) ^ eisneg (d2));
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


/* Truncate REAL_VALUE_TYPE toward zero to signed HOST_WIDE_INT.
   implements REAL_VALUE_RNDZINT (x) (etrunci (x)).  */

REAL_VALUE_TYPE
etrunci (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  REAL_VALUE_TYPE r;
  HOST_WIDE_INT l;

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


/* Truncate REAL_VALUE_TYPE toward zero to unsigned HOST_WIDE_INT;
   implements REAL_VALUE_UNSIGNED_RNDZINT (x) (etruncui (x)).  */

REAL_VALUE_TYPE
etruncui (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  REAL_VALUE_TYPE r;
  unsigned HOST_WIDE_INT l;

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


/* This is the REAL_VALUE_ATOF function.  It converts a decimal or hexadecimal
   string to binary, rounding off as indicated by the machine_mode argument.
   Then it promotes the rounded value to REAL_VALUE_TYPE.  */

REAL_VALUE_TYPE
ereal_atof (s, t)
     const char *s;
     enum machine_mode t;
{
  unsigned EMUSHORT tem[NE], e[NE];
  REAL_VALUE_TYPE r;

  switch (t)
    {
#ifdef C4X
    case QFmode:
    case HFmode:
      asctoe53 (s, tem);
      e53toe (tem, e);
      break;
#else
    case HFmode:
#endif

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

    case TFmode:
      asctoe113 (s, tem);
      e113toe (tem, e);
      break;

    default:
      asctoe (s, e);
    }
  PUT_REAL (e, &r);
  return (r);
}


/* Expansion of REAL_NEGATE.  */

REAL_VALUE_TYPE
ereal_negate (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT e[NE];
  REAL_VALUE_TYPE r;

  GET_REAL (&x, e);
  eneg (e);
  PUT_REAL (e, &r);
  return (r);
}


/* Round real toward zero to HOST_WIDE_INT;
   implements REAL_VALUE_FIX (x).  */

HOST_WIDE_INT
efixi (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  HOST_WIDE_INT l;

  GET_REAL (&x, f);
#ifdef NANS
  if (eisnan (f))
    {
      warning ("conversion from NaN to int");
      return (-1);
    }
#endif
  eifrac (f, &l, g);
  return l;
}

/* Round real toward zero to unsigned HOST_WIDE_INT
   implements  REAL_VALUE_UNSIGNED_FIX (x).
   Negative input returns zero.  */

unsigned HOST_WIDE_INT
efixui (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT f[NE], g[NE];
  unsigned HOST_WIDE_INT l;

  GET_REAL (&x, f);
#ifdef NANS
  if (eisnan (f))
    {
      warning ("conversion from NaN to unsigned int");
      return (-1);
    }
#endif
  euifrac (f, &l, g);
  return l;
}


/* REAL_VALUE_FROM_INT macro.  */

void
ereal_from_int (d, i, j, mode)
     REAL_VALUE_TYPE *d;
     HOST_WIDE_INT i, j;
     enum machine_mode mode;
{
  unsigned EMUSHORT df[NE], dg[NE];
  HOST_WIDE_INT low, high;
  int sign;

  if (GET_MODE_CLASS (mode) != MODE_FLOAT)
    abort ();
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
  eldexp (eone, HOST_BITS_PER_WIDE_INT, df);
  ultoe ((unsigned HOST_WIDE_INT *) &high, dg);
  emul (dg, df, dg);
  ultoe ((unsigned HOST_WIDE_INT *) &low, df);
  eadd (df, dg, dg);
  if (sign)
    eneg (dg);

  /* A REAL_VALUE_TYPE may not be wide enough to hold the two HOST_WIDE_INTS.
     Avoid double-rounding errors later by rounding off now from the
     extra-wide internal format to the requested precision.  */
  switch (GET_MODE_BITSIZE (mode))
    {
    case 32:
      etoe24 (dg, df);
      e24toe (df, dg);
      break;

    case 64:
      etoe53 (dg, df);
      e53toe (df, dg);
      break;

    case 96:
      etoe64 (dg, df);
      e64toe (df, dg);
      break;

    case 128:
      etoe113 (dg, df);
      e113toe (df, dg);
      break;

    default:
      abort ();
  }

  PUT_REAL (dg, d);
}


/* REAL_VALUE_FROM_UNSIGNED_INT macro.   */

void
ereal_from_uint (d, i, j, mode)
     REAL_VALUE_TYPE *d;
     unsigned HOST_WIDE_INT i, j;
     enum machine_mode mode;
{
  unsigned EMUSHORT df[NE], dg[NE];
  unsigned HOST_WIDE_INT low, high;

  if (GET_MODE_CLASS (mode) != MODE_FLOAT)
    abort ();
  low = i;
  high = j;
  eldexp (eone, HOST_BITS_PER_WIDE_INT, df);
  ultoe (&high, dg);
  emul (dg, df, dg);
  ultoe (&low, df);
  eadd (df, dg, dg);

  /* A REAL_VALUE_TYPE may not be wide enough to hold the two HOST_WIDE_INTS.
     Avoid double-rounding errors later by rounding off now from the
     extra-wide internal format to the requested precision.  */
  switch (GET_MODE_BITSIZE (mode))
    {
    case 32:
      etoe24 (dg, df);
      e24toe (df, dg);
      break;

    case 64:
      etoe53 (dg, df);
      e53toe (df, dg);
      break;

    case 96:
      etoe64 (dg, df);
      e64toe (df, dg);
      break;

    case 128:
      etoe113 (dg, df);
      e113toe (df, dg);
      break;

    default:
      abort ();
  }

  PUT_REAL (dg, d);
}


/* REAL_VALUE_TO_INT macro.  */

void
ereal_to_int (low, high, rr)
     HOST_WIDE_INT *low, *high;
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
  eldexp (eone, HOST_BITS_PER_WIDE_INT, df);
  ediv (df, d, dg);		/* dg = d / 2^32 is the high word */
  euifrac (dg, (unsigned HOST_WIDE_INT *) high, dh);
  emul (df, dh, dg);		/* fractional part is the low word */
  euifrac (dg, (unsigned HOST_WIDE_INT *)low, dh);
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


/* REAL_VALUE_LDEXP macro.  */

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
   of the same names may be defined in fold-const.c.  */

#ifdef REAL_ARITHMETIC

/* Check for infinity in a REAL_VALUE_TYPE.  */

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

/* Check whether a REAL_VALUE_TYPE item is a NaN.  */

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
   This just checks the sign bit, so that -0 counts as negative.  */

int
target_negative (x)
     REAL_VALUE_TYPE x;
{
  return ereal_isneg (x);
}

/* Expansion of REAL_VALUE_TRUNCATE.
   The result is in floating point, rounded to nearest or even.  */

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
    case TFmode:
      etoe113 (e, t);
      e113toe (t, t);
      break;

    case XFmode:
      etoe64 (e, t);
      e64toe (t, t);
      break;

    case DFmode:
      etoe53 (e, t);
      e53toe (t, t);
      break;

    case SFmode:
#ifndef C4X
    case HFmode:
#endif
      etoe24 (e, t);
      e24toe (t, t);
      break;

#ifdef C4X
    case HFmode:
    case QFmode:
      etoe53 (e, t);
      e53toe (t, t);
      break;
#endif

    case SImode:
      r = etrunci (arg);
      return (r);

    /* If an unsupported type was requested, presume that
       the machine files know something useful to do with
       the unmodified value.  */

    default:
      return (arg);
    }
  PUT_REAL (t, &r);
  return (r);
}

/* Try to change R into its exact multiplicative inverse in machine mode
   MODE.  Return nonzero function value if successful.  */

int
exact_real_inverse (mode, r)
     enum machine_mode mode;
     REAL_VALUE_TYPE *r;
{
  unsigned EMUSHORT e[NE], einv[NE];
  REAL_VALUE_TYPE rinv;
  int i;

  GET_REAL (r, e);

  /* Test for input in range.  Don't transform IEEE special values.  */
  if (eisinf (e) || eisnan (e) || (ecmp (e, ezero) == 0))
    return 0;

  /* Test for a power of 2: all significand bits zero except the MSB.
     We are assuming the target has binary (or hex) arithmetic.  */
  if (e[NE - 2] != 0x8000)
    return 0;

  for (i = 0; i < NE - 2; i++)
    {
      if (e[i] != 0)
	return 0;
    }

  /* Compute the inverse and truncate it to the required mode.  */
  ediv (e, eone, einv);
  PUT_REAL (einv, &rinv);
  rinv = real_value_truncate (mode, rinv);

#ifdef CHECK_FLOAT_VALUE
  /* This check is not redundant.  It may, for example, flush
     a supposedly IEEE denormal value to zero.  */
  i = 0;
  if (CHECK_FLOAT_VALUE (mode, rinv, i))
    return 0;
#endif
  GET_REAL (&rinv, einv);

  /* Check the bits again, because the truncation might have
     generated an arbitrary saturation value on overflow.  */
  if (einv[NE - 2] != 0x8000)
    return 0;

  for (i = 0; i < NE - 2; i++)
    {
      if (einv[i] != 0)
	return 0;
    }

  /* Fail if the computed inverse is out of range.  */
  if (eisinf (einv) || eisnan (einv) || (ecmp (einv, ezero) == 0))
    return 0;

  /* Output the reciprocal and return success flag.  */
  PUT_REAL (einv, r);
  return 1;
}
#endif /* REAL_ARITHMETIC defined */

/* Used for debugging--print the value of R in human-readable format
   on stderr.  */

void
debug_real (r)
     REAL_VALUE_TYPE r;
{
  char dstr[30];

  REAL_VALUE_TO_DECIMAL (r, "%.20g", dstr);
  fprintf (stderr, "%s", dstr);
}


/* The following routines convert REAL_VALUE_TYPE to the various floating
   point formats that are meaningful to supported computers.

   The results are returned in 32-bit pieces, each piece stored in a `long'.
   This is so they can be printed by statements like

      fprintf (file, "%lx, %lx", L[0],  L[1]);

   that will work on both narrow- and wide-word host computers.  */

/* Convert R to a 128-bit long double precision value.  The output array L
   contains four 32-bit pieces of the result, in the order they would appear
   in memory.  */

void
etartdouble (r, l)
     REAL_VALUE_TYPE r;
     long l[];
{
  unsigned EMUSHORT e[NE];

  GET_REAL (&r, e);
  etoe113 (e, e);
  endian (e, l, TFmode);
}

/* Convert R to a double extended precision value.  The output array L
   contains three 32-bit pieces of the result, in the order they would
   appear in memory.  */

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

/* Convert R to a double precision value.  The output array L contains two
   32-bit pieces of the result, in the order they would appear in memory.  */

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

/* Convert R to a single precision float value stored in the least-significant
   bits of a `long'.  */

long
etarsingle (r)
     REAL_VALUE_TYPE r;
{
  unsigned EMUSHORT e[NE];
  long l;

  GET_REAL (&r, e);
  etoe24 (e, e);
  endian (e, &l, SFmode);
  return ((long) l);
}

/* Convert X to a decimal ASCII string S for output to an assembly
   language file.  Note, there is no standard way to spell infinity or
   a NaN, so these values may require special treatment in the tm.h
   macros.  */

void
ereal_to_decimal (x, s)
     REAL_VALUE_TYPE x;
     char *s;
{
  unsigned EMUSHORT e[NE];

  GET_REAL (&x, e);
  etoasc (e, s, 20);
}

/* Compare X and Y.  Return 1 if X > Y, 0 if X == Y, -1 if X < Y,
   or -2 if either is a NaN.   */

int
ereal_cmp (x, y)
     REAL_VALUE_TYPE x, y;
{
  unsigned EMUSHORT ex[NE], ey[NE];

  GET_REAL (&x, ex);
  GET_REAL (&y, ey);
  return (ecmp (ex, ey));
}

/*  Return 1 if the sign bit of X is set, else return 0.  */

int
ereal_isneg (x)
     REAL_VALUE_TYPE x;
{
  unsigned EMUSHORT ex[NE];

  GET_REAL (&x, ex);
  return (eisneg (ex));
}

/* End of REAL_ARITHMETIC interface */

/*
  Extended precision IEEE binary floating point arithmetic routines

  Numbers are stored in C language as arrays of 16-bit unsigned
  short integers.  The arguments of the routines are pointers to
  the arrays.

  External e type data structure, similar to Intel 8087 chip
  temporary real format but possibly with a larger significand:

	NE-1 significand words	(least significant word first,
				 most significant bit is normally set)
	exponent		(value = EXONE for 1.0,
				top bit is the sign)


  Internal exploded e-type data structure of a number (a "word" is 16 bits):

  ei[0]	sign word	(0 for positive, 0xffff for negative)
  ei[1]	biased exponent	(value = EXONE for the number 1.0)
  ei[2]	high guard word	(always zero after normalization)
  ei[3]
  to ei[NI-2]	significand	(NI-4 significand words,
 				 most significant word first,
 				 most significant bit is set)
  ei[NI-1]	low guard word	(0x8000 bit is rounding place)



 		Routines for external format e-type numbers

 	asctoe (string, e)	ASCII string to extended double e type
 	asctoe64 (string, &d)	ASCII string to long double
 	asctoe53 (string, &d)	ASCII string to double
 	asctoe24 (string, &f)	ASCII string to single
 	asctoeg (string, e, prec) ASCII string to specified precision
 	e24toe (&f, e)		IEEE single precision to e type
 	e53toe (&d, e)		IEEE double precision to e type
 	e64toe (&d, e)		IEEE long double precision to e type
 	e113toe (&d, e)		128-bit long double precision to e type
#if 0
 	eabs (e)			absolute value
#endif
 	eadd (a, b, c)		c = b + a
 	eclear (e)		e = 0
 	ecmp (a, b)		Returns 1 if a > b, 0 if a == b,
 				-1 if a < b, -2 if either a or b is a NaN.
 	ediv (a, b, c)		c = b / a
 	efloor (a, b)		truncate to integer, toward -infinity
 	efrexp (a, exp, s)	extract exponent and significand
 	eifrac (e, &l, frac)    e to HOST_WIDE_INT and e type fraction
 	euifrac (e, &l, frac)   e to unsigned HOST_WIDE_INT and e type fraction
 	einfin (e)		set e to infinity, leaving its sign alone
 	eldexp (a, n, b)	multiply by 2**n
 	emov (a, b)		b = a
 	emul (a, b, c)		c = b * a
 	eneg (e)			e = -e
#if 0
 	eround (a, b)		b = nearest integer value to a
#endif
 	esub (a, b, c)		c = b - a
#if 0
 	e24toasc (&f, str, n)	single to ASCII string, n digits after decimal
 	e53toasc (&d, str, n)	double to ASCII string, n digits after decimal
 	e64toasc (&d, str, n)	80-bit long double to ASCII string
 	e113toasc (&d, str, n)	128-bit long double to ASCII string
#endif
 	etoasc (e, str, n)	e to ASCII string, n digits after decimal
 	etoe24 (e, &f)		convert e type to IEEE single precision
 	etoe53 (e, &d)		convert e type to IEEE double precision
 	etoe64 (e, &d)		convert e type to IEEE long double precision
 	ltoe (&l, e)		HOST_WIDE_INT to e type
 	ultoe (&l, e)		unsigned HOST_WIDE_INT to e type
	eisneg (e)              1 if sign bit of e != 0, else 0
	eisinf (e)              1 if e has maximum exponent (non-IEEE)
 				or is infinite (IEEE)
        eisnan (e)              1 if e is a NaN


 		Routines for internal format exploded e-type numbers

 	eaddm (ai, bi)		add significands, bi = bi + ai
 	ecleaz (ei)		ei = 0
 	ecleazs (ei)		set ei = 0 but leave its sign alone
 	ecmpm (ai, bi)		compare significands, return 1, 0, or -1
 	edivm (ai, bi)		divide  significands, bi = bi / ai
 	emdnorm (ai,l,s,exp)	normalize and round off
 	emovi (a, ai)		convert external a to internal ai
 	emovo (ai, a)		convert internal ai to external a
 	emovz (ai, bi)		bi = ai, low guard word of bi = 0
 	emulm (ai, bi)		multiply significands, bi = bi * ai
 	enormlz (ei)		left-justify the significand
 	eshdn1 (ai)		shift significand and guards down 1 bit
 	eshdn8 (ai)		shift down 8 bits
 	eshdn6 (ai)		shift down 16 bits
 	eshift (ai, n)		shift ai n bits up (or down if n < 0)
 	eshup1 (ai)		shift significand and guards up 1 bit
 	eshup8 (ai)		shift up 8 bits
 	eshup6 (ai)		shift up 16 bits
 	esubm (ai, bi)		subtract significands, bi = bi - ai
        eiisinf (ai)            1 if infinite
        eiisnan (ai)            1 if a NaN
 	eiisneg (ai)		1 if sign bit of ai != 0, else 0
        einan (ai)              set ai = NaN
#if 0
        eiinfin (ai)            set ai = infinity
#endif

  The result is always normalized and rounded to NI-4 word precision
  after each arithmetic operation.

  Exception flags are NOT fully supported.

  Signaling NaN's are NOT supported; they are treated the same
  as quiet NaN's.

  Define INFINITY for support of infinity; otherwise a
  saturation arithmetic is implemented.

  Define NANS for support of Not-a-Number items; otherwise the
  arithmetic will never produce a NaN output, and might be confused
  by a NaN input.
  If NaN's are supported, the output of `ecmp (a,b)' is -2 if
  either a or b is a NaN. This means asking `if (ecmp (a,b) < 0)'
  may not be legitimate. Use `if (ecmp (a,b) == -1)' for `less than'
  if in doubt.

  Denormals are always supported here where appropriate (e.g., not
  for conversion to DEC numbers).  */

/* Definitions for error codes that are passed to the common error handling
   routine mtherr.

   For Digital Equipment PDP-11 and VAX computers, certain
  IBM systems, and others that use numbers with a 56-bit
  significand, the symbol DEC should be defined.  In this
  mode, most floating point constants are given as arrays
  of octal integers to eliminate decimal to binary conversion
  errors that might be introduced by the compiler.

  For computers, such as IBM PC, that follow the IEEE
  Standard for Binary Floating Point Arithmetic (ANSI/IEEE
  Std 754-1985), the symbol IEEE should be defined.
  These numbers have 53-bit significands.  In this mode, constants
  are provided as arrays of hexadecimal 16 bit integers.
  The endian-ness of generated values is controlled by
  REAL_WORDS_BIG_ENDIAN.

  To accommodate other types of computer arithmetic, all
  constants are also provided in a normal decimal radix
  which one can hope are correctly converted to a suitable
  format by the available C language compiler.  To invoke
  this mode, the symbol UNK is defined.

  An important difference among these modes is a predefined
  set of machine arithmetic constants for each.  The numbers
  MACHEP (the machine roundoff error), MAXNUM (largest number
  represented), and several other parameters are preset by
  the configuration symbol.  Check the file const.c to
  ensure that these values are correct for your computer.

  For ANSI C compatibility, define ANSIC equal to 1.  Currently
  this affects only the atan2 function and others that use it.  */

/* Constant definitions for math error conditions.  */

#define DOMAIN		1	/* argument domain error */
#define SING		2	/* argument singularity */
#define OVERFLOW	3	/* overflow range error */
#define UNDERFLOW	4	/* underflow range error */
#define TLOSS		5	/* total loss of precision */
#define PLOSS		6	/* partial loss of precision */
#define INVALID		7	/* NaN-producing operation */

/*  e type constants used by high precision check routines */

#if LONG_DOUBLE_TYPE_SIZE == 128
/* 0.0 */
unsigned EMUSHORT ezero[NE] =
 {0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,};
extern unsigned EMUSHORT ezero[];

/* 5.0E-1 */
unsigned EMUSHORT ehalf[NE] =
 {0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x8000, 0x3ffe,};
extern unsigned EMUSHORT ehalf[];

/* 1.0E0 */
unsigned EMUSHORT eone[NE] =
 {0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x8000, 0x3fff,};
extern unsigned EMUSHORT eone[];

/* 2.0E0 */
unsigned EMUSHORT etwo[NE] =
 {0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x8000, 0x4000,};
extern unsigned EMUSHORT etwo[];

/* 3.2E1 */
unsigned EMUSHORT e32[NE] =
 {0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x8000, 0x4004,};
extern unsigned EMUSHORT e32[];

/* 6.93147180559945309417232121458176568075500134360255E-1 */
unsigned EMUSHORT elog2[NE] =
 {0x40f3, 0xf6af, 0x03f2, 0xb398,
  0xc9e3, 0x79ab, 0150717, 0013767, 0130562, 0x3ffe,};
extern unsigned EMUSHORT elog2[];

/* 1.41421356237309504880168872420969807856967187537695E0 */
unsigned EMUSHORT esqrt2[NE] =
 {0x1d6f, 0xbe9f, 0x754a, 0x89b3,
  0x597d, 0x6484, 0174736, 0171463, 0132404, 0x3fff,};
extern unsigned EMUSHORT esqrt2[];

/* 3.14159265358979323846264338327950288419716939937511E0 */
unsigned EMUSHORT epi[NE] =
 {0x2902, 0x1cd1, 0x80dc, 0x628b,
  0xc4c6, 0xc234, 0020550, 0155242, 0144417, 0040000,};
extern unsigned EMUSHORT epi[];

#else
/* LONG_DOUBLE_TYPE_SIZE is other than 128 */
unsigned EMUSHORT ezero[NE] =
 {0, 0000000, 0000000, 0000000, 0000000, 0000000,};
unsigned EMUSHORT ehalf[NE] =
 {0, 0000000, 0000000, 0000000, 0100000, 0x3ffe,};
unsigned EMUSHORT eone[NE] =
 {0, 0000000, 0000000, 0000000, 0100000, 0x3fff,};
unsigned EMUSHORT etwo[NE] =
 {0, 0000000, 0000000, 0000000, 0100000, 0040000,};
unsigned EMUSHORT e32[NE] =
 {0, 0000000, 0000000, 0000000, 0100000, 0040004,};
unsigned EMUSHORT elog2[NE] =
 {0xc9e4, 0x79ab, 0150717, 0013767, 0130562, 0x3ffe,};
unsigned EMUSHORT esqrt2[NE] =
 {0x597e, 0x6484, 0174736, 0171463, 0132404, 0x3fff,};
unsigned EMUSHORT epi[NE] =
 {0xc4c6, 0xc234, 0020550, 0155242, 0144417, 0040000,};
#endif

/* Control register for rounding precision.
   This can be set to 113 (if NE=10), 80 (if NE=6), 64, 56, 53, or 24 bits.  */

int rndprc = NBITS;
extern int rndprc;

/*  Clear out entire e-type number X.  */

static void
eclear (x)
     register unsigned EMUSHORT *x;
{
  register int i;

  for (i = 0; i < NE; i++)
    *x++ = 0;
}

/* Move e-type number from A to B.  */

static void
emov (a, b)
     register unsigned EMUSHORT *a, *b;
{
  register int i;

  for (i = 0; i < NE; i++)
    *b++ = *a++;
}


#if 0
/* Absolute value of e-type X.  */

static void
eabs (x)
     unsigned EMUSHORT x[];
{
  /* sign is top bit of last word of external format */
  x[NE - 1] &= 0x7fff;
}
#endif /* 0 */

/* Negate the e-type number X.  */

static void
eneg (x)
     unsigned EMUSHORT x[];
{

  x[NE - 1] ^= 0x8000;		/* Toggle the sign bit */
}

/* Return 1 if sign bit of e-type number X is nonzero, else zero.  */

static int
eisneg (x)
     unsigned EMUSHORT x[];
{

  if (x[NE - 1] & 0x8000)
    return (1);
  else
    return (0);
}

/* Return 1 if e-type number X is infinity, else return zero.  */

static int
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

/* Check if e-type number is not a number.  The bit pattern is one that we
   defined, so we know for sure how to detect it.  */

static int
eisnan (x)
     unsigned EMUSHORT x[];
{
#ifdef NANS
  int i;

  /* NaN has maximum exponent */
  if ((x[NE - 1] & 0x7fff) != 0x7fff)
    return (0);
  /* ... and non-zero significand field.  */
  for (i = 0; i < NE - 1; i++)
    {
      if (*x++ != 0)
        return (1);
    }
#endif

  return (0);
}

/*  Fill e-type number X with infinity pattern (IEEE)
    or largest possible number (non-IEEE).  */

static void
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
      if (rndprc == 113)
	{
	  *(x - 9) = 0;
	  *(x - 8) = 0;
	}
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

static void
enan (x, sign)
     register unsigned EMUSHORT *x;
     int sign;
{
  register int i;

  for (i = 0; i < NE - 2; i++)
    *x++ = 0;
  *x++ = 0xc000;
  *x = (sign << 15) | 0x7fff;
}

/* Move in an e-type number A, converting it to exploded e-type B.  */

static void
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

/* Move out exploded e-type number A, converting it to e type B.  */

static void
emovo (a, b)
     unsigned EMUSHORT *a, *b;
{
  register unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT i;
  int j;

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
	  enan (b, eiisneg (a));
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
  for (j = 0; j < NE - 1; j++)
    *q-- = *p++;
}

/* Clear out exploded e-type number XI.  */

static void
ecleaz (xi)
     register unsigned EMUSHORT *xi;
{
  register int i;

  for (i = 0; i < NI; i++)
    *xi++ = 0;
}

/* Clear out exploded e-type XI, but don't touch the sign.  */

static void
ecleazs (xi)
     register unsigned EMUSHORT *xi;
{
  register int i;

  ++xi;
  for (i = 0; i < NI - 1; i++)
    *xi++ = 0;
}

/* Move exploded e-type number from A to B.  */

static void
emovz (a, b)
     register unsigned EMUSHORT *a, *b;
{
  register int i;

  for (i = 0; i < NI - 1; i++)
    *b++ = *a++;
  /* clear low guard word */
  *b = 0;
}

/* Generate exploded e-type NaN.
   The explicit pattern for this is maximum exponent and
   top two significant bits set.  */

static void
einan (x)
     unsigned EMUSHORT x[];
{

  ecleaz (x);
  x[E] = 0x7fff;
  x[M + 1] = 0xc000;
}

/* Return nonzero if exploded e-type X is a NaN.  */

static int
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

/* Return nonzero if sign of exploded e-type X is nonzero.  */

static int
eiisneg (x)
     unsigned EMUSHORT x[];
{

  return x[0] != 0;
}

#if 0
/* Fill exploded e-type X with infinity pattern.
   This has maximum exponent and significand all zeros.  */

static void
eiinfin (x)
     unsigned EMUSHORT x[];
{

  ecleaz (x);
  x[E] = 0x7fff;
}
#endif /* 0 */

/* Return nonzero if exploded e-type X is infinite.  */

static int
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


/* Compare significands of numbers in internal exploded e-type format.
   Guard words are included in the comparison.

   Returns	+1 if a > b
		 0 if a == b
		-1 if a < b   */

static int
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

/* Shift significand of exploded e-type X down by 1 bit.  */

static void
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

/* Shift significand of exploded e-type X up by 1 bit.  */

static void
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


/* Shift significand of exploded e-type X down by 8 bits.  */

static void
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

/* Shift significand of exploded e-type X up by 8 bits.  */

static void
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

/* Shift significand of exploded e-type X up by 16 bits.  */

static void
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

/* Shift significand of exploded e-type X down by 16 bits.  */

static void
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

/* Add significands of exploded e-type X and Y.  X + Y replaces Y.  */

static void
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

/* Subtract significands of exploded e-type X and Y.  Y - X replaces Y.  */

static void
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


static unsigned EMUSHORT equot[NI];


#if 0
/* Radix 2 shift-and-add versions of multiply and divide  */


/* Divide significands */

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

  /* Use faster compare and subtraction if denominator has only 15 bits of
     significance.  */

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

  /* The number of quotient bits to calculate is NBITS + 1 scaling guard
     bit + 1 roundoff bit.  */

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
  while (*p == 0)		/* significand is not supposed to be zero */
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

#else

/* Radix 65536 versions of multiply and divide.  */

/* Multiply significand of e-type number B
   by 16-bit quantity A, return e-type result to C.  */

static void
m16m (a, b, c)
     unsigned int a;
     unsigned EMUSHORT b[], c[];
{
  register unsigned EMUSHORT *pp;
  register unsigned EMULONG carry;
  unsigned EMUSHORT *ps;
  unsigned EMUSHORT p[NI];
  unsigned EMULONG aa, m;
  int i;

  aa = a;
  pp = &p[NI-2];
  *pp++ = 0;
  *pp = 0;
  ps = &b[NI-1];

  for (i=M+1; i<NI; i++)
    {
      if (*ps == 0)
	{
	  --ps;
	  --pp;
	  *(pp-1) = 0;
	}
      else
	{
	  m = (unsigned EMULONG) aa * *ps--;
	  carry = (m & 0xffff) + *pp;
	  *pp-- = (unsigned EMUSHORT)carry;
	  carry = (carry >> 16) + (m >> 16) + *pp;
	  *pp = (unsigned EMUSHORT)carry;
	  *(pp-1) = carry >> 16;
	}
    }
  for (i=M; i<NI; i++)
    c[i] = p[i];
}

/* Divide significands of exploded e-types NUM / DEN.  Neither the
   numerator NUM nor the denominator DEN is permitted to have its high guard
   word nonzero.  */

static int
edivm (den, num)
     unsigned EMUSHORT den[], num[];
{
  int i;
  register unsigned EMUSHORT *p;
  unsigned EMULONG tnum;
  unsigned EMUSHORT j, tdenm, tquot;
  unsigned EMUSHORT tprod[NI+1];

  p = &equot[0];
  *p++ = num[0];
  *p++ = num[1];

  for (i=M; i<NI; i++)
    {
      *p++ = 0;
    }
  eshdn1 (num);
  tdenm = den[M+1];
  for (i=M; i<NI; i++)
    {
      /* Find trial quotient digit (the radix is 65536).  */
      tnum = (((unsigned EMULONG) num[M]) << 16) + num[M+1];

      /* Do not execute the divide instruction if it will overflow.  */
      if ((tdenm * (unsigned long)0xffff) < tnum)
	tquot = 0xffff;
      else
	tquot = tnum / tdenm;
      /* Multiply denominator by trial quotient digit.  */
      m16m ((unsigned int)tquot, den, tprod);
      /* The quotient digit may have been overestimated.  */
      if (ecmpm (tprod, num) > 0)
	{
	  tquot -= 1;
	  esubm (den, tprod);
	  if (ecmpm (tprod, num) > 0)
	    {
	      tquot -= 1;
	      esubm (den, tprod);
	    }
	}
      esubm (tprod, num);
      equot[i] = tquot;
      eshup6(num);
    }
  /* test for nonzero remainder after roundoff bit */
  p = &num[M];
  j = 0;
  for (i=M; i<NI; i++)
    {
      j |= *p++;
    }
  if (j)
    j = 1;

  for (i=0; i<NI; i++)
    num[i] = equot[i];

  return ((int)j);
}

/* Multiply significands of exploded e-type A and B, result in B.  */

static int
emulm (a, b)
     unsigned EMUSHORT a[], b[];
{
  unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT pprod[NI];
  unsigned EMUSHORT j;
  int i;

  equot[0] = b[0];
  equot[1] = b[1];
  for (i=M; i<NI; i++)
    equot[i] = 0;

  j = 0;
  p = &a[NI-1];
  q = &equot[NI-1];
  for (i=M+1; i<NI; i++)
    {
      if (*p == 0)
	{
	  --p;
	}
      else
	{
	  m16m ((unsigned int) *p--, b, pprod);
	  eaddm(pprod, equot);
	}
      j |= *q;
      eshdn6(equot);
    }

  for (i=0; i<NI; i++)
    b[i] = equot[i];

  /* return flag for lost nonzero bits */
  return ((int)j);
}
#endif


/* Normalize and round off.

  The internal format number to be rounded is S.
  Input LOST is 0 if the value is exact.  This is the so-called sticky bit.

  Input SUBFLG indicates whether the number was obtained
  by a subtraction operation.  In that case if LOST is nonzero
  then the number is slightly smaller than indicated.

  Input EXP is the biased exponent, which may be negative.
  the exponent field of S is ignored but is replaced by
  EXP as adjusted by normalization and rounding.

  Input RCNTRL is the rounding control.  If it is nonzero, the
  returned value will be rounded to RNDPRC bits.

  For future reference:  In order for emdnorm to round off denormal
   significands at the right point, the input exponent must be
   adjusted to be the actual value it would have after conversion to
   the final floating point type.  This adjustment has been
   implemented for all type conversions (etoe53, etc.) and decimal
   conversions, but not for the arithmetic functions (eadd, etc.).
   Data types having standard 15-bit exponents are not affected by
   this, but SFmode and DFmode are affected. For example, ediv with
   rndprc = 24 will not round correctly to 24-bit precision if the
   result is denormal.   */

static int rlast = -1;
static int rw = 0;
static unsigned EMUSHORT rmsk = 0;
static unsigned EMUSHORT rmbit = 0;
static unsigned EMUSHORT rebit = 0;
static int re = 0;
static unsigned EMUSHORT rbit[NI];

static void
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

  /* a blank significand could mean either zero or infinity.  */
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
  /* Round off, unless told not to by rcntrl.  */
  if (rcntrl == 0)
    goto mdfin;
  /* Set up rounding parameters if the control register changed.  */
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
	  re = rw - 1;
	  rebit = 1;
	  break;

	case 113:
	  rw = 10;
	  rmsk = 0x7fff;
	  rmbit = 0x4000;
	  rebit = 0x8000;
	  re = rw;
	  break;

	case 64:
	  rw = 7;
	  rmsk = 0xffff;
	  rmbit = 0x8000;
	  re = rw - 1;
	  rebit = 1;
	  break;

	  /* For DEC or IBM arithmetic */
	case 56:
	  rw = 6;
	  rmsk = 0xff;
	  rmbit = 0x80;
	  rebit = 0x100;
	  re = rw;
	  break;

	case 53:
	  rw = 6;
	  rmsk = 0x7ff;
	  rmbit = 0x0400;
	  rebit = 0x800;
	  re = rw;
	  break;

	  /* For C4x arithmetic */
	case 32:
	  rw = 5;
	  rmsk = 0xffff;
	  rmbit = 0x8000;
	  rebit = 1;
	  re = rw - 1;
	  break;

	case 24:
	  rw = 4;
	  rmsk = 0xff;
	  rmbit = 0x80;
	  rebit = 0x100;
	  re = rw;
	  break;
	}
      rbit[re] = rebit;
      rlast = rndprc;
    }

  /* Shift down 1 temporarily if the data structure has an implied
     most significant bit and the number is denormal.
     Intel long double denormals also lose one bit of precision.  */
  if ((exp <= 0) && (rndprc != NBITS)
      && ((rndprc != 64) || ((rndprc == 64) && ! REAL_WORDS_BIG_ENDIAN)))
    {
      lost |= s[NI - 1] & 1;
      eshdn1 (s);
    }
  /* Clear out all bits below the rounding bit,
     remembering in r if any were nonzero.  */
  r = s[rw] & rmsk;
  if (rndprc < NBITS)
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
  s[rw] &= ~rmsk;
  if ((r & rmbit) != 0)
    {
#ifndef C4X
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
#endif
      eaddm (rbit, s);
    }
 mddone:
/* Undo the temporary shift for denormal values.  */
  if ((exp <= 0) && (rndprc != NBITS)
      && ((rndprc != 64) || ((rndprc == 64) && ! REAL_WORDS_BIG_ENDIAN)))
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
      if ((rndprc < 64) || (rndprc == 113))
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

/*  Subtract.  C = B - A, all e type numbers.  */

static int subflg = 0;

static void
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
   Test for subtracting infinities of the same sign.  */
  if (eisinf (a) && eisinf (b)
      && ((eisneg (a) ^ eisneg (b)) == 0))
    {
      mtherr ("esub", INVALID);
      enan (c, 0);
      return;
    }
#endif
  subflg = 1;
  eadd1 (a, b, c);
}

/* Add.  C = A + B, all e type.  */

static void
eadd (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{

#ifdef NANS
/* NaN plus anything is a NaN.  */
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
   Test for adding infinities of opposite signs.  */
  if (eisinf (a) && eisinf (b)
      && ((eisneg (a) ^ eisneg (b)) != 0))
    {
      mtherr ("esub", INVALID);
      enan (c, 0);
      return;
    }
#endif
  subflg = 0;
  eadd1 (a, b, c);
}

/* Arithmetic common to both addition and subtraction.  */

static void
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
	  /* double denormalized tiny number */
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
		  ltb += 1;
		  if (ltb >= 0x7fff)
		    {
		      eclear (c);
		      if (ai[0] != 0)
			eneg (c);
		      einfin (c);
		      return;
		    }
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

/* Divide: C = B/A, all e type.  */

static void
ediv (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{
  unsigned EMUSHORT ai[NI], bi[NI];
  int i, sign;
  EMULONG lt, lta, ltb;

/* IEEE says if result is not a NaN, the sign is "-" if and only if
   operands have opposite signs -- but flush -0 to 0 later if not IEEE.  */
  sign = eisneg(a) ^ eisneg(b);

#ifdef NANS
/* Return any NaN input.  */
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
/* Zero over zero, or infinity over infinity, is a NaN.  */
  if (((ecmp (a, ezero) == 0) && (ecmp (b, ezero) == 0))
      || (eisinf (a) && eisinf (b)))
    {
    mtherr ("ediv", INVALID);
    enan (c, sign);
    return;
    }
#endif
/* Infinity over anything else is infinity.  */
#ifdef INFINITY
  if (eisinf (b))
    {
      einfin (c);
      goto divsign;
    }
/* Anything else over infinity is zero.  */
  if (eisinf (a))
    {
      eclear (c);
      goto divsign;
    }
#endif
  emovi (a, ai);
  emovi (b, bi);
  lta = ai[E];
  ltb = bi[E];
  if (bi[E] == 0)
    {				/* See if numerator is zero.  */
      for (i = 1; i < NI - 1; i++)
	{
	  if (bi[i] != 0)
	    {
	      ltb -= enormlz (bi);
	      goto dnzro1;
	    }
	}
      eclear (c);
      goto divsign;
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
/* Divide by zero is not an invalid operation.
   It is a divide-by-zero operation!   */
      einfin (c);
      mtherr ("ediv", SING);
      goto divsign;
    }
 dnzro2:

  i = edivm (ai, bi);
  /* calculate exponent */
  lt = ltb - lta + EXONE;
  emdnorm (bi, i, 0, lt, 64);
  emovo (bi, c);

 divsign:

  if (sign
#ifndef IEEE
      && (ecmp (c, ezero) != 0)
#endif
      )
     *(c+(NE-1)) |= 0x8000;
  else
     *(c+(NE-1)) &= ~0x8000;
}

/* Multiply e-types A and B, return e-type product C.   */

static void
emul (a, b, c)
     unsigned EMUSHORT *a, *b, *c;
{
  unsigned EMUSHORT ai[NI], bi[NI];
  int i, j, sign;
  EMULONG lt, lta, ltb;

/* IEEE says if result is not a NaN, the sign is "-" if and only if
   operands have opposite signs -- but flush -0 to 0 later if not IEEE.  */
  sign = eisneg(a) ^ eisneg(b);

#ifdef NANS
/* NaN times anything is the same NaN.  */
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
/* Zero times infinity is a NaN.  */
  if ((eisinf (a) && (ecmp (b, ezero) == 0))
      || (eisinf (b) && (ecmp (a, ezero) == 0)))
    {
    mtherr ("emul", INVALID);
    enan (c, sign);
    return;
    }
#endif
/* Infinity times anything else is infinity.  */
#ifdef INFINITY
  if (eisinf (a) || eisinf (b))
    {
      einfin (c);
      goto mulsign;
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
      goto mulsign;
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
      goto mulsign;
    }
 mnzer2:

  /* Multiply significands */
  j = emulm (ai, bi);
  /* calculate exponent */
  lt = lta + ltb - (EXONE - 1);
  emdnorm (bi, j, 0, lt, 64);
  emovo (bi, c);

 mulsign:

  if (sign
#ifndef IEEE
      && (ecmp (c, ezero) != 0)
#endif
      )
     *(c+(NE-1)) |= 0x8000;
  else
     *(c+(NE-1)) &= ~0x8000;
}

/* Convert double precision PE to e-type Y.  */

static void
e53toe (pe, y)
     unsigned EMUSHORT *pe, *y;
{
#ifdef DEC

  dectoe (pe, y);

#else
#ifdef IBM

  ibmtoe (pe, y, DFmode);

#else
#ifdef C4X

  c4xtoe (pe, y, HFmode);

#else
  register unsigned EMUSHORT r;
  register unsigned EMUSHORT *e, *p;
  unsigned EMUSHORT yy[NI];
  int denorm, k;

  e = pe;
  denorm = 0;			/* flag if denormalized number */
  ecleaz (yy);
  if (! REAL_WORDS_BIG_ENDIAN)
    e += 3;
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
      if (! REAL_WORDS_BIG_ENDIAN)
	{
	  if (((pe[3] & 0xf) != 0) || (pe[2] != 0)
	      || (pe[1] != 0) || (pe[0] != 0))
	    {
	      enan (y, yy[0] != 0);
	      return;
	    }
	}
      else
	{
	  if (((pe[0] & 0xf) != 0) || (pe[1] != 0)
	      || (pe[2] != 0) || (pe[3] != 0))
	    {
	      enan (y, yy[0] != 0);
	      return;
	    }
	}
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
     So take back the understood high significand bit.  */

  if (r == 0)
    {
      denorm = 1;
      yy[M] &= ~0x10;
    }
  r += EXONE - 01777;
  yy[E] = r;
  p = &yy[M + 1];
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    {
      *p++ = *(--e);
      *p++ = *(--e);
      *p++ = *(--e);
    }
  else
    {
      ++e;
      *p++ = *e++;
      *p++ = *e++;
      *p++ = *e++;
    }
#endif
  eshift (yy, -5);
  if (denorm)
    {
	/* If zero exponent, then normalize the significand.  */
      if ((k = enormlz (yy)) > NBITS)
	ecleazs (yy);
      else
	yy[E] -= (unsigned EMUSHORT) (k - 1);
    }
  emovo (yy, y);
#endif /* not C4X */
#endif /* not IBM */
#endif /* not DEC */
}

/* Convert double extended precision float PE to e type Y.  */

static void
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
/* This precision is not ordinarily supported on DEC or IBM.  */
#ifdef DEC
  for (i = 0; i < 5; i++)
    *p++ = *e++;
#endif
#ifdef IBM
  p = &yy[0] + (NE - 1);
  *p-- = *e++;
  ++e;
  for (i = 0; i < 5; i++)
    *p-- = *e++;
#endif
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    {
      for (i = 0; i < 5; i++)
	*p++ = *e++;

      /* For denormal long double Intel format, shift significand up one
	 -- but only if the top significand bit is zero.  A top bit of 1
	 is "pseudodenormal" when the exponent is zero.  */
      if((yy[NE-1] & 0x7fff) == 0 && (yy[NE-2] & 0x8000) == 0)
	{
	  unsigned EMUSHORT temp[NI];

	  emovi(yy, temp);
	  eshup1(temp);
	  emovo(temp,y);
	  return;
	}
    }
  else
    {
      p = &yy[0] + (NE - 1);
#ifdef ARM_EXTENDED_IEEE_FORMAT
      /* For ARMs, the exponent is in the lowest 15 bits of the word.  */
      *p-- = (e[0] & 0x8000) | (e[1] & 0x7ffff);
      e += 2;
#else
      *p-- = *e++;
      ++e;
#endif
      for (i = 0; i < 4; i++)
	*p-- = *e++;
    }
#endif
#ifdef INFINITY
  /* Point to the exponent field and check max exponent cases.  */
  p = &yy[NE - 1];
  if ((*p & 0x7fff) == 0x7fff)
    {
#ifdef NANS
      if (! REAL_WORDS_BIG_ENDIAN)
	{
	  for (i = 0; i < 4; i++)
	    {
	      if ((i != 3 && pe[i] != 0)
		  /* Anything but 0x8000 here, including 0, is a NaN.  */
		  || (i == 3 && pe[i] != 0x8000))
		{
		  enan (y, (*p & 0x8000) != 0);
		  return;
		}
	    }
	}
      else
	{
#ifdef ARM_EXTENDED_IEEE_FORMAT
	  for (i = 2; i <= 5; i++)
	    {
	      if (pe[i] != 0)
		{
		  enan (y, (*p & 0x8000) != 0);
		  return;
		}
	    }
#else /* not ARM */
	  /* In Motorola extended precision format, the most significant
	     bit of an infinity mantissa could be either 1 or 0.  It is
	     the lower order bits that tell whether the value is a NaN.  */
	  if ((pe[2] & 0x7fff) != 0)
	    goto bigend_nan;

	  for (i = 3; i <= 5; i++)
	    {
	      if (pe[i] != 0)
		{
bigend_nan:
		  enan (y, (*p & 0x8000) != 0);
		  return;
		}
	    }
#endif /* not ARM */
	}
#endif /* NANS */
      eclear (y);
      einfin (y);
      if (*p & 0x8000)
	eneg (y);
      return;
    }
#endif  /* INFINITY */
  p = yy;
  q = y;
  for (i = 0; i < NE; i++)
    *q++ = *p++;
}

/* Convert 128-bit long double precision float PE to e type Y.  */

static void
e113toe (pe, y)
     unsigned EMUSHORT *pe, *y;
{
  register unsigned EMUSHORT r;
  unsigned EMUSHORT *e, *p;
  unsigned EMUSHORT yy[NI];
  int denorm, i;

  e = pe;
  denorm = 0;
  ecleaz (yy);
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    e += 7;
#endif
  r = *e;
  yy[0] = 0;
  if (r & 0x8000)
    yy[0] = 0xffff;
  r &= 0x7fff;
#ifdef INFINITY
  if (r == 0x7fff)
    {
#ifdef NANS
      if (! REAL_WORDS_BIG_ENDIAN)
	{
	  for (i = 0; i < 7; i++)
	    {
	      if (pe[i] != 0)
		{
		  enan (y, yy[0] != 0);
		  return;
		}
	    }
	}
      else
	{
	  for (i = 1; i < 8; i++)
	    {
	      if (pe[i] != 0)
		{
		  enan (y, yy[0] != 0);
		  return;
		}
	    }
	}
#endif /* NANS */
      eclear (y);
      einfin (y);
      if (yy[0])
	eneg (y);
      return;
    }
#endif  /* INFINITY */
  yy[E] = r;
  p = &yy[M + 1];
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    {
      for (i = 0; i < 7; i++)
	*p++ = *(--e);
    }
  else
    {
      ++e;
      for (i = 0; i < 7; i++)
	*p++ = *e++;
    }
#endif
/* If denormal, remove the implied bit; else shift down 1.  */
  if (r == 0)
    {
      yy[M] = 0;
    }
  else
    {
      yy[M] = 1;
      eshift (yy, -1);
    }
  emovo (yy, y);
}

/* Convert single precision float PE to e type Y.  */

static void
e24toe (pe, y)
     unsigned EMUSHORT *pe, *y;
{
#ifdef IBM

  ibmtoe (pe, y, SFmode);

#else

#ifdef C4X

  c4xtoe (pe, y, QFmode);

#else

  register unsigned EMUSHORT r;
  register unsigned EMUSHORT *e, *p;
  unsigned EMUSHORT yy[NI];
  int denorm, k;

  e = pe;
  denorm = 0;			/* flag if denormalized number */
  ecleaz (yy);
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
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
      if (REAL_WORDS_BIG_ENDIAN)
	{
	  if (((pe[0] & 0x7f) != 0) || (pe[1] != 0))
	    {
	      enan (y, yy[0] != 0);
	      return;
	    }
	}
      else
	{
	  if (((pe[1] & 0x7f) != 0) || (pe[0] != 0))
	    {
	      enan (y, yy[0] != 0);
	      return;
	    }
	}
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
     So take back the understood high significand bit.  */
  if (r == 0)
    {
      denorm = 1;
      yy[M] &= ~0200;
    }
  r += EXONE - 0177;
  yy[E] = r;
  p = &yy[M + 1];
#ifdef DEC
  *p++ = *(--e);
#endif
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    *p++ = *(--e);
  else
    {
      ++e;
      *p++ = *e++;
    }
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
#endif /* not C4X */
#endif /* not IBM */
}

/* Convert e-type X to IEEE 128-bit long double format E.  */

static void
etoe113 (x, e)
     unsigned EMUSHORT *x, *e;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, eisneg (x), TFmode);
      return;
    }
#endif
  emovi (x, xi);
  exp = (EMULONG) xi[E];
#ifdef INFINITY
  if (eisinf (x))
    goto nonorm;
#endif
  /* round off to nearest or even */
  rndsav = rndprc;
  rndprc = 113;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
 nonorm:
  toe113 (xi, e);
}

/* Convert exploded e-type X, that has already been rounded to
   113-bit precision, to IEEE 128-bit long double format Y.  */

static void
toe113 (a, b)
     unsigned EMUSHORT *a, *b;
{
  register unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT i;

#ifdef NANS
  if (eiisnan (a))
    {
      make_nan (b, eiisneg (a), TFmode);
      return;
    }
#endif
  p = a;
  if (REAL_WORDS_BIG_ENDIAN)
    q = b;
  else
    q = b + 7;			/* point to output exponent */

  /* If not denormal, delete the implied bit.  */
  if (a[E] != 0)
    {
      eshup1 (a);
    }
  /* combine sign and exponent */
  i = *p++;
  if (REAL_WORDS_BIG_ENDIAN)
    {
      if (i)
	*q++ = *p++ | 0x8000;
      else
	*q++ = *p++;
    }
  else
    {
      if (i)
	*q-- = *p++ | 0x8000;
      else
	*q-- = *p++;
    }
  /* skip over guard word */
  ++p;
  /* move the significand */
  if (REAL_WORDS_BIG_ENDIAN)
    {
      for (i = 0; i < 7; i++)
	*q++ = *p++;
    }
  else
    {
      for (i = 0; i < 7; i++)
	*q-- = *p++;
    }
}

/* Convert e-type X to IEEE double extended format E.  */

static void
etoe64 (x, e)
     unsigned EMUSHORT *x, *e;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, eisneg (x), XFmode);
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

/* Convert exploded e-type X, that has already been rounded to
   64-bit precision, to IEEE double extended format Y.  */

static void
toe64 (a, b)
     unsigned EMUSHORT *a, *b;
{
  register unsigned EMUSHORT *p, *q;
  unsigned EMUSHORT i;

#ifdef NANS
  if (eiisnan (a))
    {
      make_nan (b, eiisneg (a), XFmode);
      return;
    }
#endif
  /* Shift denormal long double Intel format significand down one bit.  */
  if ((a[E] == 0) && ! REAL_WORDS_BIG_ENDIAN)
    eshdn1 (a);
  p = a;
#ifdef IBM
  q = b;
#endif
#ifdef DEC
  q = b + 4;
#endif
#ifdef IEEE
  if (REAL_WORDS_BIG_ENDIAN)
    q = b;
  else
    {
      q = b + 4;			/* point to output exponent */
#if LONG_DOUBLE_TYPE_SIZE == 96
      /* Clear the last two bytes of 12-byte Intel format */
      *(q+1) = 0;
#endif
    }
#endif

  /* combine sign and exponent */
  i = *p++;
#ifdef IBM
  if (i)
    *q++ = *p++ | 0x8000;
  else
    *q++ = *p++;
  *q++ = 0;
#endif
#ifdef DEC
  if (i)
    *q-- = *p++ | 0x8000;
  else
    *q-- = *p++;
#endif
#ifdef IEEE
  if (REAL_WORDS_BIG_ENDIAN)
    {
#ifdef ARM_EXTENDED_IEEE_FORMAT
      /* The exponent is in the lowest 15 bits of the first word.  */
      *q++ = i ? 0x8000 : 0;
      *q++ = *p++;
#else
      if (i)
	*q++ = *p++ | 0x8000;
      else
	*q++ = *p++;
      *q++ = 0;
#endif
    }
  else
    {
      if (i)
	*q-- = *p++ | 0x8000;
      else
	*q-- = *p++;
    }
#endif
  /* skip over guard word */
  ++p;
  /* move the significand */
#ifdef IBM
  for (i = 0; i < 4; i++)
    *q++ = *p++;
#endif
#ifdef DEC
  for (i = 0; i < 4; i++)
    *q-- = *p++;
#endif
#ifdef IEEE
  if (REAL_WORDS_BIG_ENDIAN)
    {
      for (i = 0; i < 4; i++)
	*q++ = *p++;
    }
  else
    {
#ifdef INFINITY
      if (eiisinf (a))
	{
	  /* Intel long double infinity significand.  */
	  *q-- = 0x8000;
	  *q-- = 0;
	  *q-- = 0;
	  *q = 0;
	  return;
	}
#endif
      for (i = 0; i < 4; i++)
	*q-- = *p++;
    }
#endif
}

/* e type to double precision.  */

#ifdef DEC
/* Convert e-type X to DEC-format double E.  */

static void
etoe53 (x, e)
     unsigned EMUSHORT *x, *e;
{
  etodec (x, e);		/* see etodec.c */
}

/* Convert exploded e-type X, that has already been rounded to
   56-bit double precision, to DEC double Y.  */

static void
toe53 (x, y)
     unsigned EMUSHORT *x, *y;
{
  todec (x, y);
}

#else
#ifdef IBM
/* Convert e-type X to IBM 370-format double E.  */

static void
etoe53 (x, e)
     unsigned EMUSHORT *x, *e;
{
  etoibm (x, e, DFmode);
}

/* Convert exploded e-type X, that has already been rounded to
   56-bit precision, to IBM 370 double Y.  */

static void
toe53 (x, y)
     unsigned EMUSHORT *x, *y;
{
  toibm (x, y, DFmode);
}

#else /* it's neither DEC nor IBM */
#ifdef C4X
/* Convert e-type X to C4X-format long double E.  */

static void
etoe53 (x, e)
     unsigned EMUSHORT *x, *e;
{
  etoc4x (x, e, HFmode);
}

/* Convert exploded e-type X, that has already been rounded to
   56-bit precision, to IBM 370 double Y.  */

static void
toe53 (x, y)
     unsigned EMUSHORT *x, *y;
{
  toc4x (x, y, HFmode);
}

#else  /* it's neither DEC nor IBM nor C4X */

/* Convert e-type X to IEEE double E.  */

static void
etoe53 (x, e)
     unsigned EMUSHORT *x, *e;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, eisneg (x), DFmode);
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

/* Convert exploded e-type X, that has already been rounded to
   53-bit precision, to IEEE double Y.  */

static void
toe53 (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMUSHORT i;
  unsigned EMUSHORT *p;

#ifdef NANS
  if (eiisnan (x))
    {
      make_nan (y, eiisneg (x), DFmode);
      return;
    }
#endif
  p = &x[0];
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    y += 3;
#endif
  *y = 0;			/* output high order */
  if (*p++)
    *y = 0x8000;		/* output sign bit */

  i = *p++;
  if (i >= (unsigned int) 2047)
    {
      /* Saturate at largest number less than infinity.  */
#ifdef INFINITY
      *y |= 0x7ff0;
      if (! REAL_WORDS_BIG_ENDIAN)
	{
	  *(--y) = 0;
	  *(--y) = 0;
	  *(--y) = 0;
	}
      else
	{
	  ++y;
	  *y++ = 0;
	  *y++ = 0;
	  *y++ = 0;
	}
#else
      *y |= (unsigned EMUSHORT) 0x7fef;
      if (! REAL_WORDS_BIG_ENDIAN)
	{
	  *(--y) = 0xffff;
	  *(--y) = 0xffff;
	  *(--y) = 0xffff;
	}
      else
	{
	  ++y;
	  *y++ = 0xffff;
	  *y++ = 0xffff;
	  *y++ = 0xffff;
	}
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
  if (! REAL_WORDS_BIG_ENDIAN)
    {
      *(--y) = *p++;
      *(--y) = *p++;
      *(--y) = *p;
    }
  else
    {
      ++y;
      *y++ = *p++;
      *y++ = *p++;
      *y++ = *p++;
    }
}

#endif /* not C4X */
#endif /* not IBM */
#endif /* not DEC */



/* e type to single precision.  */

#ifdef IBM
/* Convert e-type X to IBM 370 float E.  */

static void
etoe24 (x, e)
     unsigned EMUSHORT *x, *e;
{
  etoibm (x, e, SFmode);
}

/* Convert exploded e-type X, that has already been rounded to
   float precision, to IBM 370 float Y.  */

static void
toe24 (x, y)
     unsigned EMUSHORT *x, *y;
{
  toibm (x, y, SFmode);
}

#else

#ifdef C4X
/* Convert e-type X to C4X float E.  */

static void
etoe24 (x, e)
     unsigned EMUSHORT *x, *e;
{
  etoc4x (x, e, QFmode);
}

/* Convert exploded e-type X, that has already been rounded to
   float precision, to IBM 370 float Y.  */

static void
toe24 (x, y)
     unsigned EMUSHORT *x, *y;
{
  toc4x (x, y, QFmode);
}

#else

/* Convert e-type X to IEEE float E.  DEC float is the same as IEEE float.  */

static void
etoe24 (x, e)
     unsigned EMUSHORT *x, *e;
{
  EMULONG exp;
  unsigned EMUSHORT xi[NI];
  int rndsav;

#ifdef NANS
  if (eisnan (x))
    {
      make_nan (e, eisneg (x), SFmode);
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

/* Convert exploded e-type X, that has already been rounded to
   float precision, to IEEE float Y.  */

static void
toe24 (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMUSHORT i;
  unsigned EMUSHORT *p;

#ifdef NANS
  if (eiisnan (x))
    {
      make_nan (y, eiisneg (x), SFmode);
      return;
    }
#endif
  p = &x[0];
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    y += 1;
#endif
#ifdef DEC
  y += 1;
#endif
  *y = 0;			/* output high order */
  if (*p++)
    *y = 0x8000;		/* output sign bit */

  i = *p++;
/* Handle overflow cases.  */
  if (i >= 255)
    {
#ifdef INFINITY
      *y |= (unsigned EMUSHORT) 0x7f80;
#ifdef DEC
      *(--y) = 0;
#endif
#ifdef IEEE
      if (! REAL_WORDS_BIG_ENDIAN)
	*(--y) = 0;
      else
	{
	  ++y;
	  *y = 0;
	}
#endif
#else  /* no INFINITY */
      *y |= (unsigned EMUSHORT) 0x7f7f;
#ifdef DEC
      *(--y) = 0xffff;
#endif
#ifdef IEEE
      if (! REAL_WORDS_BIG_ENDIAN)
	*(--y) = 0xffff;
      else
	{
	  ++y;
	  *y = 0xffff;
	}
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
  /* High order output already has sign bit set.  */
  *y |= i;
#ifdef DEC
  *(--y) = *p;
#endif
#ifdef IEEE
  if (! REAL_WORDS_BIG_ENDIAN)
    *(--y) = *p;
  else
    {
      ++y;
      *y = *p;
    }
#endif
}
#endif  /* not C4X */
#endif  /* not IBM */

/* Compare two e type numbers.
   Return +1 if a > b
           0 if a == b
          -1 if a < b
          -2 if either a or b is a NaN.  */

static int
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

#if 0
/* Find e-type nearest integer to X, as floor (X + 0.5).  */

static void
eround (x, y)
     unsigned EMUSHORT *x, *y;
{
  eadd (ehalf, x, y);
  efloor (y, y);
}
#endif /* 0 */

/* Convert HOST_WIDE_INT LP to e type Y.  */

static void
ltoe (lp, y)
     HOST_WIDE_INT *lp;
     unsigned EMUSHORT *y;
{
  unsigned EMUSHORT yi[NI];
  unsigned HOST_WIDE_INT ll;
  int k;

  ecleaz (yi);
  if (*lp < 0)
    {
      /* make it positive */
      ll = (unsigned HOST_WIDE_INT) (-(*lp));
      yi[0] = 0xffff;		/* put correct sign in the e type number */
    }
  else
    {
      ll = (unsigned HOST_WIDE_INT) (*lp);
    }
  /* move the long integer to yi significand area */
#if HOST_BITS_PER_WIDE_INT == 64
  yi[M] = (unsigned EMUSHORT) (ll >> 48);
  yi[M + 1] = (unsigned EMUSHORT) (ll >> 32);
  yi[M + 2] = (unsigned EMUSHORT) (ll >> 16);
  yi[M + 3] = (unsigned EMUSHORT) ll;
  yi[E] = EXONE + 47;		/* exponent if normalize shift count were 0 */
#else
  yi[M] = (unsigned EMUSHORT) (ll >> 16);
  yi[M + 1] = (unsigned EMUSHORT) ll;
  yi[E] = EXONE + 15;		/* exponent if normalize shift count were 0 */
#endif

  if ((k = enormlz (yi)) > NBITS)/* normalize the significand */
    ecleaz (yi);		/* it was zero */
  else
    yi[E] -= (unsigned EMUSHORT) k;/* subtract shift count from exponent */
  emovo (yi, y);		/* output the answer */
}

/* Convert unsigned HOST_WIDE_INT LP to e type Y.  */

static void
ultoe (lp, y)
     unsigned HOST_WIDE_INT *lp;
     unsigned EMUSHORT *y;
{
  unsigned EMUSHORT yi[NI];
  unsigned HOST_WIDE_INT ll;
  int k;

  ecleaz (yi);
  ll = *lp;

  /* move the long integer to ayi significand area */
#if HOST_BITS_PER_WIDE_INT == 64
  yi[M] = (unsigned EMUSHORT) (ll >> 48);
  yi[M + 1] = (unsigned EMUSHORT) (ll >> 32);
  yi[M + 2] = (unsigned EMUSHORT) (ll >> 16);
  yi[M + 3] = (unsigned EMUSHORT) ll;
  yi[E] = EXONE + 47;		/* exponent if normalize shift count were 0 */
#else
  yi[M] = (unsigned EMUSHORT) (ll >> 16);
  yi[M + 1] = (unsigned EMUSHORT) ll;
  yi[E] = EXONE + 15;		/* exponent if normalize shift count were 0 */
#endif

  if ((k = enormlz (yi)) > NBITS)/* normalize the significand */
    ecleaz (yi);		/* it was zero */
  else
    yi[E] -= (unsigned EMUSHORT) k;  /* subtract shift count from exponent */
  emovo (yi, y);		/* output the answer */
}


/* Find signed HOST_WIDE_INT integer I and floating point fractional
   part FRAC of e-type (packed internal format) floating point input X.
   The integer output I has the sign of the input, except that
   positive overflow is permitted if FIXUNS_TRUNC_LIKE_FIX_TRUNC.
   The output e-type fraction FRAC is the positive fractional
   part of abs (X).  */

static void
eifrac (x, i, frac)
     unsigned EMUSHORT *x;
     HOST_WIDE_INT *i;
     unsigned EMUSHORT *frac;
{
  unsigned EMUSHORT xi[NI];
  int j, k;
  unsigned HOST_WIDE_INT ll;

  emovi (x, xi);
  k = (int) xi[E] - (EXONE - 1);
  if (k <= 0)
    {
      /* if exponent <= 0, integer = 0 and real output is fraction */
      *i = 0L;
      emovo (xi, frac);
      return;
    }
  if (k > (HOST_BITS_PER_WIDE_INT - 1))
    {
      /* long integer overflow: output large integer
	 and correct fraction  */
      if (xi[0])
	*i = ((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1);
      else
	{
#ifdef FIXUNS_TRUNC_LIKE_FIX_TRUNC
	  /* In this case, let it overflow and convert as if unsigned.  */
	  euifrac (x, &ll, frac);
	  *i = (HOST_WIDE_INT) ll;
	  return;
#else
	  /* In other cases, return the largest positive integer.  */
	  *i = (((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1)) - 1;
#endif
	}
      eshift (xi, k);
      if (extra_warnings)
	warning ("overflow on truncation to integer");
    }
  else if (k > 16)
    {
      /* Shift more than 16 bits: first shift up k-16 mod 16,
	 then shift up by 16's.  */
      j = k - ((k >> 4) << 4);
      eshift (xi, j);
      ll = xi[M];
      k -= j;
      do
	{
	  eshup6 (xi);
	  ll = (ll << 16) | xi[M];
	}
      while ((k -= 16) > 0);
      *i = ll;
      if (xi[0])
	*i = -(*i);
    }
  else
      {
        /* shift not more than 16 bits */
          eshift (xi, k);
        *i = (HOST_WIDE_INT) xi[M] & 0xffff;
        if (xi[0])
	  *i = -(*i);
      }
  xi[0] = 0;
  xi[E] = EXONE - 1;
  xi[M] = 0;
  if ((k = enormlz (xi)) > NBITS)
    ecleaz (xi);
  else
    xi[E] -= (unsigned EMUSHORT) k;

  emovo (xi, frac);
}


/* Find unsigned HOST_WIDE_INT integer I and floating point fractional part
   FRAC of e-type X.  A negative input yields integer output = 0 but
   correct fraction.  */

static void
euifrac (x, i, frac)
     unsigned EMUSHORT *x;
     unsigned HOST_WIDE_INT *i;
     unsigned EMUSHORT *frac;
{
  unsigned HOST_WIDE_INT ll;
  unsigned EMUSHORT xi[NI];
  int j, k;

  emovi (x, xi);
  k = (int) xi[E] - (EXONE - 1);
  if (k <= 0)
    {
      /* if exponent <= 0, integer = 0 and argument is fraction */
      *i = 0L;
      emovo (xi, frac);
      return;
    }
  if (k > HOST_BITS_PER_WIDE_INT)
    {
      /* Long integer overflow: output large integer
	 and correct fraction.
	 Note, the BSD microvax compiler says that ~(0UL)
	 is a syntax error.  */
      *i = ~(0L);
      eshift (xi, k);
      if (extra_warnings)
	warning ("overflow on truncation to unsigned integer");
    }
  else if (k > 16)
    {
      /* Shift more than 16 bits: first shift up k-16 mod 16,
	 then shift up by 16's.  */
      j = k - ((k >> 4) << 4);
      eshift (xi, j);
      ll = xi[M];
      k -= j;
      do
	{
	  eshup6 (xi);
	  ll = (ll << 16) | xi[M];
	}
      while ((k -= 16) > 0);
      *i = ll;
    }
  else
    {
      /* shift not more than 16 bits */
      eshift (xi, k);
      *i = (HOST_WIDE_INT) xi[M] & 0xffff;
    }

  if (xi[0])  /* A negative value yields unsigned integer 0.  */
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

/* Shift the significand of exploded e-type X up or down by SC bits.  */

static int
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

/* Shift normalize the significand area of exploded e-type X.
   Return the shift count (up = positive).  */

static int
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
       Return true if all are zero.  */
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

/* Powers of ten used in decimal <-> binary conversions.  */

#define NTEN 12
#define MAXP 4096

#if LONG_DOUBLE_TYPE_SIZE == 128
static unsigned EMUSHORT etens[NTEN + 1][NE] =
{
  {0x6576, 0x4a92, 0x804a, 0x153f,
   0xc94c, 0x979a, 0x8a20, 0x5202, 0xc460, 0x7525,},	/* 10**4096 */
  {0x6a32, 0xce52, 0x329a, 0x28ce,
   0xa74d, 0x5de4, 0xc53d, 0x3b5d, 0x9e8b, 0x5a92,},	/* 10**2048 */
  {0x526c, 0x50ce, 0xf18b, 0x3d28,
   0x650d, 0x0c17, 0x8175, 0x7586, 0xc976, 0x4d48,},
  {0x9c66, 0x58f8, 0xbc50, 0x5c54,
   0xcc65, 0x91c6, 0xa60e, 0xa0ae, 0xe319, 0x46a3,},
  {0x851e, 0xeab7, 0x98fe, 0x901b,
   0xddbb, 0xde8d, 0x9df9, 0xebfb, 0xaa7e, 0x4351,},
  {0x0235, 0x0137, 0x36b1, 0x336c,
   0xc66f, 0x8cdf, 0x80e9, 0x47c9, 0x93ba, 0x41a8,},
  {0x50f8, 0x25fb, 0xc76b, 0x6b71,
   0x3cbf, 0xa6d5, 0xffcf, 0x1f49, 0xc278, 0x40d3,},
  {0x0000, 0x0000, 0x0000, 0x0000,
   0xf020, 0xb59d, 0x2b70, 0xada8, 0x9dc5, 0x4069,},
  {0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0400, 0xc9bf, 0x8e1b, 0x4034,},
  {0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x2000, 0xbebc, 0x4019,},
  {0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x9c40, 0x400c,},
  {0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0xc800, 0x4005,},
  {0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0xa000, 0x4002,},	/* 10**1 */
};

static unsigned EMUSHORT emtens[NTEN + 1][NE] =
{
  {0x2030, 0xcffc, 0xa1c3, 0x8123,
   0x2de3, 0x9fde, 0xd2ce, 0x04c8, 0xa6dd, 0x0ad8,},	/* 10**-4096 */
  {0x8264, 0xd2cb, 0xf2ea, 0x12d4,
   0x4925, 0x2de4, 0x3436, 0x534f, 0xceae, 0x256b,},	/* 10**-2048 */
  {0xf53f, 0xf698, 0x6bd3, 0x0158,
   0x87a6, 0xc0bd, 0xda57, 0x82a5, 0xa2a6, 0x32b5,},
  {0xe731, 0x04d4, 0xe3f2, 0xd332,
   0x7132, 0xd21c, 0xdb23, 0xee32, 0x9049, 0x395a,},
  {0xa23e, 0x5308, 0xfefb, 0x1155,
   0xfa91, 0x1939, 0x637a, 0x4325, 0xc031, 0x3cac,},
  {0xe26d, 0xdbde, 0xd05d, 0xb3f6,
   0xac7c, 0xe4a0, 0x64bc, 0x467c, 0xddd0, 0x3e55,},
  {0x2a20, 0x6224, 0x47b3, 0x98d7,
   0x3f23, 0xe9a5, 0xa539, 0xea27, 0xa87f, 0x3f2a,},
  {0x0b5b, 0x4af2, 0xa581, 0x18ed,
   0x67de, 0x94ba, 0x4539, 0x1ead, 0xcfb1, 0x3f94,},
  {0xbf71, 0xa9b3, 0x7989, 0xbe68,
   0x4c2e, 0xe15b, 0xc44d, 0x94be, 0xe695, 0x3fc9,},
  {0x3d4d, 0x7c3d, 0x36ba, 0x0d2b,
   0xfdc2, 0xcefc, 0x8461, 0x7711, 0xabcc, 0x3fe4,},
  {0xc155, 0xa4a8, 0x404e, 0x6113,
   0xd3c3, 0x652b, 0xe219, 0x1758, 0xd1b7, 0x3ff1,},
  {0xd70a, 0x70a3, 0x0a3d, 0xa3d7,
   0x3d70, 0xd70a, 0x70a3, 0x0a3d, 0xa3d7, 0x3ff8,},
  {0xcccd, 0xcccc, 0xcccc, 0xcccc,
   0xcccc, 0xcccc, 0xcccc, 0xcccc, 0xcccc, 0x3ffb,},	/* 10**-1 */
};
#else
/* LONG_DOUBLE_TYPE_SIZE is other than 128 */
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
#endif

#if 0
/* Convert float value X to ASCII string STRING with NDIG digits after
   the decimal point.  */

static void
e24toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e24toe (x, w);
  etoasc (w, string, ndigs);
}

/* Convert double value X to ASCII string STRING with NDIG digits after
   the decimal point.  */

static void
e53toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e53toe (x, w);
  etoasc (w, string, ndigs);
}

/* Convert double extended value X to ASCII string STRING with NDIG digits
   after the decimal point.  */

static void
e64toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e64toe (x, w);
  etoasc (w, string, ndigs);
}

/* Convert 128-bit long double value X to ASCII string STRING with NDIG digits
   after the decimal point.  */

static void
e113toasc (x, string, ndigs)
     unsigned EMUSHORT x[];
     char *string;
     int ndigs;
{
  unsigned EMUSHORT w[NI];

  e113toe (x, w);
  etoasc (w, string, ndigs);
}
#endif /* 0 */

/* Convert e-type X to ASCII string STRING with NDIGS digits after
   the decimal point.  */

static char wstring[80];	/* working storage for ASCII output */

static void
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
      goto isone;		/* valid all zeros */
    }
 tnzro:

  /* Test for infinity.  */
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
      /* Convert significand to an integer and strip trailing decimal zeros.  */
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
      /* An unordered compare result shouldn't happen here.  */
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
      /* Pad significand with trailing decimal zeros.  */
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
  /* Find the first (leading) digit.  */
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
  /* Examine number of digits requested by caller.  */
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
      *s++ = (char)digit + '0';
      *s++ = '.';
    }
  /* Generate digits after the decimal point.  */
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
      /* Test for critical rounding case in ASCII output.  */
      if (digit == 5)
	{
	  emovo (y, t);
	  if (ecmp (t, ezero) != 0)
	    goto roun;		/* round to nearest */
#ifndef C4X
	  if ((*(s - 1) & 1) == 0)
	    goto doexp;		/* round to even */
#endif
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


/* Convert ASCII string to floating point.

   Numeric input is a free format decimal number of any length, with
   or without decimal point.  Entering E after the number followed by an
   integer number causes the second number to be interpreted as a power of
   10 to be multiplied by the first number (i.e., "scientific" notation).  */

/* Convert ASCII string S to single precision float value Y.  */

static void
asctoe24 (s, y)
     const char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, 24);
}


/* Convert ASCII string S to double precision value Y.  */

static void
asctoe53 (s, y)
     const char *s;
     unsigned EMUSHORT *y;
{
#if defined(DEC) || defined(IBM)
  asctoeg (s, y, 56);
#else
#if defined(C4X)
  asctoeg (s, y, 32);
#else
  asctoeg (s, y, 53);
#endif
#endif
}


/* Convert ASCII string S to double extended value Y.  */

static void
asctoe64 (s, y)
     const char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, 64);
}

/* Convert ASCII string S to 128-bit long double Y.  */

static void
asctoe113 (s, y)
     const char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, 113);
}

/* Convert ASCII string S to e type Y.  */

static void
asctoe (s, y)
     const char *s;
     unsigned EMUSHORT *y;
{
  asctoeg (s, y, NBITS);
}

/* Convert ASCII string SS to e type Y, with a specified rounding precision
   of OPREC bits.  BASE is 16 for C9X hexadecimal floating constants.  */

static void
asctoeg (ss, y, oprec)
     const char *ss;
     unsigned EMUSHORT *y;
     int oprec;
{
  unsigned EMUSHORT yy[NI], xt[NI], tt[NI];
  int esign, decflg, sgnflg, nexp, exp, prec, lost;
  int k, trail, c, rndsav;
  EMULONG lexp;
  unsigned EMUSHORT nsign, *p;
  char *sp, *s, *lstr;
  int base = 10;

  /* Copy the input string.  */
  lstr = (char *) alloca (strlen (ss) + 1);

  while (*ss == ' ')		/* skip leading spaces */
    ++ss;

  sp = lstr;
  while ((*sp++ = *ss++) != '\0')
    ;
  s = lstr;

  if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
    {
      base = 16;
      s += 2;
    }

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
  if (*s >= '0' && *s <= '9')
    k = *s - '0';
  else if (*s >= 'a')
    k = 10 + *s - 'a';
  else
    k = 10 + *s - 'A';
  if ((k >= 0) && (k < base))
    {
      /* Ignore leading zeros */
      if ((prec == 0) && (decflg == 0) && (k == 0))
	goto donchr;
      /* Identify and strip trailing zeros after the decimal point.  */
      if ((trail == 0) && (decflg != 0))
	{
	  sp = s;
	  while ((*sp >= '0' && *sp <= '9')
		 || (base == 16 && ((*sp >= 'a' && *sp <= 'f')
				    || (*sp >= 'A' && *sp <= 'F'))))
	    ++sp;
	  /* Check for syntax error */
	  c = *sp & 0x7f;
	  if ((base != 10 || ((c != 'e') && (c != 'E')))
	      && (base != 16 || ((c != 'p') && (c != 'P')))
	      && (c != '\0')
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
	 continuing until overflow into the high guard word yy[2]
	 guarantees that there will be a roundoff bit at the top
	 of the low guard word after normalization.  */

      if (yy[2] == 0)
	{
	  if (base == 16)
	    {
	      if (decflg)
		nexp += 4;	/* count digits after decimal point */

	      eshup1 (yy);	/* multiply current number by 16 */
	      eshup1 (yy);
	      eshup1 (yy);
	      eshup1 (yy);
	    }
	  else
	    {
	      if (decflg)
		nexp += 1;	/* count digits after decimal point */

	      eshup1 (yy);	/* multiply current number by 10 */
	      emovz (yy, xt);
	      eshup1 (xt);
	      eshup1 (xt);
	      eaddm (xt, yy);
	    }
	  /* Insert the current digit.  */
	  ecleaz (xt);
	  xt[NI - 2] = (unsigned EMUSHORT) k;
	  eaddm (xt, yy);
	}
      else
	{
	  /* Mark any lost non-zero digit.  */
	  lost |= k;
	  /* Count lost digits before the decimal point.  */
	  if (decflg == 0)
	    {
	      if (base == 10)
		nexp -= 1;
	      else
		nexp -= 4;
	    }
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
    case 'P':
    case 'p':
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
  /* 0.0eXXX is zero, regardless of XXX.  Check for the 0.0. */
  for (k = 0; k < NI; k++)
    {
      if (yy[k] != 0)
	goto read_expnt;
    }
  goto aexit;

read_expnt:
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
      if (exp > 999999)
 	break;
    }
  if (esign < 0)
    exp = -exp;
  if ((exp > MAXDECEXP) && (base == 10))
    {
 infinite:
      ecleaz (yy);
      yy[E] = 0x7fff;		/* infinity */
      goto aexit;
    }
  if ((exp < MINDECEXP) && (base == 10))
    {
 zero:
      ecleaz (yy);
      goto aexit;
    }

 daldone:
  if (base == 16)
    {
      /* Base 16 hexadecimal floating constant.  */
      if ((k = enormlz (yy)) > NBITS)
	{
	  ecleaz (yy);
	  goto aexit;
	}
      /* Adjust the exponent.  NEXP is the number of hex digits,
         EXP is a power of 2.  */
      lexp = (EXONE - 1 + NBITS) - k + yy[E] + exp - nexp;
      if (lexp > 0x7fff)
	goto infinite;
      if (lexp < 0)
	goto zero;
      yy[E] = lexp;
      goto expdon;
    }

  nexp = exp - nexp;
  /* Pad trailing zeros to minimize power of 10, per IEEE spec.  */
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
  lost = 0;

  /* Convert to external format:

     Multiply by 10**nexp.  If precision is 64 bits,
     the maximum relative error incurred in forming 10**n
     for 0 <= n <= 324 is 8.2e-20, at 10**180.
     For 0 <= n <= 999, the peak relative error is 1.4e-19 at 10**947.
     For 0 >= n >= -999, it is -1.55e-19 at 10**-435.  */

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
	{
	  /* Punt.  Can't handle this without 2 divides.  */
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
  lost = k;

 expdon:

  /* Round and convert directly to the destination type */
  if (oprec == 53)
    lexp -= EXONE - 0x3ff;
#ifdef C4X
  else if (oprec == 24 || oprec == 32)
    lexp -= (EXONE - 0x7f);
#else
#ifdef IBM
  else if (oprec == 24 || oprec == 56)
    lexp -= EXONE - (0x41 << 2);
#else
  else if (oprec == 24)
    lexp -= EXONE - 0177;
#endif /* IBM */
#endif /* C4X */
#ifdef DEC
  else if (oprec == 56)
    lexp -= EXONE - 0201;
#endif
  rndprc = oprec;
  emdnorm (yy, lost, 0, lexp, 64);

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
#ifdef IBM
    case 56:
      toibm (yy, y, DFmode);
      break;
#endif
#ifdef C4X
    case 32:
      toc4x (yy, y, HFmode);
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
    case 113:
      toe113 (yy, y);
      break;
    case NBITS:
      emovo (yy, y);
      break;
    }
}



/* Return Y = largest integer not greater than X (truncated toward minus
   infinity).  */

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

static void
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


#if 0
/* Return S and EXP such that  S * 2^EXP = X and .5 <= S < 1.
   For example, 1.1 = 0.55 * 2^1.  */

static void
efrexp (x, exp, s)
     unsigned EMUSHORT x[];
     int *exp;
     unsigned EMUSHORT s[];
{
  unsigned EMUSHORT xi[NI];
  EMULONG li;

  emovi (x, xi);
  /*  Handle denormalized numbers properly using long integer exponent.  */
  li = (EMULONG) ((EMUSHORT) xi[1]);

  if (li == 0)
    {
      li -= enormlz (xi);
    }
  xi[1] = 0x3ffe;
  emovo (xi, s);
  *exp = (int) (li - 0x3ffe);
}
#endif

/* Return e type Y = X * 2^PWR2.  */

static void
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


#if 0
/* C = remainder after dividing B by A, all e type values.
   Least significant integer quotient bits left in EQUOT.  */

static void
eremain (a, b, c)
     unsigned EMUSHORT a[], b[], c[];
{
  unsigned EMUSHORT den[NI], num[NI];

#ifdef NANS
  if (eisinf (b)
      || (ecmp (a, ezero) == 0)
      || eisnan (a)
      || eisnan (b))
    {
      enan (c, 0);
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
#endif

/*  Return quotient of exploded e-types NUM / DEN in EQUOT,
    remainder in NUM.  */

static void
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
	  j = 0;
      eshup1 (equot);
      equot[NI - 1] |= j;
      eshup1 (num);
      ln -= 1;
    }
  emdnorm (num, 0, 0, ln, 0);
}

/* Report an error condition CODE encountered in function NAME.

    Mnemonic        Value          Significance

     DOMAIN            1       argument domain error
     SING              2       function singularity
     OVERFLOW          3       overflow range error
     UNDERFLOW         4       underflow range error
     TLOSS             5       total loss of precision
     PLOSS             6       partial loss of precision
     INVALID           7       NaN - producing operation
     EDOM             33       Unix domain error code
     ERANGE           34       Unix range error code

   The order of appearance of the following messages is bound to the
   error codes defined above.  */

int merror = 0;
extern int merror;

static void
mtherr (name, code)
     const char *name;
     int code;
{
  /* The string passed by the calling program is supposed to be the
     name of the function in which the error occurred.
     The code argument selects which error message string will be printed.  */

  if (strcmp (name, "esub") == 0)
    name = "subtraction";
  else if (strcmp (name, "ediv") == 0)
    name = "division";
  else if (strcmp (name, "emul") == 0)
    name = "multiplication";
  else if (strcmp (name, "enormlz") == 0)
    name = "normalization";
  else if (strcmp (name, "etoasc") == 0)
    name = "conversion to text";
  else if (strcmp (name, "asctoe") == 0)
    name = "parsing";
  else if (strcmp (name, "eremain") == 0)
    name = "modulus";
  else if (strcmp (name, "esqrt") == 0)
    name = "square root";
  if (extra_warnings)
    {
      switch (code)
	{
	case DOMAIN:    warning ("%s: argument domain error"    , name); break;
	case SING:      warning ("%s: function singularity"     , name); break;
	case OVERFLOW:  warning ("%s: overflow range error"     , name); break;
	case UNDERFLOW: warning ("%s: underflow range error"    , name); break;
	case TLOSS:     warning ("%s: total loss of precision"  , name); break;
	case PLOSS:     warning ("%s: partial loss of precision", name); break;
	case INVALID:   warning ("%s: NaN - producing operation", name); break;
	default:        abort ();
	}
    }

  /* Set global error message word */
  merror = code + 1;
}

#ifdef DEC
/* Convert DEC double precision D to e type E.  */

static void
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

/* Convert e type X to DEC double precision D.  */

static void
etodec (x, d)
     unsigned EMUSHORT *x, *d;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

  emovi (x, xi);
  /* Adjust exponent for offsets.  */
  exp = (EMULONG) xi[E] - (EXONE - 0201);
  /* Round off to nearest or even.  */
  rndsav = rndprc;
  rndprc = 56;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
  todec (xi, d);
}

/* Convert exploded e-type X, that has already been rounded to
   56-bit precision, to DEC format double Y.  */

static void
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
#endif /* DEC */

#ifdef IBM
/* Convert IBM single/double precision to e type.  */

static void
ibmtoe (d, e, mode)
     unsigned EMUSHORT *d;
     unsigned EMUSHORT *e;
     enum machine_mode mode;
{
  unsigned EMUSHORT y[NI];
  register unsigned EMUSHORT r, *p;
  int rndsav;

  ecleaz (y);			/* start with a zero */
  p = y;			/* point to our number */
  r = *d;			/* get IBM exponent word */
  if (*d & (unsigned int) 0x8000)
    *p = 0xffff;		/* fill in our sign */
  ++p;				/* bump pointer to our exponent word */
  r &= 0x7f00;			/* strip the sign bit */
  r >>= 6;			/* shift exponent word down 6 bits */
				/* in fact shift by 8 right and 2 left */
  r += EXONE - (0x41 << 2);	/* subtract IBM exponent offset */
  				/* add our e type exponent offset */
  *p++ = r;			/* to form our exponent */

  *p++ = *d++ & 0xff;		/* now do the high order mantissa */
				/* strip off the IBM exponent and sign bits */
  if (mode != SFmode)		/* there are only 2 words in SFmode */
    {
      *p++ = *d++;		/* fill in the rest of our mantissa */
      *p++ = *d++;
    }
  *p = *d;

  if (y[M] == 0 && y[M+1] == 0 && y[M+2] == 0 && y[M+3] == 0)
    y[0] = y[E] = 0;
  else
    y[E] -= 5 + enormlz (y);	/* now normalise the mantissa */
			      /* handle change in RADIX */
  emovo (y, e);
}



/* Convert e type to IBM single/double precision.  */

static void
etoibm (x, d, mode)
     unsigned EMUSHORT *x, *d;
     enum machine_mode mode;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

  emovi (x, xi);
  exp = (EMULONG) xi[E] - (EXONE - (0x41 << 2));	/* adjust exponent for offsets */
							/* round off to nearest or even */
  rndsav = rndprc;
  rndprc = 56;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
  toibm (xi, d, mode);
}

static void
toibm (x, y, mode)
     unsigned EMUSHORT *x, *y;
     enum machine_mode mode;
{
  unsigned EMUSHORT i;
  unsigned EMUSHORT *p;
  int r;

  p = x;
  *y = 0;
  if (*p++)
    *y = 0x8000;
  i = *p++;
  if (i == 0)
    {
      *y++ = 0;
      *y++ = 0;
      if (mode != SFmode)
	{
	  *y++ = 0;
	  *y++ = 0;
	}
      return;
    }
  r = i & 0x3;
  i >>= 2;
  if (i > 0x7f)
    {
      *y++ |= 0x7fff;
      *y++ = 0xffff;
      if (mode != SFmode)
	{
	  *y++ = 0xffff;
	  *y++ = 0xffff;
	}
#ifdef ERANGE
      errno = ERANGE;
#endif
      return;
    }
  i &= 0x7f;
  *y |= (i << 8);
  eshift (x, r + 5);
  *y++ |= x[M];
  *y++ = x[M + 1];
  if (mode != SFmode)
    {
      *y++ = x[M + 2];
      *y++ = x[M + 3];
    }
}
#endif /* IBM */


#ifdef C4X
/* Convert C4X single/double precision to e type.  */

static void
c4xtoe (d, e, mode)
     unsigned EMUSHORT *d;
     unsigned EMUSHORT *e;
     enum machine_mode mode;
{
  unsigned EMUSHORT y[NI];
  int r;
  int isnegative;
  int size;
  int i;
  int carry;

  /* Short-circuit the zero case. */
  if ((d[0] == 0x8000)
      && (d[1] == 0x0000)
      && ((mode == QFmode) || ((d[2] == 0x0000) && (d[3] == 0x0000))))
    {
      e[0] = 0;
      e[1] = 0;
      e[2] = 0;
      e[3] = 0;
      e[4] = 0;
      e[5] = 0;
      return;
    }

  ecleaz (y);			/* start with a zero */
  r = d[0];			/* get sign/exponent part */
  if (r & (unsigned int) 0x0080)
  {
     y[0] = 0xffff;		/* fill in our sign */
     isnegative = TRUE;
  }
  else
  {
     isnegative = FALSE;
  }

  r >>= 8;			/* Shift exponent word down 8 bits.  */
  if (r & 0x80)			/* Make the exponent negative if it is. */
  {
     r = r | (~0 & ~0xff);
  }

  if (isnegative)
  {
     /* Now do the high order mantissa.  We don't "or" on the high bit
	because it is 2 (not 1) and is handled a little differently
	below.  */
     y[M] = d[0] & 0x7f;

     y[M+1] = d[1];
     if (mode != QFmode)	/* There are only 2 words in QFmode.  */
     {
	y[M+2] = d[2];		/* Fill in the rest of our mantissa.  */
	y[M+3] = d[3];
	size = 4;
     }
     else
     {
	size = 2;
     }
     eshift(y, -8);

     /* Now do the two's complement on the data.  */

     carry = 1;	/* Initially add 1 for the two's complement. */
     for (i=size + M; i > M; i--)
     {
	if (carry && (y[i] == 0x0000))
	{
	   /* We overflowed into the next word, carry is the same.  */
	   y[i] = carry ? 0x0000 : 0xffff;
	}
	else
	{
	   /* No overflow, just invert and add carry.  */
	   y[i] = ((~y[i]) + carry) & 0xffff;
	   carry = 0;
	}
     }

     if (carry)
     {
	eshift(y, -1);
	y[M+1] |= 0x8000;
	r++;
     }
     y[1] = r + EXONE;
  }
  else
  {
    /* Add our e type exponent offset to form our exponent.  */
     r += EXONE;
     y[1] = r;

     /* Now do the high order mantissa strip off the exponent and sign
	bits and add the high 1 bit.  */
     y[M] = (d[0] & 0x7f) | 0x80;

     y[M+1] = d[1];
     if (mode != QFmode)	/* There are only 2 words in QFmode.  */
     {
	y[M+2] = d[2];		/* Fill in the rest of our mantissa.  */
	y[M+3] = d[3];
     }
     eshift(y, -8);
  }

  emovo (y, e);
}


/* Convert e type to C4X single/double precision.  */

static void
etoc4x (x, d, mode)
     unsigned EMUSHORT *x, *d;
     enum machine_mode mode;
{
  unsigned EMUSHORT xi[NI];
  EMULONG exp;
  int rndsav;

  emovi (x, xi);

  /* Adjust exponent for offsets. */
  exp = (EMULONG) xi[E] - (EXONE - 0x7f);

  /* Round off to nearest or even. */
  rndsav = rndprc;
  rndprc = mode == QFmode ? 24 : 32;
  emdnorm (xi, 0, 0, exp, 64);
  rndprc = rndsav;
  toc4x (xi, d, mode);
}

static void
toc4x (x, y, mode)
     unsigned EMUSHORT *x, *y;
     enum machine_mode mode;
{
  int i;
  int v;
  int carry;

  /* Short-circuit the zero case */
  if ((x[0] == 0)	/* Zero exponent and sign */
      && (x[1] == 0)
      && (x[M] == 0)	/* The rest is for zero mantissa */
      && (x[M+1] == 0)
      /* Only check for double if necessary */
      && ((mode == QFmode) || ((x[M+2] == 0) && (x[M+3] == 0))))
    {
      /* We have a zero.  Put it into the output and return. */
      *y++ = 0x8000;
      *y++ = 0x0000;
      if (mode != QFmode)
        {
          *y++ = 0x0000;
          *y++ = 0x0000;
        }
      return;
    }

  *y = 0;

  /* Negative number require a two's complement conversion of the
     mantissa. */
  if (x[0])
    {
      *y = 0x0080;

      i = ((int) x[1]) - 0x7f;

      /* Now add 1 to the inverted data to do the two's complement. */
      if (mode != QFmode)
	v = 4 + M;
      else
	v = 2 + M;
      carry = 1;
      while (v > M)
	{
	  if (x[v] == 0x0000)
	    {
	      x[v] = carry ? 0x0000 : 0xffff;
	    }
	  else
	    {
	      x[v] = ((~x[v]) + carry) & 0xffff;
	      carry = 0;
	    }
	  v--;
	}

      /* The following is a special case.  The C4X negative float requires
	 a zero in the high bit (because the format is (2 - x) x 2^m), so
	 if a one is in that bit, we have to shift left one to get rid
	 of it.  This only occurs if the number is -1 x 2^m. */
      if (x[M+1] & 0x8000)
	{
	  /* This is the case of -1 x 2^m, we have to rid ourselves of the
	     high sign bit and shift the exponent. */
	  eshift(x, 1);
	  i--;
	}
    }
  else
    {
      i = ((int) x[1]) - 0x7f;
    }

  if ((i < -128) || (i > 127))
    {
      y[0] |= 0xff7f;
      y[1] = 0xffff;
      if (mode != QFmode)
	{
	  y[2] = 0xffff;
	  y[3] = 0xffff;
	}
#ifdef ERANGE
      errno = ERANGE;
#endif
      return;
    }

  y[0] |= ((i & 0xff) << 8);

  eshift (x, 8);

  y[0] |= x[M] & 0x7f;
  y[1] = x[M + 1];
  if (mode != QFmode)
    {
      y[2] = x[M + 2];
      y[3] = x[M + 3];
    }
}
#endif /* C4X */

/* Output a binary NaN bit pattern in the target machine's format.  */

/* If special NaN bit patterns are required, define them in tm.h
   as arrays of unsigned 16-bit shorts.  Otherwise, use the default
   patterns here.  */
#ifdef TFMODE_NAN
TFMODE_NAN;
#else
#ifdef IEEE
unsigned EMUSHORT TFbignan[8] =
 {0x7fff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff};
unsigned EMUSHORT TFlittlenan[8] = {0, 0, 0, 0, 0, 0, 0x8000, 0xffff};
#endif
#endif

#ifdef XFMODE_NAN
XFMODE_NAN;
#else
#ifdef IEEE
unsigned EMUSHORT XFbignan[6] =
 {0x7fff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff};
unsigned EMUSHORT XFlittlenan[6] = {0, 0, 0, 0xc000, 0xffff, 0};
#endif
#endif

#ifdef DFMODE_NAN
DFMODE_NAN;
#else
#ifdef IEEE
unsigned EMUSHORT DFbignan[4] = {0x7fff, 0xffff, 0xffff, 0xffff};
unsigned EMUSHORT DFlittlenan[4] = {0, 0, 0, 0xfff8};
#endif
#endif

#ifdef SFMODE_NAN
SFMODE_NAN;
#else
#ifdef IEEE
unsigned EMUSHORT SFbignan[2] = {0x7fff, 0xffff};
unsigned EMUSHORT SFlittlenan[2] = {0, 0xffc0};
#endif
#endif


static void
make_nan (nan, sign, mode)
     unsigned EMUSHORT *nan;
     int sign;
     enum machine_mode mode;
{
  int n;
  unsigned EMUSHORT *p;

  switch (mode)
    {
/* Possibly the `reserved operand' patterns on a VAX can be
   used like NaN's, but probably not in the same way as IEEE.  */
#if !defined(DEC) && !defined(IBM) && !defined(C4X)
    case TFmode:
      n = 8;
      if (REAL_WORDS_BIG_ENDIAN)
	p = TFbignan;
      else
	p = TFlittlenan;
      break;

    case XFmode:
      n = 6;
      if (REAL_WORDS_BIG_ENDIAN)
	p = XFbignan;
      else
	p = XFlittlenan;
      break;

    case DFmode:
      n = 4;
      if (REAL_WORDS_BIG_ENDIAN)
	p = DFbignan;
      else
	p = DFlittlenan;
      break;

    case SFmode:
    case HFmode:
      n = 2;
      if (REAL_WORDS_BIG_ENDIAN)
	p = SFbignan;
      else
	p = SFlittlenan;
      break;
#endif

    default:
      abort ();
    }
  if (REAL_WORDS_BIG_ENDIAN)
    *nan++ = (sign << 15) | (*p++ & 0x7fff);
  while (--n != 0)
    *nan++ = *p++;
  if (! REAL_WORDS_BIG_ENDIAN)
    *nan = (sign << 15) | (*p & 0x7fff);
}

/* This is the inverse of the function `etarsingle' invoked by
   REAL_VALUE_TO_TARGET_SINGLE.  */

REAL_VALUE_TYPE
ereal_unto_float (f)
     long f;
{
  REAL_VALUE_TYPE r;
  unsigned EMUSHORT s[2];
  unsigned EMUSHORT e[NE];

  /* Convert 32 bit integer to array of 16 bit pieces in target machine order.
   This is the inverse operation to what the function `endian' does.  */
  if (REAL_WORDS_BIG_ENDIAN)
    {
      s[0] = (unsigned EMUSHORT) (f >> 16);
      s[1] = (unsigned EMUSHORT) f;
    }
  else
    {
      s[0] = (unsigned EMUSHORT) f;
      s[1] = (unsigned EMUSHORT) (f >> 16);
    }
  /* Convert and promote the target float to E-type. */
  e24toe (s, e);
  /* Output E-type to REAL_VALUE_TYPE. */
  PUT_REAL (e, &r);
  return r;
}


/* This is the inverse of the function `etardouble' invoked by
   REAL_VALUE_TO_TARGET_DOUBLE.  */

REAL_VALUE_TYPE
ereal_unto_double (d)
     long d[];
{
  REAL_VALUE_TYPE r;
  unsigned EMUSHORT s[4];
  unsigned EMUSHORT e[NE];

  /* Convert array of HOST_WIDE_INT to equivalent array of 16-bit pieces.  */
  if (REAL_WORDS_BIG_ENDIAN)
    {
      s[0] = (unsigned EMUSHORT) (d[0] >> 16);
      s[1] = (unsigned EMUSHORT) d[0];
      s[2] = (unsigned EMUSHORT) (d[1] >> 16);
      s[3] = (unsigned EMUSHORT) d[1];
    }
  else
    {
      /* Target float words are little-endian.  */
      s[0] = (unsigned EMUSHORT) d[0];
      s[1] = (unsigned EMUSHORT) (d[0] >> 16);
      s[2] = (unsigned EMUSHORT) d[1];
      s[3] = (unsigned EMUSHORT) (d[1] >> 16);
    }
  /* Convert target double to E-type. */
  e53toe (s, e);
  /* Output E-type to REAL_VALUE_TYPE. */
  PUT_REAL (e, &r);
  return r;
}


/* Convert an SFmode target `float' value to a REAL_VALUE_TYPE.
   This is somewhat like ereal_unto_float, but the input types
   for these are different.  */

REAL_VALUE_TYPE
ereal_from_float (f)
     HOST_WIDE_INT f;
{
  REAL_VALUE_TYPE r;
  unsigned EMUSHORT s[2];
  unsigned EMUSHORT e[NE];

  /* Convert 32 bit integer to array of 16 bit pieces in target machine order.
   This is the inverse operation to what the function `endian' does.  */
  if (REAL_WORDS_BIG_ENDIAN)
    {
      s[0] = (unsigned EMUSHORT) (f >> 16);
      s[1] = (unsigned EMUSHORT) f;
    }
  else
    {
      s[0] = (unsigned EMUSHORT) f;
      s[1] = (unsigned EMUSHORT) (f >> 16);
    }
  /* Convert and promote the target float to E-type.  */
  e24toe (s, e);
  /* Output E-type to REAL_VALUE_TYPE.  */
  PUT_REAL (e, &r);
  return r;
}


/* Convert a DFmode target `double' value to a REAL_VALUE_TYPE.
   This is somewhat like ereal_unto_double, but the input types
   for these are different.

   The DFmode is stored as an array of HOST_WIDE_INT in the target's
   data format, with no holes in the bit packing.  The first element
   of the input array holds the bits that would come first in the
   target computer's memory.  */

REAL_VALUE_TYPE
ereal_from_double (d)
     HOST_WIDE_INT d[];
{
  REAL_VALUE_TYPE r;
  unsigned EMUSHORT s[4];
  unsigned EMUSHORT e[NE];

  /* Convert array of HOST_WIDE_INT to equivalent array of 16-bit pieces.  */
  if (REAL_WORDS_BIG_ENDIAN)
    {
#if HOST_BITS_PER_WIDE_INT == 32
      s[0] = (unsigned EMUSHORT) (d[0] >> 16);
      s[1] = (unsigned EMUSHORT) d[0];
      s[2] = (unsigned EMUSHORT) (d[1] >> 16);
      s[3] = (unsigned EMUSHORT) d[1];
#else
      /* In this case the entire target double is contained in the
	 first array element.  The second element of the input is
	 ignored.  */
      s[0] = (unsigned EMUSHORT) (d[0] >> 48);
      s[1] = (unsigned EMUSHORT) (d[0] >> 32);
      s[2] = (unsigned EMUSHORT) (d[0] >> 16);
      s[3] = (unsigned EMUSHORT) d[0];
#endif
    }
  else
    {
      /* Target float words are little-endian.  */
      s[0] = (unsigned EMUSHORT) d[0];
      s[1] = (unsigned EMUSHORT) (d[0] >> 16);
#if HOST_BITS_PER_WIDE_INT == 32
      s[2] = (unsigned EMUSHORT) d[1];
      s[3] = (unsigned EMUSHORT) (d[1] >> 16);
#else
      s[2] = (unsigned EMUSHORT) (d[0] >> 32);
      s[3] = (unsigned EMUSHORT) (d[0] >> 48);
#endif
    }
  /* Convert target double to E-type.  */
  e53toe (s, e);
  /* Output E-type to REAL_VALUE_TYPE.  */
  PUT_REAL (e, &r);
  return r;
}


#if 0
/* Convert target computer unsigned 64-bit integer to e-type.
   The endian-ness of DImode follows the convention for integers,
   so we use WORDS_BIG_ENDIAN here, not REAL_WORDS_BIG_ENDIAN.  */

static void
uditoe (di, e)
     unsigned EMUSHORT *di;  /* Address of the 64-bit int.  */
     unsigned EMUSHORT *e;
{
  unsigned EMUSHORT yi[NI];
  int k;

  ecleaz (yi);
  if (WORDS_BIG_ENDIAN)
    {
      for (k = M; k < M + 4; k++)
	yi[k] = *di++;
    }
  else
    {
      for (k = M + 3; k >= M; k--)
	yi[k] = *di++;
    }
  yi[E] = EXONE + 47;	/* exponent if normalize shift count were 0 */
  if ((k = enormlz (yi)) > NBITS)/* normalize the significand */
    ecleaz (yi);		/* it was zero */
  else
    yi[E] -= (unsigned EMUSHORT) k;/* subtract shift count from exponent */
  emovo (yi, e);
}

/* Convert target computer signed 64-bit integer to e-type.  */

static void
ditoe (di, e)
     unsigned EMUSHORT *di;  /* Address of the 64-bit int.  */
     unsigned EMUSHORT *e;
{
  unsigned EMULONG acc;
  unsigned EMUSHORT yi[NI];
  unsigned EMUSHORT carry;
  int k, sign;

  ecleaz (yi);
  if (WORDS_BIG_ENDIAN)
    {
      for (k = M; k < M + 4; k++)
	yi[k] = *di++;
    }
  else
    {
      for (k = M + 3; k >= M; k--)
	yi[k] = *di++;
    }
  /* Take absolute value */
  sign = 0;
  if (yi[M] & 0x8000)
    {
      sign = 1;
      carry = 0;
      for (k = M + 3; k >= M; k--)
	{
	  acc = (unsigned EMULONG) (~yi[k] & 0xffff) + carry;
	  yi[k] = acc;
	  carry = 0;
	  if (acc & 0x10000)
	    carry = 1;
	}
    }
  yi[E] = EXONE + 47;	/* exponent if normalize shift count were 0 */
  if ((k = enormlz (yi)) > NBITS)/* normalize the significand */
    ecleaz (yi);		/* it was zero */
  else
    yi[E] -= (unsigned EMUSHORT) k;/* subtract shift count from exponent */
  emovo (yi, e);
  if (sign)
	eneg (e);
}


/* Convert e-type to unsigned 64-bit int.  */

static void
etoudi (x, i)
     unsigned EMUSHORT *x;
     unsigned EMUSHORT *i;
{
  unsigned EMUSHORT xi[NI];
  int j, k;

  emovi (x, xi);
  if (xi[0])
    {
      xi[M] = 0;
      goto noshift;
    }
  k = (int) xi[E] - (EXONE - 1);
  if (k <= 0)
    {
      for (j = 0; j < 4; j++)
	*i++ = 0;
      return;
    }
  if (k > 64)
    {
      for (j = 0; j < 4; j++)
	*i++ = 0xffff;
      if (extra_warnings)
	warning ("overflow on truncation to integer");
      return;
    }
  if (k > 16)
    {
      /* Shift more than 16 bits: first shift up k-16 mod 16,
	 then shift up by 16's.  */
      j = k - ((k >> 4) << 4);
      if (j == 0)
	j = 16;
      eshift (xi, j);
      if (WORDS_BIG_ENDIAN)
	*i++ = xi[M];
      else
	{
	  i += 3;
	  *i-- = xi[M];
	}
      k -= j;
      do
	{
	  eshup6 (xi);
	  if (WORDS_BIG_ENDIAN)
	    *i++ = xi[M];
	  else
	    *i-- = xi[M];
	}
      while ((k -= 16) > 0);
    }
  else
    {
        /* shift not more than 16 bits */
      eshift (xi, k);

noshift:

      if (WORDS_BIG_ENDIAN)
	{
	  i += 3;
	  *i-- = xi[M];
	  *i-- = 0;
	  *i-- = 0;
	  *i = 0;
	}
      else
	{
	  *i++ = xi[M];
	  *i++ = 0;
	  *i++ = 0;
	  *i = 0;
	}
    }
}


/* Convert e-type to signed 64-bit int.  */

static void
etodi (x, i)
     unsigned EMUSHORT *x;
     unsigned EMUSHORT *i;
{
  unsigned EMULONG acc;
  unsigned EMUSHORT xi[NI];
  unsigned EMUSHORT carry;
  unsigned EMUSHORT *isave;
  int j, k;

  emovi (x, xi);
  k = (int) xi[E] - (EXONE - 1);
  if (k <= 0)
    {
      for (j = 0; j < 4; j++)
	*i++ = 0;
      return;
    }
  if (k > 64)
    {
      for (j = 0; j < 4; j++)
	*i++ = 0xffff;
      if (extra_warnings)
	warning ("overflow on truncation to integer");
      return;
    }
  isave = i;
  if (k > 16)
    {
      /* Shift more than 16 bits: first shift up k-16 mod 16,
	 then shift up by 16's.  */
      j = k - ((k >> 4) << 4);
      if (j == 0)
	j = 16;
      eshift (xi, j);
      if (WORDS_BIG_ENDIAN)
	*i++ = xi[M];
      else
	{
	  i += 3;
	  *i-- = xi[M];
	}
      k -= j;
      do
	{
	  eshup6 (xi);
	  if (WORDS_BIG_ENDIAN)
	    *i++ = xi[M];
	  else
	    *i-- = xi[M];
	}
      while ((k -= 16) > 0);
    }
  else
    {
        /* shift not more than 16 bits */
      eshift (xi, k);

      if (WORDS_BIG_ENDIAN)
	{
	  i += 3;
	  *i = xi[M];
	  *i-- = 0;
	  *i-- = 0;
	  *i = 0;
	}
      else
	{
	  *i++ = xi[M];
	  *i++ = 0;
	  *i++ = 0;
	  *i = 0;
	}
    }
  /* Negate if negative */
  if (xi[0])
    {
      carry = 0;
      if (WORDS_BIG_ENDIAN)
	isave += 3;
      for (k = 0; k < 4; k++)
	{
	  acc = (unsigned EMULONG) (~(*isave) & 0xffff) + carry;
	  if (WORDS_BIG_ENDIAN)
	    *isave-- = acc;
	  else
	    *isave++ = acc;
	  carry = 0;
	  if (acc & 0x10000)
	    carry = 1;
	}
    }
}


/* Longhand square root routine.  */


static int esqinited = 0;
static unsigned short sqrndbit[NI];

static void
esqrt (x, y)
     unsigned EMUSHORT *x, *y;
{
  unsigned EMUSHORT temp[NI], num[NI], sq[NI], xx[NI];
  EMULONG m, exp;
  int i, j, k, n, nlups;

  if (esqinited == 0)
    {
      ecleaz (sqrndbit);
      sqrndbit[NI - 2] = 1;
      esqinited = 1;
    }
  /* Check for arg <= 0 */
  i = ecmp (x, ezero);
  if (i <= 0)
    {
      if (i == -1)
	{
	  mtherr ("esqrt", DOMAIN);
	  eclear (y);
	}
      else
	emov (x, y);
      return;
    }

#ifdef INFINITY
  if (eisinf (x))
    {
      eclear (y);
      einfin (y);
      return;
    }
#endif
  /* Bring in the arg and renormalize if it is denormal.  */
  emovi (x, xx);
  m = (EMULONG) xx[1];		/* local long word exponent */
  if (m == 0)
    m -= enormlz (xx);

  /* Divide exponent by 2 */
  m -= 0x3ffe;
  exp = (unsigned short) ((m / 2) + 0x3ffe);

  /* Adjust if exponent odd */
  if ((m & 1) != 0)
    {
      if (m > 0)
	exp += 1;
      eshdn1 (xx);
    }

  ecleaz (sq);
  ecleaz (num);
  n = 8;			/* get 8 bits of result per inner loop */
  nlups = rndprc;
  j = 0;

  while (nlups > 0)
    {
      /* bring in next word of arg */
      if (j < NE)
	num[NI - 1] = xx[j + 3];
      /* Do additional bit on last outer loop, for roundoff.  */
      if (nlups <= 8)
	n = nlups + 1;
      for (i = 0; i < n; i++)
	{
	  /* Next 2 bits of arg */
	  eshup1 (num);
	  eshup1 (num);
	  /* Shift up answer */
	  eshup1 (sq);
	  /* Make trial divisor */
	  for (k = 0; k < NI; k++)
	    temp[k] = sq[k];
	  eshup1 (temp);
	  eaddm (sqrndbit, temp);
	  /* Subtract and insert answer bit if it goes in */
	  if (ecmpm (temp, num) <= 0)
	    {
	      esubm (temp, num);
	      sq[NI - 2] |= 1;
	    }
	}
      nlups -= n;
      j += 1;
    }

  /* Adjust for extra, roundoff loop done.  */
  exp += (NBITS - 1) - rndprc;

  /* Sticky bit = 1 if the remainder is nonzero.  */
  k = 0;
  for (i = 3; i < NI; i++)
    k |= (int) num[i];

  /* Renormalize and round off.  */
  emdnorm (sq, k, 0, exp, 64);
  emovo (sq, y);
}
#endif
#endif /* EMU_NON_COMPILE not defined */

/* Return the binary precision of the significand for a given
   floating point mode.  The mode can hold an integer value
   that many bits wide, without losing any bits.  */

int
significand_size (mode)
     enum machine_mode mode;
{

/* Don't test the modes, but their sizes, lest this
   code won't work for BITS_PER_UNIT != 8 .  */

switch (GET_MODE_BITSIZE (mode))
  {
  case 32:

#if TARGET_FLOAT_FORMAT == C4X_FLOAT_FORMAT
    return 56;
#endif

    return 24;

  case 64:
#if TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
    return 53;
#else
#if TARGET_FLOAT_FORMAT == IBM_FLOAT_FORMAT
    return 56;
#else
#if TARGET_FLOAT_FORMAT == VAX_FLOAT_FORMAT
    return 56;
#else
#if TARGET_FLOAT_FORMAT == C4X_FLOAT_FORMAT
    return 56;
#else
    abort ();
#endif
#endif
#endif
#endif

  case 96:
    return 64;
  case 128:
    return 113;

  default:
    abort ();
  }
}
