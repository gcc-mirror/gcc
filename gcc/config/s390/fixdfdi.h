/* Definitions of target machine for GNU compiler, for IBM S/390
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).

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

#ifdef L_fixunsdfdi
#define EXPD(fp)	(((fp.l.upper) >> 20) & 0x7FF)
#define EXCESSD		1022
#define SIGNBIT		0x80000000
#define SIGND(fp)	((fp.l.upper) & SIGNBIT)
#define MANTD_LL(fp)	((fp.ll & (HIDDEND_LL-1)) | HIDDEND_LL)
#define FRACD_LL(fp)	(fp.ll & (HIDDEND_LL-1))
#define HIDDEND_LL	((UDItype_x)1 << 52)

typedef int DItype_x __attribute__ ((mode (DI)));
typedef unsigned int UDItype_x __attribute__ ((mode (DI)));
typedef int SItype_x __attribute__ ((mode (SI)));
typedef unsigned int USItype_x __attribute__ ((mode (SI)));

union double_long {
    double d;
    struct {
      SItype_x upper;
      USItype_x lower;
    } l;
    UDItype_x ll;
};


/* convert double to unsigned int */
UDItype_x
__fixunsdfdi (double a1)
{
    register union double_long dl1;
    register int exp;
    register UDItype_x l;

    dl1.d = a1;

    /* +/- 0, denormalized, negativ */

    if (!EXPD (dl1) || SIGND(dl1))
      return 0;

    exp = EXPD (dl1) - EXCESSD - 53;

    /* number < 1 */

    if (exp < -53)
      return 0;

    /* NaN */

    if ((EXPD(dl1) == 0x7ff) && (FRACD_LL(dl1) != 0)) /* NaN */
      return 0x0ULL;

    /* Number big number & + inf */

    if (exp >= 12) {
      return 0xFFFFFFFFFFFFFFFFULL;
    }

    l = MANTD_LL(dl1);

    /* shift down until exp < 12 or l = 0 */
    if (exp > 0)
      l <<= exp;
    else 
      l >>= -exp;

    return l;
}
#define __fixunsdfdi ___fixunsdfdi
#endif
#undef L_fixunsdfdi

#ifdef L_fixdfdi
#define EXPD(fp)	(((fp.l.upper) >> 20) & 0x7FF)
#define EXCESSD		1022
#define SIGNBIT		0x80000000
#define SIGND(fp)	((fp.l.upper) & SIGNBIT)
#define MANTD_LL(fp)	((fp.ll & (HIDDEND_LL-1)) | HIDDEND_LL)
#define FRACD_LL(fp)	(fp.ll & (HIDDEND_LL-1))
#define HIDDEND_LL	((UDItype_x)1 << 52)

typedef int DItype_x __attribute__ ((mode (DI)));
typedef unsigned int UDItype_x __attribute__ ((mode (DI)));
typedef int SItype_x __attribute__ ((mode (SI)));
typedef unsigned int USItype_x __attribute__ ((mode (SI)));

union double_long {
    double d;
    struct {
      SItype_x upper;
      USItype_x lower;
    } l;
    UDItype_x ll;
};

/* convert double to int */
DItype_x
__fixdfdi (double a1)
{
    register union double_long dl1;
    register int exp;
    register DItype_x l;

    dl1.d = a1;

    /* +/- 0, denormalized */

    if (!EXPD (dl1))
      return 0;

    exp = EXPD (dl1) - EXCESSD - 53;

    /* number < 1 */

    if (exp < -53)
      return 0;

    /* NaN */

    if ((EXPD(dl1) == 0x7ff) && (FRACD_LL(dl1) != 0)) /* NaN */
      return 0x8000000000000000ULL;

    /* Number big number & +/- inf */

    if (exp >= 11) {
	l = (long long)1<<63;
	if (!SIGND(dl1))
	    l--;
	return l;
    }

    l = MANTD_LL(dl1);

    /* shift down until exp < 12 or l = 0 */
    if (exp > 0)
      l <<= exp;
    else 
      l >>= -exp;

    return (SIGND (dl1) ? -l : l);
}
#define __fixdfdi ___fixdfdi
#endif
#undef L_fixdfdi

#ifdef L_fixunssfdi
#define EXP(fp)         (((fp.l) >> 23) & 0xFF)
#define EXCESS          126
#define SIGNBIT         0x80000000
#define SIGN(fp)        ((fp.l) & SIGNBIT)
#define HIDDEN          (1 << 23)
#define MANT(fp)        (((fp.l) & 0x7FFFFF) | HIDDEN)
#define FRAC(fp)        ((fp.l) & 0x7FFFFF)

typedef int DItype_x __attribute__ ((mode (DI)));
typedef unsigned int UDItype_x __attribute__ ((mode (DI)));
typedef int SItype_x __attribute__ ((mode (SI)));
typedef unsigned int USItype_x __attribute__ ((mode (SI)));

union float_long
  {
    float f;
    USItype_x l;
  };

/* convert float to unsigned int */
UDItype_x
__fixunssfdi (float a1)
{
    register union float_long fl1;
    register int exp;
    register UDItype_x l;

    fl1.f = a1;

    /* +/- 0, denormalized, negativ */

    if (!EXP (fl1) || SIGN(fl1))
      return 0;

    exp = EXP (fl1) - EXCESS - 24;

    /* number < 1 */

    if (exp < -24)
      return 0;

    /* NaN */

    if ((EXP(fl1) == 0xff) && (FRAC(fl1) != 0)) /* NaN */
      return 0x0ULL;

    /* Number big number & + inf */

    if (exp >= 41) {
      return 0xFFFFFFFFFFFFFFFFULL;
    }

    l = MANT(fl1);

    if (exp > 0)
      l <<= exp;
    else 
      l >>= -exp;

    return l;
}
#define __fixunssfdi ___fixunssfdi
#endif
#undef L_fixunssfdi

#ifdef L_fixsfdi
#define EXP(fp)         (((fp.l) >> 23) & 0xFF)
#define EXCESS          126
#define SIGNBIT         0x80000000
#define SIGN(fp)        ((fp.l) & SIGNBIT)
#define HIDDEN          (1 << 23)
#define MANT(fp)        (((fp.l) & 0x7FFFFF) | HIDDEN)
#define FRAC(fp)        ((fp.l) & 0x7FFFFF)

typedef int DItype_x __attribute__ ((mode (DI)));
typedef unsigned int UDItype_x __attribute__ ((mode (DI)));
typedef int SItype_x __attribute__ ((mode (SI)));
typedef unsigned int USItype_x __attribute__ ((mode (SI)));

union float_long
  {
    float f;
    USItype_x l;
  };

/* convert double to int */
DItype_x
__fixsfdi (float a1)
{
    register union float_long fl1;
    register int exp;
    register DItype_x l;

    fl1.f = a1;

    /* +/- 0, denormalized */

    if (!EXP (fl1))
      return 0;

    exp = EXP (fl1) - EXCESS - 24;

    /* number < 1 */

    if (exp < -24)
      return 0;

    /* NaN */

    if ((EXP(fl1) == 0xff) && (FRAC(fl1) != 0)) /* NaN */
      return 0x8000000000000000ULL;

    /* Number big number & +/- inf */

    if (exp >= 40) {
	l = (long long)1<<63;
	if (!SIGN(fl1))
	    l--;
	return l;
    }

    l = MANT(fl1);

    if (exp > 0)
      l <<= exp;
    else 
      l >>= -exp;

    return (SIGN (fl1) ? -l : l);
}
#define __fixsfdi ___fixsfdi
#endif
#undef L_fixsfdi

