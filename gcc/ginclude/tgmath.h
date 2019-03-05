/* Copyright (C) 2004-2019 Free Software Foundation, Inc.
   Contributed by Apple, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.22  Type-generic math <tgmath.h>
 */

#ifndef _TGMATH_H
#define _TGMATH_H

#include <math.h>

#ifndef __cplusplus
#include <complex.h>

/* Naming convention: generic macros are defining using
   __TGMATH_CPLX*, __TGMATH_REAL*, and __TGMATH_CPLX_ONLY.  _CPLX
   means the generic argument(s) may be real or complex, _REAL means
   real only, _CPLX means complex only.  If there is no suffix, we are
   defining a function of one argument.  If the suffix is _n
   it is a function of n arguments.  We only define these macros for
   values of n that are needed. */

#define __TGMATH_CPLX(z,R,C)				\
  __builtin_tgmath (R##f, R, R##l, C##f, C, C##l, (z))

#define __TGMATH_CPLX_2(z1,z2,R,C)				\
  __builtin_tgmath (R##f, R, R##l, C##f, C, C##l, (z1), (z2))

#define __TGMATH_REAL(x,R) \
  __builtin_tgmath (R##f, R, R##l, (x))
#define __TGMATH_REAL_2(x,y,R) \
  __builtin_tgmath (R##f, R, R##l, (x), (y))
#define __TGMATH_REAL_3(x,y,z,R) \
  __builtin_tgmath (R##f, R, R##l, (x), (y), (z))
#define __TGMATH_CPLX_ONLY(z,C) \
  __builtin_tgmath (C##f, C, C##l, (z))

/* Functions defined in both <math.h> and <complex.h> (7.22p4) */
#define acos(z)          __TGMATH_CPLX(z, acos, cacos)
#define asin(z)          __TGMATH_CPLX(z, asin, casin)
#define atan(z)          __TGMATH_CPLX(z, atan, catan)
#define acosh(z)         __TGMATH_CPLX(z, acosh, cacosh)
#define asinh(z)         __TGMATH_CPLX(z, asinh, casinh)
#define atanh(z)         __TGMATH_CPLX(z, atanh, catanh)
#define cos(z)           __TGMATH_CPLX(z, cos, ccos)
#define sin(z)           __TGMATH_CPLX(z, sin, csin)
#define tan(z)           __TGMATH_CPLX(z, tan, ctan)
#define cosh(z)          __TGMATH_CPLX(z, cosh, ccosh)
#define sinh(z)          __TGMATH_CPLX(z, sinh, csinh)
#define tanh(z)          __TGMATH_CPLX(z, tanh, ctanh)
#define exp(z)           __TGMATH_CPLX(z, exp, cexp)
#define log(z)           __TGMATH_CPLX(z, log, clog)
#define pow(z1,z2)       __TGMATH_CPLX_2(z1, z2, pow, cpow)
#define sqrt(z)          __TGMATH_CPLX(z, sqrt, csqrt)
#define fabs(z)          __TGMATH_CPLX(z, fabs, cabs)

/* Functions defined in <math.h> only (7.22p5) */
#define atan2(x,y)       __TGMATH_REAL_2(x, y, atan2)
#define cbrt(x)          __TGMATH_REAL(x, cbrt)
#define ceil(x)          __TGMATH_REAL(x, ceil)
#define copysign(x,y)    __TGMATH_REAL_2(x, y, copysign)
#define erf(x)           __TGMATH_REAL(x, erf)
#define erfc(x)          __TGMATH_REAL(x, erfc)
#define exp2(x)          __TGMATH_REAL(x, exp2)
#define expm1(x)         __TGMATH_REAL(x, expm1)
#define fdim(x,y)        __TGMATH_REAL_2(x, y, fdim)
#define floor(x)         __TGMATH_REAL(x, floor)
#define fma(x,y,z)       __TGMATH_REAL_3(x, y, z, fma)
#define fmax(x,y)        __TGMATH_REAL_2(x, y, fmax)
#define fmin(x,y)        __TGMATH_REAL_2(x, y, fmin)
#define fmod(x,y)        __TGMATH_REAL_2(x, y, fmod)
#define frexp(x,y)       __TGMATH_REAL_2(x, y, frexp)
#define hypot(x,y)       __TGMATH_REAL_2(x, y, hypot)
#define ilogb(x)         __TGMATH_REAL(x, ilogb)
#define ldexp(x,y)       __TGMATH_REAL_2(x, y, ldexp)
#define lgamma(x)        __TGMATH_REAL(x, lgamma)
#define llrint(x)        __TGMATH_REAL(x, llrint)
#define llround(x)       __TGMATH_REAL(x, llround)
#define log10(x)         __TGMATH_REAL(x, log10)
#define log1p(x)         __TGMATH_REAL(x, log1p)
#define log2(x)          __TGMATH_REAL(x, log2)
#define logb(x)          __TGMATH_REAL(x, logb)
#define lrint(x)         __TGMATH_REAL(x, lrint)
#define lround(x)        __TGMATH_REAL(x, lround)
#define nearbyint(x)     __TGMATH_REAL(x, nearbyint)
#define nextafter(x,y)   __TGMATH_REAL_2(x, y, nextafter)
#define nexttoward(x,y)  __TGMATH_REAL_2(x, y, nexttoward)
#define remainder(x,y)   __TGMATH_REAL_2(x, y, remainder)
#define remquo(x,y,z)    __TGMATH_REAL_3(x, y, z, remquo)
#define rint(x)          __TGMATH_REAL(x, rint)
#define round(x)         __TGMATH_REAL(x, round)
#define scalbn(x,y)      __TGMATH_REAL_2(x, y, scalbn)
#define scalbln(x,y)     __TGMATH_REAL_2(x, y, scalbln)
#define tgamma(x)        __TGMATH_REAL(x, tgamma)
#define trunc(x)         __TGMATH_REAL(x, trunc)

/* Functions defined in <complex.h> only (7.22p6) */
#define carg(z)          __TGMATH_CPLX_ONLY(z, carg)
#define cimag(z)         __TGMATH_CPLX_ONLY(z, cimag)
#define conj(z)          __TGMATH_CPLX_ONLY(z, conj)
#define cproj(z)         __TGMATH_CPLX_ONLY(z, cproj)
#define creal(z)         __TGMATH_CPLX_ONLY(z, creal)

#endif /* __cplusplus */
#endif /* _TGMATH_H */
