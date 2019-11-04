/* Test for <tgmath.h> in C99. */
/* Origin: Matt Austern <austern@apple.com>
/* { dg-do preprocess { target c99_runtime } } */
/* { dg-options "-std=iso9899:1999" } */
/* { dg-require-effective-target tgmath_h } */

/* Test that tgmath defines the macros it's supposed to. */
#include <tgmath.h>

#ifndef acos
#error acos undefined
#endif

#ifndef asin
#error asin undefined
#endif

#ifndef atan
#error atan undefined
#endif

#ifndef acosh
#error acosh undefined
#endif

#ifndef asinh
#error asinh undefined
#endif

#ifndef atanh
#error atanh undefined
#endif

#ifndef cos
#error cos undefined
#endif

#ifndef sin
#error sin undefined
#endif

#ifndef tan
#error tan undefined
#endif

#ifndef cosh
#error cosh undefined
#endif

#ifndef sinh
#error sinh undefined
#endif

#ifndef tanh
#error tanh undefined
#endif

#ifndef exp
#error exp undefined
#endif

#ifndef log
#error log undefined
#endif

#ifndef pow
#error pow undefined
#endif

#ifndef sqrt
#error sqrt undefined
#endif

#ifndef fabs
#error fabs undefined
#endif

#ifndef atan2
#error atan2 undefined
#endif

#ifndef cbrt
#error cbrt undefined
#endif

#ifndef ceil
#error ceil undefined
#endif

#ifndef copysign
#error copysign undefined
#endif

#ifndef erf
#error erf undefined
#endif

#ifndef erfc
#error erfc undefined
#endif

#ifndef exp2
#error exp2 undefined
#endif

#ifndef expm1
#error expm1 undefined
#endif

#ifndef fdim
#error fdim undefined
#endif

#ifndef floor
#error floor undefined
#endif

#ifndef fma
#error fma undefined
#endif

#ifndef fmax
#error fmax undefined
#endif

#ifndef fmin
#error fmin undefined
#endif

#ifndef fmod
#error fmod undefined
#endif

#ifndef frexp
#error frexp undefined
#endif

#ifndef hypot
#error hypot undefined
#endif

#ifndef ilogb
#error ilogb undefined
#endif

#ifndef ldexp
#error ldexp undefined
#endif

#ifndef lgamma
#error lgamma undefined
#endif

#ifndef llrint
#error llrint undefined
#endif

#ifndef llround
#error llround undefined
#endif

#ifndef log10
#error log10 undefined
#endif

#ifndef log1p
#error log1p undefined
#endif

#ifndef log2
#error log2 undefined
#endif

#ifndef logb
#error logb undefined
#endif

#ifndef lrint
#error lrint undefined
#endif

#ifndef lround
#error lround undefined
#endif

#ifndef nearbyint
#error nearbyint undefined
#endif

#ifndef nextafter
#error nextafter undefined
#endif

#ifndef nexttoward
#error nexttoward undefined
#endif

#ifndef remainder
#error remainder undefined
#endif

#ifndef remquo
#error remquo undefined
#endif

#ifndef rint
#error rint undefined
#endif

#ifndef round
#error round undefined
#endif

#ifndef scalbn
#error scalbn undefined
#endif

#ifndef scalbln
#error scalbln undefined
#endif

#ifndef tgamma
#error tgamma undefined
#endif

#ifndef trunc
#error trunc undefined
#endif

#ifndef carg
#error carg undefined
#endif

#ifndef cimag
#error cimag undefined
#endif

#ifndef conj
#error conj undefined
#endif

#ifndef cproj
#error cproj undefined
#endif

#ifndef creal
#error creal undefined
#endif
