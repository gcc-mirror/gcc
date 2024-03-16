/* Test *_IS_IEC_60559 macros.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <float.h>

#ifndef FLT_IS_IEC_60559
#error "FLT_IS_IEC_60559 undefined"
#endif

#ifndef DBL_IS_IEC_60559
#error "DBL_IS_IEC_60559 undefined"
#endif

#ifndef LDBL_IS_IEC_60559
#error "LDBL_IS_IEC_60559 undefined"
#endif

#if defined __pdp11__ || defined __vax__
_Static_assert (FLT_IS_IEC_60559 == 0);
_Static_assert (DBL_IS_IEC_60559 == 0);
_Static_assert (LDBL_IS_IEC_60559 == 0);
#else
_Static_assert (FLT_IS_IEC_60559 == 1);
_Static_assert (DBL_IS_IEC_60559 == 1);
#if LDBL_MANT_DIG == 106 || LDBL_MIN_EXP == -16382
/* IBM long double and m68k extended format do not meet the definition
   of an IEC 60559 interchange or extended format.  */
_Static_assert (LDBL_IS_IEC_60559 == 0);
#else
_Static_assert (LDBL_IS_IEC_60559 == 1);
#endif
#endif
