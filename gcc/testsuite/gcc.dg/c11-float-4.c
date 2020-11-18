/* Test infinity and NaN macros not defined for C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <float.h>

#ifdef INFINITY
#error "INFINITY defined"
#endif

#ifdef NAN
#error "NAN defined"
#endif

#ifdef FLT_SNAN
#error "FLT_SNAN defined"
#endif

#ifdef DBL_SNAN
#error "DBL_SNAN defined"
#endif

#ifdef LDBL_SNAN
#error "LDBL_SNAN defined"
#endif
