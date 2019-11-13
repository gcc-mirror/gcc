/* Test *_NORM_MAX not defined for C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <float.h>

#ifdef FLT_NORM_MAX
#error "FLT_NORM_MAX defined"
#endif

#ifdef DBL_NORM_MAX
#error "DBL_NORM_MAX defined"
#endif

#ifdef LDBL_NORM_MAX
#error "LDBL_NORM_MAX defined"
#endif
