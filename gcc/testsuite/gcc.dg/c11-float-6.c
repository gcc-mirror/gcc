/* Test *_IS_IEC_60559 not defined for C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <float.h>

#ifdef FLT_IS_IEC_60559
#error "FLT_IS_IEC_60559 defined"
#endif

#ifdef DBL_IS_IEC_60559
#error "DBL_IS_IEC_60559 defined"
#endif

#ifdef LDBL_IS_IEC_60559
#error "LDBL_IS_IEC_60559 defined"
#endif
