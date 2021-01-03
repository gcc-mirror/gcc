/* Test sNaN macros for _FloatN and _FloatNx not defined for C11 with
   __STDC_WANT_IEC_60559_TYPES_EXT__.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>

#ifdef FLT16_SNAN
#error "FLT16_SNAN defined"
#endif

#ifdef FLT32_SNAN
#error "FLT32_SNAN defined"
#endif

#ifdef FLT64_SNAN
#error "FLT64_SNAN defined"
#endif

#ifdef FLT128_SNAN
#error "FLT128_SNAN defined"
#endif

#ifdef FLT32X_SNAN
#error "FLT32X_SNAN defined"
#endif

#ifdef FLT64X_SNAN
#error "FLT64X_SNAN defined"
#endif

#ifdef FLT128X_SNAN
#error "FLT128X_SNAN defined"
#endif
