/* Test for <float.h> C90 macros.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do preprocess } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

/* This test checks that the C90 macros (but not the C99 ones) are defined;
   it does not check the correctness of their values.  */

#include <float.h>

#ifndef FLT_ROUNDS
#error "FLT_ROUNDS undefined"
#endif

#ifndef FLT_RADIX
#error "FLT_RADIX undefined"
#endif

#ifndef FLT_MANT_DIG
#error "FLT_MANT_DIG undefined"
#endif

#ifndef FLT_DIG
#error "FLT_DIG undefined"
#endif

#ifndef FLT_MIN_EXP
#error "FLT_MIN_EXP undefined"
#endif

#ifndef FLT_MIN_10_EXP
#error "FLT_MIN_10_EXP undefined"
#endif

#ifndef FLT_MAX_EXP
#error "FLT_MAX_EXP undefined"
#endif

#ifndef FLT_MAX_10_EXP
#error "FLT_MAX_10_EXP undefined"
#endif

#ifndef FLT_MAX
#error "FLT_MAX undefined"
#endif

#ifndef FLT_EPSILON
#error "FLT_EPSILON undefined"
#endif

#ifndef FLT_MIN
#error "FLT_MIN undefined"
#endif

#ifndef DBL_MANT_DIG
#error "DBL_MANT_DIG undefined"
#endif

#ifndef DBL_DIG
#error "DBL_DIG undefined"
#endif

#ifndef DBL_MIN_EXP
#error "DBL_MIN_EXP undefined"
#endif

#ifndef DBL_MIN_10_EXP
#error "DBL_MIN_10_EXP undefined"
#endif

#ifndef DBL_MAX_EXP
#error "DBL_MAX_EXP undefined"
#endif

#ifndef DBL_MAX_10_EXP
#error "DBL_MAX_10_EXP undefined"
#endif

#ifndef DBL_MAX
#error "DBL_MAX undefined"
#endif

#ifndef DBL_EPSILON
#error "DBL_EPSILON undefined"
#endif

#ifndef DBL_MIN
#error "DBL_MIN undefined"
#endif

#ifndef LDBL_MANT_DIG
#error "LDBL_MANT_DIG undefined"
#endif

#ifndef LDBL_DIG
#error "LDBL_DIG undefined"
#endif

#ifndef LDBL_MIN_EXP
#error "LDBL_MIN_EXP undefined"
#endif

#ifndef LDBL_MIN_10_EXP
#error "LDBL_MIN_10_EXP undefined"
#endif

#ifndef LDBL_MAX_EXP
#error "LDBL_MAX_EXP undefined"
#endif

#ifndef LDBL_MAX_10_EXP
#error "LDBL_MAX_10_EXP undefined"
#endif

#ifndef LDBL_MAX
#error "LDBL_MAX undefined"
#endif

#ifndef LDBL_EPSILON
#error "LDBL_EPSILON undefined"
#endif

#ifndef LDBL_MIN
#error "LDBL_MIN undefined"
#endif

#ifdef FLT_EVAL_METHOD
#error "FLT_EVAL_METHOD defined"
#endif

#ifdef DECIMAL_DIG
#error "DECIMAL_DIG defined"
#endif
