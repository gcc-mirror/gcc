/* Test AAPCS64 function result return.

   This test covers complex types.  Complex floating-point types are treated
   as homogeneous floating-point aggregates, while complex integral types
   are treated as general composite types.  */

/* { dg-do run { target aarch64*-*-* } } */
/* { dg-require-effective-target aarch64_big_endian } */
/* { dg-additional-options "-mbranch-protection=none" } */
/* { dg-additional-sources "abitest.S" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "func-ret-4.c"

#include "abitest-2.h"
#else
  /* Complex floating-point types are passed in fp/simd registers.  */
FUNC_VAL_CHECK ( 0, _Complex float , 12.3f + 23.4fi, S0, flat)
FUNC_VAL_CHECK ( 1, _Complex double, 34.56 + 45.67i, D0, flat)
FUNC_VAL_CHECK ( 2, _Complex long double, 56789.01234 + 67890.12345i, Q0, flat)

  /* Complex integral types are passed in general registers or via a pointer in
     X8.  */
FUNC_VAL_CHECK (10, _Complex short , 12345 + 23456i, X0, flat)
FUNC_VAL_CHECK (11, _Complex int   , 34567 + 45678i, X0, flat)
FUNC_VAL_CHECK (12, _Complex __int128, 567890 + 678901i, X8, flat)

#endif
