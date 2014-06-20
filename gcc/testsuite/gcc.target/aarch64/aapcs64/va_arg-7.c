/* Test AAPCS64 layout and __builtin_va_arg.

   This test covers complex types.  Complex floating-point types are treated
   as homogeneous floating-point aggregates, while complex integral types
   are treated as general composite types.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-7.c"
#include "type-def.h"

_Complex __int128 complex_qword = 567890 + 678901i;

#include "abitest.h"
#else
  ARG      (int, 1, W0, LAST_NAMED_ARG_ID)
  DOTS
  /* Complex floating-point types are passed in fp/simd registers.  */
  ANON     (_Complex float      , 12.3f + 23.4fi              , S0,  0)
  ANON     (_Complex double     , 34.56 + 45.67i              , D2,  1)
  ANON     (_Complex long double, 56789.01234L + 67890.12345Li, Q4,  2)

  /* Complex integral types are passed in general registers or via reference.  */
  ANON     (_Complex short      , (_Complex short) (12345 + 23456i), X1, 10)
  ANON     (_Complex int        , 34567 + 45678i              , X2, 11)
  PTR_ANON (_Complex __int128   , complex_qword               , X3, 12)

  LAST_ANON(int                 , 1                           , W4, 20)
#endif
