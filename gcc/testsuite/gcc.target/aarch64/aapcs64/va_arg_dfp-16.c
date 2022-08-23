/* Test AAPCS64 layout and __builtin_va_arg.

   This test is focused particularly on __fp16 unnamed homogeneous
   floating-point aggregate types which should be passed in fp/simd
   registers until we run out of those, then the stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-16.c"
#include "type-def.h"

struct hfa_f16x1_t hfa_f16x1 = {2.0f};
struct hfa_f16x2_t hfa_f16x2 = {4.0f, 8.0f};
struct hfa_f16x3_t hfa_f16x3 = {16.0f, 32.0f, 64.0f};

#include "abitest.h"
#else
  ARG      (int, 1, W0, LAST_NAMED_ARG_ID)
  DOTS
  ANON     (struct hfa_f16x1_t, hfa_f16x1, H0     , 0)
  ANON     (struct hfa_f16x2_t, hfa_f16x2, H1     , 1)
  ANON     (struct hfa_f16x3_t, hfa_f16x3, H3     , 2)
  ANON     (struct hfa_f16x2_t, hfa_f16x2, H6     , 3)
  ANON     (struct hfa_f16x1_t, hfa_f16x1, STACK  , 4)
  LAST_ANON(_Decimal64        , 1.0dd    , STACK+8, 5)
#endif
