/* Test AAPCS64 layout and __builtin_va_start.

   Pass named HFA/HVA argument on stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-14.c"
#include "type-def.h"

struct hfa_dfx2_t hfa_dfx2 = {1.2df, 2.2df};
struct hfa_dfx3_t hfa_dfx3 = {3.2df, 4.2df, 5.2df};
vf4_t float32x4 = {6.2f, 7.2f, 8.2f, 9.2f};
vf4_t float32x4_2 = {10.2f, 11.2f, 12.2f, 13.2f};

#include "abitest.h"
#else
  ARG (_Decimal32, 1.0df, S0, 0)
  ARG (_Decimal32, 2.0df, S1, 1)
  ARG (_Decimal32, 3.0df, S2, 2)
  ARG (_Decimal32, 4.0df, S3, 3)
  ARG (_Decimal32, 5.0df, S4, 4)
  ARG (_Decimal32, 6.0df, S5, 5)
  ARG (_Decimal32, 7.0df, S6, 6)
  ARG (struct hfa_dfx3_t, hfa_dfx3, STACK, 7)
  /* Previous argument size has been rounded up to the nearest multiple of
     8 bytes.  */
  ARG (struct hfa_dfx2_t, hfa_dfx2, STACK + 16, 8)
  /* NSAA is rounded up to the nearest natural alignment of float32x4.  */
  ARG (vf4_t, float32x4, STACK + 32, 9)
  ARG (vf4_t, float32x4_2, STACK + 48, LAST_NAMED_ARG_ID)
  DOTS
  LAST_ANON (_Decimal64, 123456789.987dd, STACK + 64, 11)
#endif
