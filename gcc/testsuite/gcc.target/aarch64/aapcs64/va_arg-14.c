/* Test AAPCS64 layout and __builtin_va_start.

   Pass named HFA/HVA argument on stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-14.c"
#include "type-def.h"

struct hfa_fx2_t hfa_fx2 = {1.2f, 2.2f};
struct hfa_fx3_t hfa_fx3 = {3.2f, 4.2f, 5.2f};
vf4_t float32x4 = {6.2f, 7.2f, 8.2f, 9.2f};
vf4_t float32x4_2 = {10.2f, 11.2f, 12.2f, 13.2f};

#include "abitest.h"
#else
  ARG (float, 1.0f, S0, 0)
  ARG (float, 2.0f, S1, 1)
  ARG (float, 3.0f, S2, 2)
  ARG (float, 4.0f, S3, 3)
  ARG (float, 5.0f, S4, 4)
  ARG (float, 6.0f, S5, 5)
  ARG (float, 7.0f, S6, 6)
  ARG (struct hfa_fx3_t, hfa_fx3, STACK, 7)
  /* Previous argument size has been rounded up to the nearest multiple of
     8 bytes.  */
  ARG (struct hfa_fx2_t, hfa_fx2, STACK + 16, 8)
  /* NSAA is rounded up to the nearest natural alignment of float32x4.  */
  ARG (vf4_t, float32x4, STACK + 32, 9)
  ARG (vf4_t, float32x4_2, STACK + 48, LAST_NAMED_ARG_ID)
  DOTS
  LAST_ANON (double, 123456789.987, STACK + 64, 11)
#endif
