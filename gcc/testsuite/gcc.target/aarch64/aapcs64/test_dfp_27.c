/* Test AAPCS64 layout

   Test named homogeneous floating-point aggregates of __fp16 data,
   which should be passed in SIMD/FP registers or via the stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_27.c"

struct x0
{
  __fp16 v[1];
} f16x1;

struct x1
{
  __fp16 v[2];
} f16x2;

struct x2
{
  __fp16 v[3];
} f16x3;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  f16x1.v[0] = 2.0f;
  f16x2.v[0] = 4.0f;
  f16x2.v[1] = 8.0f;
  f16x3.v[0] = 16.0f;
  f16x3.v[1] = 32.0f;
  f16x3.v[2] = 64.0f;
}

#include "abitest.h"
#else
ARG (struct x0, f16x1, H0)
ARG (struct x1, f16x2, H1)
ARG (struct x2, f16x3, H3)
ARG (struct x1, f16x2, H6)
ARG (struct x0, f16x1, STACK)
ARG (int, 0xdeadbeef, W0)
LAST_ARG (_Decimal64, 456.789dd, STACK+8)
#endif
