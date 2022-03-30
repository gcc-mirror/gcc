/* Test AAPCS64 layout

   Test homogeneous floating-point aggregates and homogeneous short-vector
   aggregates, which should be passed in SIMD/FP registers or via the
   stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_25.c"

typedef _Decimal32 vf2_t __attribute__((vector_size (8)));
struct x0
{
  vf2_t v;
} s0;
struct x3
{
  vf2_t v[2];
} s3;
struct x4
{
  vf2_t v[3];
} s4;

typedef _Decimal32 vf4_t __attribute__((vector_size(16)));
struct x1
{
  vf4_t v;
} s1;

struct x2
{
  _Decimal64 df[3];
} s2;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  s0.v = (vf2_t){ 17.df, 18.df };
  s1.v = (vf4_t){ 567.890df, 678.901df, 789.012df, 890.123df };
  s2.df[0] = 123.456dd;
  s2.df[1] = 234.567dd;
  s2.df[2] = 345.678dd;
  s3.v[0] = (vf2_t){ 19.df, 20.df };
  s3.v[1] = (vf2_t){ 23.df, 24.df };
  s4.v[0] = (vf2_t){ 27.df, 28.df };
  s4.v[1] = (vf2_t){ 31.df, 32.df };
  s4.v[2] = (vf2_t){ 35.df, 36.df };
}

#include "abitest.h"
#else
ARG (struct x0, s0, D0)
ARG (struct x2, s2, D1)
ARG (struct x1, s1, Q4)
ARG (struct x3, s3, D5)
ARG (struct x4, s4, STACK)
ARG (int, 0xdeadbeef, W0)
LAST_ARG (_Decimal64, 456.789dd, STACK+24)
#endif
