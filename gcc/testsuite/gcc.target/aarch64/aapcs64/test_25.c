/* Test AAPCS64 layout

   Test homogeneous floating-point aggregates and homogeneous short-vector
   aggregates, which should be passed in SIMD/FP registers or via the
   stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_25.c"

typedef float vf2_t __attribute__((vector_size (8)));
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

typedef float vf4_t __attribute__((vector_size(16)));
struct x1
{
  vf4_t v;
} s1;

struct x2
{
  double df[3];
} s2;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  s0.v = (vf2_t){ 17.f, 18.f };
  s1.v = (vf4_t){ 567.890f, 678.901f, 789.012f, 890.123f };
  s2.df[0] = 123.456;
  s2.df[1] = 234.567;
  s2.df[2] = 345.678;
  s3.v[0] = (vf2_t){ 19.f, 20.f };
  s3.v[1] = (vf2_t){ 23.f, 24.f };
  s4.v[0] = (vf2_t){ 27.f, 28.f };
  s4.v[1] = (vf2_t){ 31.f, 32.f };
  s4.v[2] = (vf2_t){ 35.f, 36.f };
}

#include "abitest.h"
#else
ARG (struct x0, s0, D0)
ARG (struct x2, s2, D1)
ARG (struct x1, s1, Q4)
ARG (struct x3, s3, D5)
ARG (struct x4, s4, STACK)
ARG (int, 0xdeadbeef, W0)
LAST_ARG (double, 456.789, STACK+24)
#endif
