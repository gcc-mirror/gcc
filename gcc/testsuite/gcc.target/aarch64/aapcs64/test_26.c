/* Test AAPCS64 layout.

   Test some small structures that should be passed in GPRs.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_26.c"

struct y0
{
  char ch;
} c0 = { 'A' };

struct y2
{
  long long ll[2];
} c2 = { 0xDEADBEEF, 0xCAFEBABE };

struct y3
{
  int i[3];
} c3 = { 56789, 67890, 78901 };

typedef float vf2_t __attribute__((vector_size (8)));
struct x0
{
  vf2_t v;
} s0;

typedef short vh4_t __attribute__((vector_size (8)));

struct x1
{
  vh4_t v[2];
} s1;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  s0.v = (vf2_t){ 17.f, 18.f };
  s1.v[0] = (vh4_t){ 345, 456, 567, 678 };
  s1.v[1] = (vh4_t){ 789, 890, 901, 123 };
}

#include "abitest.h"
#else
ARG (struct y0, c0, X0)
ARG (struct y2, c2, X1)
ARG (struct y3, c3, X3)
ARG_NONFLAT (struct x0, s0, D0, f32in64)
ARG (struct x1, s1, D1)
LAST_ARG_NONFLAT (int, 89012, X5, i32in64)
#endif
