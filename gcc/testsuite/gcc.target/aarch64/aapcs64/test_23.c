/* Test AAPCS64 layout.

   Larger than machine-supported vector size.  The behavior is unspecified by
   the AAPCS64 document; the implementation opts for pass by reference.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_23.c"

typedef char A __attribute__ ((vector_size (64)));

struct y
{
  double df[8];
};

union u
{
  struct y x;
  A a;
} u;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  u.x.df[0] = 1.0;
  u.x.df[1] = 2.0;
  u.x.df[2] = 3.0;
  u.x.df[3] = 4.0;
  u.x.df[4] = 5.0;
  u.x.df[5] = 6.0;
  u.x.df[6] = 7.0;
  u.x.df[7] = 8.0;
}

#include "abitest.h"
#else
ARG (float, 123.0f, S0)
PTR (A, u.a, X0)
LAST_ARG_NONFLAT (int, 0xdeadbeef, X1, i32in64)
#endif
