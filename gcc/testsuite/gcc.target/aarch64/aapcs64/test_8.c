/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_8.c"

struct z
{
  double x[4];
};

struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#include "abitest.h"
#else
  ARG(struct z, a, D0)
  ARG(struct z, b, D4)
  ARG(double, 0.5, STACK)
  ARG(int, 7, W0)
  LAST_ARG(int, 8, W1) 
#endif
