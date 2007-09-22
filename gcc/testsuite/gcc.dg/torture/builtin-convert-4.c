/* Copyright (C) 2007  Free Software Foundation.

   Verify that nearbyint isn't transformed into e.g. rint or lrint
   when -ftrapping-math is set.

   Written by Kaveh ghazi, 2007-03-04.  */

/* { dg-do compile } */
/* { dg-options "-ftrapping-math -fdump-tree-original" } */
/* { dg-add-options c99_runtime } */

#include "../builtins-config.h"

extern void bar (long);

#define TESTIT(FUNC) do { \
  bar (__builtin_##FUNC(d)); \
  bar (__builtin_##FUNC##f(f)); \
  bar (__builtin_##FUNC##l(ld)); \
} while (0)

void __attribute__ ((__noinline__)) foo (double d, float f, long double ld)
{
  TESTIT(nearbyint);
}

int main()
{
  foo (1.0, 2.0, 3.0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "nearbyint " 1 "original" } } */
/* { dg-final { scan-tree-dump-times "nearbyintf" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "nearbyintl" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
