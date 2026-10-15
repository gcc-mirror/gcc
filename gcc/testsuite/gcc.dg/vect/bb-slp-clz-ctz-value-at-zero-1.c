/* Lanes of .CLZ and .CTZ whose value at zero is not the value of the
   target must keep that value.  */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target clzsi } */
/* { dg-require-effective-target ctzsi } */
/* { dg-additional-options "-fgimple -fdump-tree-optimized" } */

#include "tree-vect.h"

unsigned int a[4];
int r[4];
unsigned int u[4];

void __GIMPLE (ssa,startwith ("fix_loops")) __attribute__((noipa))
clz5 (void)
{
  unsigned int _1;
  unsigned int _2;
  unsigned int _3;
  unsigned int _4;
  int _5;
  int _6;
  int _7;
  int _8;

  __BB(2):
  _1 = a[0];
  _2 = a[1];
  _3 = a[2];
  _4 = a[3];
  _5 = .CLZ (_1, 5);
  _6 = .CLZ (_2, 5);
  _7 = .CLZ (_3, 5);
  _8 = .CLZ (_4, 5);
  r[0] = _5;
  r[1] = _6;
  r[2] = _7;
  r[3] = _8;
  return;
}

/* The conversion makes the call part of a vectorizer pattern, which
   selects 5 for zero lanes.  */

void __GIMPLE (ssa,startwith ("fix_loops")) __attribute__((noipa))
clz5u (void)
{
  unsigned int _1;
  unsigned int _2;
  unsigned int _3;
  unsigned int _4;
  int _5;
  int _6;
  int _7;
  int _8;
  unsigned int _9;
  unsigned int _10;
  unsigned int _11;
  unsigned int _12;

  __BB(2):
  _1 = a[0];
  _2 = a[1];
  _3 = a[2];
  _4 = a[3];
  _5 = .CLZ (_1, 5);
  _6 = .CLZ (_2, 5);
  _7 = .CLZ (_3, 5);
  _8 = .CLZ (_4, 5);
  _9 = (unsigned int) _5;
  _10 = (unsigned int) _6;
  _11 = (unsigned int) _7;
  _12 = (unsigned int) _8;
  u[0] = _9;
  u[1] = _10;
  u[2] = _11;
  u[3] = _12;
  return;
}

void __GIMPLE (ssa,startwith ("fix_loops")) __attribute__((noipa))
ctz5u (void)
{
  unsigned int _1;
  unsigned int _2;
  unsigned int _3;
  unsigned int _4;
  int _5;
  int _6;
  int _7;
  int _8;
  unsigned int _9;
  unsigned int _10;
  unsigned int _11;
  unsigned int _12;

  __BB(2):
  _1 = a[0];
  _2 = a[1];
  _3 = a[2];
  _4 = a[3];
  _5 = .CTZ (_1, 5);
  _6 = .CTZ (_2, 5);
  _7 = .CTZ (_3, 5);
  _8 = .CTZ (_4, 5);
  _9 = (unsigned int) _5;
  _10 = (unsigned int) _6;
  _11 = (unsigned int) _7;
  _12 = (unsigned int) _8;
  u[0] = _9;
  u[1] = _10;
  u[2] = _11;
  u[3] = _12;
  return;
}

/* 32 is the value of the target on AArch64, so this vectorizes there.  */

void __GIMPLE (ssa,startwith ("fix_loops")) __attribute__((noipa))
clz32u (void)
{
  unsigned int _1;
  unsigned int _2;
  unsigned int _3;
  unsigned int _4;
  int _5;
  int _6;
  int _7;
  int _8;
  unsigned int _9;
  unsigned int _10;
  unsigned int _11;
  unsigned int _12;

  __BB(2):
  _1 = a[0];
  _2 = a[1];
  _3 = a[2];
  _4 = a[3];
  _5 = .CLZ (_1, 32);
  _6 = .CLZ (_2, 32);
  _7 = .CLZ (_3, 32);
  _8 = .CLZ (_4, 32);
  _9 = (unsigned int) _5;
  _10 = (unsigned int) _6;
  _11 = (unsigned int) _7;
  _12 = (unsigned int) _8;
  u[0] = _9;
  u[1] = _10;
  u[2] = _11;
  u[3] = _12;
  return;
}

static void __attribute__((noipa))
check (const unsigned int *p, unsigned int e0, unsigned int e1,
       unsigned int e2, unsigned int e3)
{
  if (p[0] != e0 || p[1] != e1 || p[2] != e2 || p[3] != e3)
    __builtin_abort ();
}

int
main (void)
{
  check_vect ();

  /* Write A at run time, so that LTO cannot fold the loads.  */
  a[1] = 1;
  a[3] = 0x80000000u;
  clz5 ();
  check ((const unsigned int *) r, 5, 31, 5, 0);
  clz5u ();
  check (u, 5, 31, 5, 0);
  ctz5u ();
  check (u, 5, 0, 5, 31);
  clz32u ();
  check (u, 32, 31, 32, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times {\.CLZ \(vect_} 2 "optimized" { target aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump-times {\.CTZ \(vect_} 1 "optimized" { target aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump-times {\.VCOND_MASK \(} 2 "optimized" { target aarch64*-*-* } } } */
