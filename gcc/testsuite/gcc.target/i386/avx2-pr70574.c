/* PR rtl-optimization/70574 */
/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O -frerun-cse-after-loop -fno-tree-ccp -mcmodel=medium -mavx2" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

#include "avx2-check.h"

typedef char A __attribute__((vector_size (32)));
typedef short B __attribute__((vector_size (32)));

int
foo (int x, __int128 y, __int128 z, A w)
{
  y <<= 64;
  w *= (A) { 0, -1, z, 0, ~y };
  return w[0] + ((B) { x, 0, y, 0, -1 } | 1)[4];
}

static void
avx2_test ()
{
  int x = foo (0, 0, 0, (A) {});
  if (x != -1)
    __builtin_abort ();
}
