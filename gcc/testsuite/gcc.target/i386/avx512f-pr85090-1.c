/* PR middle-end/85090 */
/* { dg-do run { target int128 } } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -fno-tree-dominator-opts -mavx512f -fira-algorithm=priority" } */

#include "avx512f-check.h"

typedef unsigned short U __attribute__ ((vector_size (64)));
typedef unsigned int V __attribute__ ((vector_size (64)));
typedef unsigned __int128 W __attribute__ ((vector_size (64)));

V h;
W d, e, g;
U f;

static __attribute__((noipa)) U
foo (U i)
{
  f >>= ((U)d > f) & 1;
  i[0] <<= 1;
  e = (7 & -d) << (7 & -(g & 7));
  return i;
}

void
avx512f_test (void)
{
  U x;
  for (unsigned i = 0; i < 32; i++)
    x[i] = i;
  x = foo (x);
  for (unsigned i = 0; i < 32; i++)
    if (x[i] != i)
      abort ();
}
