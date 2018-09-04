/* PR middle-end/87138 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O -fno-tree-fre -mavx512bw -mtune=k8" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

typedef int U __attribute__ ((vector_size (64)));
typedef __int128 V __attribute__ ((vector_size (64)));
V g, i;

static inline void
foo (unsigned h, V j, U k, V n)
{
  k /= h;
  __builtin_memmove (&h, &n, 1);
  n[j[1]] *= 0x7FFFFFFFFFFFFFFF;
  j[k[5]] = 0;
  g = n;
  i = h + j + n;
}

void
avx512bw_test ()
{
  foo (~0, (V) { }, (U) { 5 }, (V) { 3 });
  if (g[0] != (__int128) 3 * 0x7FFFFFFFFFFFFFFF)
    abort ();
}
