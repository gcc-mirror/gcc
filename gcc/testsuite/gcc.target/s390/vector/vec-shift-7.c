/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -save-temps" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler "\tvsra\t" } } */
/* { dg-final { scan-assembler "\tvsrab\t" } } */

#include <assert.h>

typedef __attribute__ ((vector_size (16))) signed long long v2di;
typedef __attribute__ ((vector_size (16))) signed __int128 v1ti;

__attribute__ ((noipa)) v1ti
const_shift (v1ti x)
{
  return x >> 42;
}

int
main (void)
{
  v2di x, y;

  x = (v2di){ 0xcafed00ddeadbeef, 0xcafed00ddeadbeef };
  y = (v2di){ 0xfffffffffff2bfb4, 0x377ab6fbbf2bfb4 };
  x = (v2di) const_shift ((v1ti) x);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0x4afed00ddeadbeee, 0xcafed00ddeadbeef };
  y = (v2di){ 0x000000000012bfb4, 0x0377ab6fbbb2bfb4 };
  x = (v2di) const_shift ((v1ti) x);
  assert (x[0] == y[0] && x[1] == y[1]);

  return 0;
}
