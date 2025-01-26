/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -save-temps" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler "\tvsrlb\t" } } */
/* { dg-final { scan-assembler-not "\tvsrl\t" } } */

#include <assert.h>

typedef __attribute__ ((vector_size (16))) unsigned long long uv2di;
typedef __attribute__ ((vector_size (16))) unsigned __int128 uv1ti;

__attribute__ ((noipa)) uv1ti
const_shift (uv1ti x)
{
  return x >> 16;
}

int
main (void)
{
  uv2di x, y;

  x = (uv2di){ 0xcafed00ddeadbeef, 0xcafed00ddeadbeef };
  y = (uv2di){ 0x0000cafed00ddead, 0xbeefcafed00ddead };
  x = (uv2di) const_shift ((uv1ti) x);
  assert (x[0] == y[0] && x[1] == y[1]);

  return 0;
}
