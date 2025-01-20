/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -save-temps" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler "\tvsra\t" } } */
/* { dg-final { scan-assembler-not "\tvsrab\t" } } */

#include <assert.h>

typedef __attribute__ ((vector_size (16))) signed long long v2di;
typedef __attribute__ ((vector_size (16))) signed __int128 v1ti;

__attribute__ ((noipa)) v1ti
const_shift (v1ti x)
{
  return x >> 1;
}

int
main (void)
{
  v2di x, y;

  x = (v2di){ 0xcafed00ddeadbeef, 0xcafed00ddeadbeef };
  y = (v2di){ 0xe57f6806ef56df77, 0xe57f6806ef56df77 };
  x = (v2di) const_shift ((v1ti) x);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0x4afed00dbeadbeee, 0xcafed00ddeadbeef };
  y = (v2di){ 0x257f6806df56df77, 0x657f6806ef56df77 };
  x = (v2di) const_shift ((v1ti) x);
  assert (x[0] == y[0] && x[1] == y[1]);

  return 0;
}
