/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -save-temps" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler "\tvsra\t" } } */
/* { dg-final { scan-assembler "\tvsrab\t" } } */

#include <assert.h>

typedef __attribute__ ((vector_size (16))) signed long long v2di;
typedef __attribute__ ((vector_size (16))) signed __int128 v1ti;

__attribute__ ((noipa)) v1ti
dyn_shift (v1ti x, unsigned int n)
{
  return x >> n;
}

int
main (void)
{
  v2di x, y;

  x = (v2di){ 0xcafed00ddeadbeef, 0xcafed00ddeadbeef };
  y = (v2di){ 0xe57f6806ef56df77, 0xe57f6806ef56df77 };
  x = (v2di) dyn_shift ((v1ti) x, 1);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0x4afed00dbeadbeee, 0xcafed00ddeadbeef };
  y = (v2di){ 0x257f6806df56df77, 0x657f6806ef56df77 };
  x = (v2di) dyn_shift ((v1ti) x, 1);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0xcafed00ddeadbeef, 0xcafed00ddeadbeef };
  y = (v2di){ 0xffffcafed00ddead, 0xbeefcafed00ddead };
  x = (v2di) dyn_shift ((v1ti) x, 16);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0x4afed00ddead3eef, 0xcafed00ddeadbeef };
  y = (v2di){ 0x00004afed00ddead, 0x3eefcafed00ddead };
  x = (v2di) dyn_shift ((v1ti) x, 16);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0xcafed00ddeadbeef, 0xcafed00ddeadbeef };
  y = (v2di){ 0xfffffffffff2bfb4, 0x377ab6fbbf2bfb4 };
  x = (v2di) dyn_shift ((v1ti) x, 42);
  assert (x[0] == y[0] && x[1] == y[1]);

  x = (v2di){ 0x4afed00ddeadbeee, 0xcafed00ddeadbeef };
  y = (v2di){ 0x000000000012bfb4, 0x0377ab6fbbb2bfb4 };
  x = (v2di) dyn_shift ((v1ti) x, 42);
  assert (x[0] == y[0] && x[1] == y[1]);

  return 0;
}
