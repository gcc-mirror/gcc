/* PR rtl-optimization/78575 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -g -Wno-psabi" } */

typedef unsigned __int128 V __attribute__((vector_size(64)));

V g;

void
foo (V v)
{
  unsigned __int128 x = 1;
  int c = v[1] <= ~x;
  v &= v[1];
  g = v;
}
