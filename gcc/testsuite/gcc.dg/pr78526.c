/* PR rtl-optimization/78526 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O -fno-tree-ccp -fno-tree-sra -g -w" } */
/* { dg-additional-options "-mavx512bw" { target i?86-*-* x86_64-*-* } } */

typedef unsigned U __attribute__ ((vector_size (64)));
typedef unsigned __int128 V __attribute__ ((vector_size (64)));

static inline V
bar (U u, U x, V v)
{
  v = (V)(U) { 0, ~0 };
  v[x[0]] <<= u[-63];
  return v;
}

V
foo (U u)
{
  return bar (u, (U) {}, (V) {});
}
