/* PR tree-optimization/105528 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fcompare-debug" } */
/* { dg-additional-options "-mavx512f" { target i?86-*-* x86_64-*-* } } */

typedef unsigned V __attribute__((__vector_size__ (64)));
V g;

V
bar (V v)
{
  V w;
  v <<= (V){(V){}[53]} >= v & 5;
  w[w[5]] -= ~0;
  v %= ~0;
  return v + w;
}

void
foo (void)
{
  g -= (V){bar((V){~0})[3]};
}
