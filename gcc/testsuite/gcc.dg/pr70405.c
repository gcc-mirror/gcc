/* PR tree-optimization/70405 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */
/* { dg-additional-options "-mavx512f" { target i?86-*-* x86_64-*-* } } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

typedef short V __attribute__ ((vector_size (32)));

int
foo (V *p)
{
  V v = *p;
  v >>= v;
  v -= v[0];
  return v[3];
}
