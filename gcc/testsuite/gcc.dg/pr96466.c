/* PR tree-optimization/96466 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-Og -finline-functions-called-once -fno-tree-ccp" } */

typedef unsigned long __attribute__ ((__vector_size__ (8))) V;

V
bar (unsigned long x, V v)
{
  v &= x >= v;
  return (V) v;
}

V
foo (void)
{
  return bar (5, (V) 4441221375);
}
