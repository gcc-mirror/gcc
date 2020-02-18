/* PR tree-optimization/93780 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx" { target avx } } */

typedef float V __attribute__((vector_size (32)));

float
foo (void)
{
  const float init[6] = {};
  V v = {};
  __builtin_memcpy (&v, init, sizeof (init));
  return v[0];
}
