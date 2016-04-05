/* PR target/70510 */
/* { dg-do assemble { target avx512bw } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-Og -mavx512bw -masm=intel" } */

typedef int V __attribute__ ((vector_size (64)));

V
foo (V u, V v)
{
  v[0] |= v[u[0]];
  u /= ((V)v)[0];
  return u;
}
