/* PR target/70049 */
/* { dg-do assemble { target avx } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-Og -mavx -masm=intel" } */

typedef unsigned short A;
typedef unsigned short B __attribute__ ((vector_size (32)));
typedef unsigned int C;
typedef unsigned int D __attribute__ ((vector_size (32)));
typedef unsigned long long E;
typedef unsigned long long F __attribute__ ((vector_size (32)));

C
foo(A a, C b, E c, F d, B e, D f, F g)
{
  b <<= 28;
  e[1] += b;
  d %= (F) { 0, f[4] } | 1;
  return a + b + c + d[3] + e[1] + g[3];
}
