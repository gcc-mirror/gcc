/* PR target/104779 */
/* { dg-do compile } */
/* { dg-options "-O1 --param sccvn-max-alias-queries-per-access=0" } */

__attribute__ ((simd)) int
foo (int x, int y, int z)
{
  return (x & y) * !!z;
}

__attribute__ ((simd)) int
bar (int x, int y, int z)
{
  return (x | y) * !!z;
}

__attribute__ ((simd)) int
baz (int x, int y, int z)
{
  return (x ^ y) * !!z;
}

__attribute__ ((simd, target ("avx512dq"))) long
qux (long x, long y, long z)
{
  return (x * y) * !!z;
}
