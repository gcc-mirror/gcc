/* PR target/89848 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mtune=pentium3m" } */

long long
foo (long long x)
{
  x >>= 3;
  x <<= x;
  return x;
}
