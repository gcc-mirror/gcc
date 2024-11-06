/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "--param=vect-partial-vector-usage=2 -O2" } */
/* { dg-additional-options "-mavx512vbmi2" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-march=armv9-a" { target aarch64-*-* } } */

signed int __attribute__ ((noipa))
fn (signed int n,
   signed char *restrict a,
   signed char *restrict b,
   signed char *restrict c,
   signed char *restrict d)
{
  signed int res = 0;

  for (int i = 0; i < n; ++i)
    {
      res += a[i] * b[i];
      res += i + 1;
      res += c[i] * d[i];
    }
  return res;
}
