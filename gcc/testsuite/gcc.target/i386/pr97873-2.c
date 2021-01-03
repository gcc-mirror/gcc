/* PR target/97873 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -mtune=generic" } */

#ifdef __SIZEOF_INT128__
typedef __int128_t T;
#else
typedef long long T;
#endif

T test_abs (T x)
{
  return (x < 0) ? -x : x;
}

/* { dg-final { scan-assembler "adc" } } */

T test_smin (T x, T y)
{
  return (x < y) ? x : y;
}

/* { dg-final { scan-assembler "sbb" } } */
