/* PR rtl-optimization/97282 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "call\[^\n\r]*__udivmod\[dt]i4" } } */

#ifdef __SIZEOF_INT128__
typedef __uint128_t T;
#else
typedef unsigned long long T;
#endif

unsigned long
foo (T x)
{
  if (x == 0)
    return 0;

  unsigned long ret = 0;
  while (x > 0)
    {
      ret = ret + x % 123456;
      x = x / 123456;
    }
  return ret;
}
