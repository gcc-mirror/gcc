/* { dg-do compile } */
/* { dg-options "-O2 -Wno-long-long" } */

typedef unsigned int __attribute ((mode(TI))) uti_t;
typedef int __attribute ((mode(TI))) ti_t;

long test1(long x, long y)
{
  return ((ti_t)x * (ti_t)y) >> 64;
}

long test2(long x)
{
  return ((ti_t)x * 19065) >> 64;
}

long test3(long x, long y)
{
  return (uti_t)((ti_t)x * (ti_t)y) >> 64;
}

long test4(long x)
{
  return (uti_t)((ti_t)x * 19065) >> 64;
}

ti_t test5(long x, long y)
{
  return ((ti_t)x * (ti_t)y) >> 64;
}

ti_t test6(long x)
{
  return ((ti_t)x * 19065) >> 64;
}

uti_t test7(long x, long y)
{
  return (uti_t)((ti_t)x * (ti_t)y) >> 64;
}

uti_t test8(long x)
{
  return (uti_t)((ti_t)x * 19065) >> 64;
}

/* { dg-final { scan-assembler-times "mul.hi.s64" 8 } } */
