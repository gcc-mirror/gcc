/* { dg-do compile } */
/* { dg-options "-O2 -Wno-long-long" } */

typedef unsigned int __attribute ((mode(TI))) uti_t;
typedef int __attribute ((mode(TI))) ti_t;

unsigned long test1(unsigned long x, unsigned long y)
{
  return ((uti_t)x * (uti_t)y) >> 64;
}

unsigned long test2(unsigned long x)
{
  return ((uti_t)x * 19065) >> 64;
}

unsigned long test3(unsigned long x, unsigned long y)
{
  return (ti_t)((uti_t)x * (uti_t)y) >> 64;
}

unsigned long test4(unsigned long x)
{
  return (ti_t)((uti_t)x * 19065) >> 64;
}

uti_t test5(unsigned long x, unsigned long y)
{
  return ((uti_t)x * (uti_t)y) >> 64;
}

uti_t test6(unsigned long x)
{
  return ((uti_t)x * 19065) >> 64;
}

ti_t test7(unsigned long x, unsigned long y)
{
  return (ti_t)((uti_t)x * (uti_t)y) >> 64;
}

ti_t test8(unsigned long x)
{
  return (ti_t)((uti_t)x * 19065) >> 64;
}

/* { dg-final { scan-assembler-times "mul.hi.u64" 8 } } */
