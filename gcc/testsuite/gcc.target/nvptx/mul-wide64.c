/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int __attribute ((mode(TI))) ti_t;

ti_t foo(long x, long y)
{
  return (ti_t)x * (ti_t)y;
}

/* { dg-final { scan-assembler-times "mul.lo.u64" 1 } } */
/* { dg-final { scan-assembler-times "mul.hi.s64" 1 } } */

