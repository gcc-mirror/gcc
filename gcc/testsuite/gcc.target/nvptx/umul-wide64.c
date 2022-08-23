/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int __attribute ((mode(TI))) uti_t;

uti_t foo(unsigned long x, unsigned long y)
{
  return (uti_t)x * (uti_t)y;
}

/* { dg-final { scan-assembler-times "mul.lo.u64" 1 } } */
/* { dg-final { scan-assembler-times "mul.hi.u64" 1 } } */

