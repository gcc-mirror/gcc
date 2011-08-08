/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

double func(unsigned long long x)
{
  if (x <= 0x7ffffffffffffffeULL)
    return (x + 1) * 0.01;
  return 0.0;
}

/* { dg-final { scan-assembler-times "cvtsi2sdq" 1 } } */
