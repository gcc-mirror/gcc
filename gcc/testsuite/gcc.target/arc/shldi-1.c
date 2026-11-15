/* { dg-do compile } */
/* { dg-options "-O2" } */

long long foo(long long x, int y)
{
  return x << y;
}

/* { dg-final { scan-assembler-not "mov_s" } } */
/* { dg-final { scan-assembler-not "mov.eq" } } */
/* { dg-final { scan-assembler-times "mov.ne" 1 } } */
