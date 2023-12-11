/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d -mexplicit-relocs=auto" } */

float a[8001];
float
t (void)
{
  return a[0] + a[8000];
}

/* { dg-final { scan-assembler-not "la.local" } } */
