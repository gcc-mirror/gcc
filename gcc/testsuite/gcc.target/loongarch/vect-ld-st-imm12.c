/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -mlasx -O2" } */
/* { dg-final { scan-assembler-not "addi.d" } } */

extern short a[1000];
extern short b[1000];
extern short c[1000];

void
test (void)
{
  for (int i = 501; i < 517; i++)
    ((int *)(c + 1))[i] = ((int *)(a + 1))[i] + ((int *)(b + 1))[i];
}

