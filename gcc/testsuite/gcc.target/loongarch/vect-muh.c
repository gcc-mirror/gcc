/* { dg-do compile } */
/* { dg-options "-mlasx -O3" } */
/* { dg-final { scan-assembler "\tvmuh\.w\t" } } */
/* { dg-final { scan-assembler "\tvmuh\.wu\t" } } */
/* { dg-final { scan-assembler "\txvmuh\.w\t" } } */
/* { dg-final { scan-assembler "\txvmuh\.wu\t" } } */

int a[8], b[8], c[8];

void
test1 (void)
{
  for (int i = 0; i < 4; i++)
    c[i] = ((long)a[i] * (long)b[i]) >> 32;
}

void
test2 (void)
{
  for (int i = 0; i < 4; i++)
    c[i] = ((long)(unsigned)a[i] * (long)(unsigned)b[i]) >> 32;
}

void
test3 (void)
{
  for (int i = 0; i < 8; i++)
    c[i] = ((long)a[i] * (long)b[i]) >> 32;
}

void
test4 (void)
{
  for (int i = 0; i < 8; i++)
    c[i] = ((long)(unsigned)a[i] * (long)(unsigned)b[i]) >> 32;
}
