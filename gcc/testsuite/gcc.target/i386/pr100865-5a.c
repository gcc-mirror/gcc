/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake" } */

extern short array[64];

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = -45;
}

/* { dg-final { scan-assembler-times "vpbroadcastw\[\\t \]+%xmm\[0-9\]+, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 4 } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
