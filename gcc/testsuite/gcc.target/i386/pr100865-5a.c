/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

extern short array[64];

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = -45;
}

/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%xmm\[0-9\]+, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 4 } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
