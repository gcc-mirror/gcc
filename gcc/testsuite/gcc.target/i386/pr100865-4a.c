/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mtune-ctrl=avx256_store_by_pieces" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

extern char array[64];

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = -45;
}

/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%xmm\[0-9\]+, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 2 } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
