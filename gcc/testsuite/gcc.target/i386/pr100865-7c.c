/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake -mno-avx2" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

extern long long int array[64];

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = -45;
}

/* { dg-final { scan-assembler-times "vbroadcastsd" 1  } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 16 } } */
/* { dg-final { scan-assembler-not "vmovdqa" { target { ! ia32 } } } } */
