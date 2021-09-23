/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake" } */

extern long long int array[64];

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = -45;
}

/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+\[^\n\]*, %ymm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 16 } } */
/* { dg-final { scan-assembler-not "vpbroadcastq" { target ia32 } } } */
/* { dg-final { scan-assembler-not "vmovdqa" { target { ! ia32 } } } } */
