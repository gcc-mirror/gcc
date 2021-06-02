/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake" } */

extern char array[64];

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = -45;
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[\\t \]+%xmm\[0-9\]+, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%xmm\[0-9\]+, " 4 } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
