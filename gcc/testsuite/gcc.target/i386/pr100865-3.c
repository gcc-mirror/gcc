/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 16);
}

/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastd\[\\t \]+%xmm\[0-9\]+, %xmm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
