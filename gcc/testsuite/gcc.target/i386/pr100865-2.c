/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 16);
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[\\t \]+%xmm\[0-9\]+, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
