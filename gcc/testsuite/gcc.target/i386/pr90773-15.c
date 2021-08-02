/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern char *dst;

void
foo (int c)
{
  __builtin_memset (dst, c, 17);
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[\\t \]+%edi, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]+%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movb\[\\t \]+%dil, 16\\(%\[\^,\]+\\)" 1 } } */
