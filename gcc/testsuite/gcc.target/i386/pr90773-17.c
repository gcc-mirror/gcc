/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 12, 19);
}

/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]+%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "vmovd\[\\t \]+%xmm\[0-9\]+, 15\\(%\[\^,\]+\\)" 1 } } */
