/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, -1, 17);
}

/* { dg-final { scan-assembler-times "(?:vpcmpeqd|vpternlogd)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[\\t \]+%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movb\[\\t \]+\\\$-1, 16\\(%\[\^,\]+\\)" 1 } } */
