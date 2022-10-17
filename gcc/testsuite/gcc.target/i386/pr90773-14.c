/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 1, 20);
}

/* { dg-final { scan-assembler-times "movups\[\\t \]+%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movd\[\\t \]+%xmm\[0-9\]+, 16\\(%\[\^,\]+\\)" 1 { xfail *-*-* } } } */
