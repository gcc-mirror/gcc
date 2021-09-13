/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=skylake" } */

void
foo (char *dst)
{
  __builtin_memset (dst, 0, 255);
}

/* { dg-final { scan-assembler-times "movups\[\\t \]+%xmm\[0-9\]+, \[0-9\]*\\(%\[\^,\]+\\)" 16 } } */
/* { dg-final { scan-assembler-not "mov\[bwlq\]" } } */
