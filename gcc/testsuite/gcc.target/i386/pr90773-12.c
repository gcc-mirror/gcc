/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=skylake" } */

void
foo (char *dst, char *src)
{
  __builtin_memcpy (dst, src, 255);
}

/* { dg-final { scan-assembler-times "movdqu\[\\t \]+\[0-9\]*\\(%\[\^,\]+\\)," 16 } } */
/* { dg-final { scan-assembler-not "mov\[bwlq\]" } } */
