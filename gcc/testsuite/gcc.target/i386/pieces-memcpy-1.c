/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 64);
}

/* { dg-final { scan-assembler-times "movdqu\[ \\t\]+\[^\n\]*%xmm" 4 } } */
/* { dg-final { scan-assembler-times "movups\[ \\t\]+\[^\n\]*%xmm" 4 } } */
