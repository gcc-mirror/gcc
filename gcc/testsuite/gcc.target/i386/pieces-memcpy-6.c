/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx2 -mavx -mtune=sandybridge" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 33);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%xmm" 4 } } */
