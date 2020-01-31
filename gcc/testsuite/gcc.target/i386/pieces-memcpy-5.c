/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f -mtune=generic" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 19);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%xmm" 2 } } */
