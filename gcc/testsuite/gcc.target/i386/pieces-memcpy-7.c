/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */

void
foo (int a1, int a2, int a3, int a4, int a5, int a6, char *dst, char *src)
{
  __builtin_memcpy (dst, src, 17);
}

/* { dg-final { scan-assembler-times "movdqu\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "movups\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
