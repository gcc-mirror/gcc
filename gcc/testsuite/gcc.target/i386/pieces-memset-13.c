/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 33);
}

/* { dg-final { scan-assembler-times "movups\[ \\t\]+\[^\n\]*%xmm" 2 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
