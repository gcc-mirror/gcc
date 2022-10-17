/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -mno-stackrealign" } */

extern char *dst;

void
foo (int x)
{
  __builtin_memset (dst, x, 64);
}

/* { dg-final { scan-assembler-times "movups\[ \\t\]+\[^\n\]*%xmm" 4 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
