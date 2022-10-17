/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx512f -mtune=generic" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 0, 33);
}

/* { dg-final { scan-assembler-times "vpxor\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
