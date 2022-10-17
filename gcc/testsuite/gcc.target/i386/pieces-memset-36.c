/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -mavx2 -mtune=generic" } */

extern char *dst;

void
foo (int x)
{
  __builtin_memset (dst, x, 17);
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
