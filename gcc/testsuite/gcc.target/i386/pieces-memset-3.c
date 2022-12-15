/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512bw -mno-avx512vl -mavx512f -mtune=intel" } */

extern char *dst;

void
foo (int x)
{
  __builtin_memset (dst, x, 66);
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\n\]*%ymm" 1 } } */
/* { dg-final { scan-assembler-times "vinserti64x4\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" { xfail ia32 } } } */
