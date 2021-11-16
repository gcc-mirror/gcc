/* { dg-do compile } */
/* { dg-options "-O2 -mtune=sapphirerapids -march=x86-64 -mavx2" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 66);
}

/* { dg-final { scan-assembler-times "vmovdqu(?:8|)\[ \\t\]+\[^\n\]*%ymm" 2 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
