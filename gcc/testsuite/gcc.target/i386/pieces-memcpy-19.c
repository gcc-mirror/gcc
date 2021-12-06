/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids -mmove-max=128 -mstore-max=128" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 66);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%xmm" 8 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
