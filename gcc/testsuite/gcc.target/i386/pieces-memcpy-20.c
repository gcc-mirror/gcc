/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids -mmove-max=256 -mstore-max=256" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 66);
}

/* { dg-final { scan-assembler-times "vmovdqu(?:64|)\[ \\t\]+\[^\n\]*%ymm" 4 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
