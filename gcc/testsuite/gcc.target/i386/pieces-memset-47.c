/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids -mstore-max=128" } */
/* { dg-warning ".-mstore-max=. is deprecated; use .-mmove-max=. instead" "" { target *-*-* } 0 } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 66);
}

/* { dg-final { scan-assembler-times "vmovdqu(?:8|)\[ \\t\]+\[^\n\]*%xmm" 4 } } */
/* { dg-final { scan-assembler-times "vmovw\[ \\t\]+\[^\n\]*%xmm" 1 { xfail *-*-* } } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
