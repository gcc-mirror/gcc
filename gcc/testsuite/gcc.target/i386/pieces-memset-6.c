/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx512f -mtune=intel" } */

extern char *dst;

void
foo (int x)
{
  __builtin_memset (dst, x, 33);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" { target { ! ia32 } } } } */
