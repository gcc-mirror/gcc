/* { dg-do compile } */
/* { dg-options "-O2 -march=alderlake" } */

extern char *dst;

void
foo (int x)
{
  __builtin_memset (dst, x, 64);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 2 } } */
