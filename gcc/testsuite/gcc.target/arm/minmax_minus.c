/* { dg-do compile } */
/* { dg-options "-O2" } */

#define MAX(a, b) (a > b ? a : b)
int
foo (int a, int b, int c)
{
  return c - MAX (a, b);
}

/* { dg-final { scan-assembler "rsbge" } } */
/* { dg-final { scan-assembler "rsblt" } } */
