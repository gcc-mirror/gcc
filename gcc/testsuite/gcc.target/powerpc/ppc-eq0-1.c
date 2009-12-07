/* PR rtl-optimization/10588 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x)
{
  return x == 0;
}

/* { dg-final { scan-assembler "cntlzw|isel" } } */
