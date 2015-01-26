/* PR rtl-optimization/63637 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int a, b, c, d, e, f;
  asm ("# Magic instruction" : "=r" (a), "=r" (d) : "r" (0));
  asm ("# Magic instruction" : "=r" (b), "=r" (e) : "r" (0));
  asm ("# Magic instruction" : "=r" (c), "=r" (f) : "r" (0));
  return a + b + c;
}

/* { dg-final { scan-assembler-times "# Magic instruction" 3 } } */
