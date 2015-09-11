/* PR rtl-optimization/63637 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int a, b, c, d, e, f;
  asm ("# Magic instruction" : "=r" (a), "=r" (d) : : "eax");
  asm ("# Magic instruction" : "=r" (b), "=r" (e) : : "edx");
  asm ("# Magic instruction" : "=r" (c), "=r" (f) : : "ecx");
  return a + b + c;
}

/* { dg-final { scan-assembler-times "# Magic instruction" 3 } } */
