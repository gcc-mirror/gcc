/* PR rtl-optimization/63637 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int a, b, c;
  asm ("# Magic instruction" : "=r" (a) : : "eax", "memory");
  asm ("# Magic instruction" : "=r" (b) : : "edx", "memory");
  asm ("# Magic instruction" : "=r" (c) : : "ecx", "memory");
  return a + b + c;
}

/* { dg-final { scan-assembler-times "# Magic instruction" 3 } } */
