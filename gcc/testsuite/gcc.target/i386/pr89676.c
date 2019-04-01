/* PR rtl-optimization/89676 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-stv" } */

unsigned long long
foo (unsigned long long i)
{
  return i << 3;
}

/* { dg-final { scan-assembler-times "movl" 2 } } */
