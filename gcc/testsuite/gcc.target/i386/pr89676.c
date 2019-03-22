/* PR rtl-optimization/89676 */
/* { dg-do compile } */
/* { dg-options "-O2 -m32 -march=i686" } */
unsigned long long
foo (unsigned long long i)
{
  return i << 3;
}

/* { dg-final { scan-assembler-times "movl" 2 } } */
