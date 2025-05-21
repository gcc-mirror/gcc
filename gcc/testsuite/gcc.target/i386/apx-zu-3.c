/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-times "setzune" 1} } */

char foo0 (int a)
{
  return a == 0 ? 0 : 1;
}
