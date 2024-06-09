/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -m64 -mapxf" } */
/* { dg-final { scan-assembler-times "cmov(l\.)?e\[^\n\r]*, %eax" 1 } } */
/* { dg-final { scan-assembler-times "cmov(l\.)?ge\[^\n\r]*, %eax" 1 } } */

unsigned int c[4];

unsigned long long foo1 (int a, unsigned int b)
{
  return a ? b : c[1];
}

unsigned int foo3 (int a, int b, unsigned int c, unsigned int d)
{
  return a < b ? c : d;
}
