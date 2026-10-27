/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64 -O2" } */

unsigned int
f1 (unsigned int a, unsigned int b, unsigned int c)
{
  return (a * 2 + b) / c;
}

int
f2 (int a, int b, int c)
{
  return (a * 4 + b) % c;
}

unsigned int
f3 (unsigned int a, unsigned int b, unsigned int c)
{
  return (a * 8 + b) % c;
}

/* { dg-final { scan-assembler-times "sh1add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh2add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh3add\t" 1 } } */
/* { dg-final { scan-assembler-times "divuw\t" 1 } } */
/* { dg-final { scan-assembler-times "remw\t" 1 } } */
/* { dg-final { scan-assembler-times "remuw\t" 1 } } */
