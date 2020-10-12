/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\tcmp|\ttest} } } */

extern void foo(void);
unsigned int x (unsigned int a, unsigned int b, unsigned int *c)
{
  unsigned int y = a & 15;
  unsigned int z = y + b;
  if (y == 0)
    *c = z;
  return z;
}
