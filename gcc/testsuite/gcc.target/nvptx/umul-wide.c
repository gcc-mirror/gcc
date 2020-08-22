/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int umulhisi3(unsigned short x, unsigned short y)
{
  return (unsigned int)x * (unsigned int)y;
}

unsigned long umulsidi3(unsigned int x, unsigned int y)
{
  return (unsigned long)x * (unsigned long)y;
}

/* { dg-final { scan-assembler-times "mul.wide.u16" 1 } } */
/* { dg-final { scan-assembler-times "mul.wide.u32" 1 } } */

