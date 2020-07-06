/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x, int y, int z)
{
  return x + y + z;
}

unsigned int bar(unsigned int x, unsigned int y, unsigned int z)
{
  return x + y + z;
}

/* { dg-final { scan-assembler-times "vadd.u32.u32.u32.add" 2 } } */

