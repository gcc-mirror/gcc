/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x, int y, int z)
{
  return (x - y) + z;
}

int bar(int x, int y, int z)
{
  return x + (y - z);
}

unsigned int ufoo(unsigned int x, unsigned int y, unsigned int z)
{
  return (x - y) + z;
}

unsigned int ubar(unsigned int x, unsigned int y, unsigned int z)
{
  return x + (y - z);
}

/* { dg-final { scan-assembler-times "vsub.u32.u32.u32.add" 4 } } */

