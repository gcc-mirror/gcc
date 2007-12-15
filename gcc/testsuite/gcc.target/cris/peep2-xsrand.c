/* { dg-do compile } */
/* { dg-final { scan-assembler "and.w " } } */
/* { dg-final { scan-assembler "and.b " } } */
/* { dg-final { scan-assembler-not "and.d" } } */
/* { dg-options "-O2" } */

/* Test the "asrandb", "asrandw", "lsrandb" and "lsrandw" peephole2:s
   trivially.  */

unsigned int
andwlsr (unsigned int x)
{
  return (x >> 17) & 0x7ff;
}

unsigned int
andblsr (unsigned int x)
{
  return (x >> 25) & 0x5f;
}

int
andwasr (int x)
{
  return (x >> 17) & 0x7ff;
}

int
andbasr (int x)
{
  return (x >> 25) & 0x5f;
}
