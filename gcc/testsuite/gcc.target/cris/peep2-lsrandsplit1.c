/* { dg-do compile } */
/* { dg-final { scan-assembler-not " and" } } */
/* { dg-final { scan-assembler-times "lsrq " 2 } } */
/* { dg-final { scan-assembler-times "lslq " 2 } } */
/* { dg-options "-O2" } */

/* Test the "lsrlsllsr1" peephole2 trivially.  */

unsigned int
andwlsr (unsigned int x)
{
  return (x >> 17) & 0x7ff;
}

int
andwasr (int x)
{
  return (x >> 17) & 0x7ff;
}
