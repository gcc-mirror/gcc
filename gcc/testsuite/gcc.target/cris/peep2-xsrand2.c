/* { dg-do compile } */
/* { dg-final { scan-assembler "and.w -137," } } */
/* { dg-final { scan-assembler "and.b -64," } } */
/* { dg-final { scan-assembler "and.w -139," } } */
/* { dg-final { scan-assembler "and.b -63," } } */
/* { dg-final { scan-assembler-not "and.d" } } */
/* { dg-options "-O2" } */

/* PR target/17984.  Test-case based on
   testsuite/gcc.dg/cris-peep2-xsrand.c.  */

unsigned int
andwlsr (unsigned int x)
{
  return (x >> 16) & 0xff77;
}

unsigned int
andblsr (unsigned int x)
{
  return (x >> 24) & 0xc0;
}

int
andwasr (int x)
{
  return (x >> 16) & 0xff75;
}

int
andbasr (int x)
{
  return (x >> 24) & 0xc1;
}
