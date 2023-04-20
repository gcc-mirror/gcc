/* { dg-do compile } */
/* { dg-final { scan-assembler "movu.w " } } */
/* { dg-final { scan-assembler "movu.b " } } */
/* { dg-final { scan-assembler-not "and.. " } } */
/* { dg-options "-O2" } */

/* Test the "movulsrb", "movulsrw" peephole2:s trivially.  */

unsigned int
movulsrb (unsigned y, unsigned int x)
{
  return (x & 255) >> 1;
}

unsigned int
movulsrw (unsigned y, unsigned int x)
{
  return (x & 65535) >> 4;
}
