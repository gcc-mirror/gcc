/* { dg-options "-O2 --save-temps" } */
/* { dg-do assemble } */


#define ROR(X,Y) ((X >> Y) | (X << (64 - Y)))

unsigned long
ror1 (unsigned long x)
{
  /* { dg-final { scan-assembler "ror\tx\[0-9\]+, x\[0-9\]+, 3\n" } } */
  return ROR (x, 3);
}

unsigned long
ror2 (unsigned long x)
{
  /* { dg-final { scan-assembler "ror\tx\[0-9\]+, x\[0-9\]+, 37\n" } } */
  return ROR (x, 37);
}

unsigned long
and1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "and\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 13\n" } } */
  return x & ROR (y, 13);
}

unsigned long
and2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "and\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 33\n" } } */
  return x & ROR (y, 33);
}

unsigned long
bic1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "bic\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 11\n" } } */
  return x & ~ROR (y, 11);
}

unsigned long
bic2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "bic\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 47\n" } } */
  return x & ~ROR (y, 47);
}

unsigned long
orr1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "orr\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 5\n" } } */
  return x | ROR (y, 5);
}

unsigned long
orr2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "orr\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 35\n" } } */
  return x | ROR (y, 35);
}

unsigned long
orn1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "orn\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 3\n" } } */
  return x | ~ROR (y, 3);
}

unsigned long
orn2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "orn\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 39\n" } } */
  return x | ~ROR (y, 39);
}

unsigned long
eor1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "eor\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 9\n" } } */
  return x ^ ROR (y, 9);
}

unsigned long
eor2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "eor\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 41\n" } } */
  return x ^ ROR (y, 41);
}

unsigned long
eon1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "eon\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 1\n" } } */
  return x ^ ~ROR (y, 1);
}

unsigned long
eon2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "eon\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, ror 38\n" } } */
  return x ^ ~ROR (y, 38);
}

unsigned long
tst1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "tst\tx\[0-9\]+, x\[0-9\]+, ror 8\n" } } */
  return (x & ROR (y, 8)) == 0;
}

unsigned long
tst2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "tst\tx\[0-9\]+, x\[0-9\]+, ror 50\n" } } */
  return (x & ROR (y, 50)) == 0;
}

unsigned long
bics1 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "bics\txzr, x\[0-9\]+, x\[0-9\]+, ror 10\n" } } */
  return (x & ~ROR (y, 10)) == 0;
}

unsigned long
bics2 (unsigned long x, unsigned long y)
{
  /* { dg-final { scan-assembler "bics\txzr, x\[0-9\]+, x\[0-9\]+, ror 62\n" } } */
  return (x & ~ROR (y, 62)) == 0;
}
