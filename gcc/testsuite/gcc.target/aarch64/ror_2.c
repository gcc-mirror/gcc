/* { dg-options "-O2 --save-temps" } */
/* { dg-do assemble } */


#define ROR(X,Y) ((X >> Y) | (X << (32 - Y)))

unsigned
ror1 (unsigned x)
{
  /* { dg-final { scan-assembler "ror\tw\[0-9\]+, w\[0-9\]+, 3\n" } } */
  return ROR (x, 3);
}

unsigned
ror2 (unsigned x)
{
  /* { dg-final { scan-assembler "ror\tw\[0-9\]+, w\[0-9\]+, 17\n" } } */
  return ROR (x, 17);
}

unsigned long
ror3 (unsigned x)
{
  /* { dg-final { scan-assembler "ror\tw\[0-9\]+, w\[0-9\]+, 2\n" } } */
  return (unsigned long) ROR (x, 2);
}

unsigned long
ror4 (unsigned x)
{
  /* { dg-final { scan-assembler "ror\tw\[0-9\]+, w\[0-9\]+, 26\n" } } */
  return (unsigned long) ROR (x, 26);
}

unsigned
and1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "and\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 13\n" } } */
  return x & ROR (y, 13);
}

unsigned
and2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "and\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 23\n" } } */
  return x & ROR (y, 23);
}

unsigned long
and3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "and\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 12\n" } } */
  return x & (unsigned long) ROR (y, 12);
}

unsigned
bic1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "bic\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 11\n" } } */
  return x & ~ROR (y, 11);
}

unsigned
bic2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "bic\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 17\n" } } */
  return x & ~ROR (y, 17);
}

unsigned long
bic3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "bic\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 22\n" } } */
  return (unsigned long) x & ~ROR (y, 22);
}

unsigned
orr1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "orr\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 5\n" } } */
  return x | ROR (y, 5);
}

unsigned
orr2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "orr\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 25\n" } } */
  return x | ROR (y, 25);
}

unsigned long
orr3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "orr\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 24\n" } } */
  return (unsigned long)x | ROR (y, 24);
}

unsigned
orn1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "orn\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 3\n" } } */
  return x | ~ROR (y, 3);
}

unsigned
orn2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "orn\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 30\n" } } */
  return x | ~ROR (y, 30);
}

unsigned long
orn3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "orn\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 9\n" } } */
  return x | (unsigned long) ~ROR (y, 9);
}

unsigned
eor1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "eor\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 9\n" } } */
  return x ^ ROR (y, 9);
}

unsigned
eor2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "eor\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 31\n" } } */
  return x ^ ROR (y, 31);
}

unsigned long
eor3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "eor\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 28\n" } } */
  return (unsigned long) x ^ ROR (y, 28);
}

unsigned
eon1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "eon\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 1\n" } } */
  return x ^ ~ROR (y, 1);
}

unsigned
eon2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "eon\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 18\n" } } */
  return x ^ ~ROR (y, 18);
}

unsigned long
eon3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "eon\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, ror 19\n" } } */
  return x ^ (unsigned long) ~ROR (y, 19);
}

int
tst1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "tst\tw\[0-9\]+, w\[0-9\]+, ror 8\n" } } */
  return (x & ROR (y, 8)) == 0;
}

int
tst2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "tst\tw\[0-9\]+, w\[0-9\]+, ror 20\n" } } */
  return (x & ROR (y, 20)) == 0;
}

int
tst3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "tst\tw\[0-9\]+, w\[0-9\]+, ror 20\n" } } */
  return ((unsigned long)x & ROR (y, 20)) == 0;
}

int
bics1 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "bics\twzr, w\[0-9\]+, w\[0-9\]+, ror 10\n" } } */
  return (x & ~ROR (y, 10)) == 0;
}

int
bics2 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "bics\twzr, w\[0-9\]+, w\[0-9\]+, ror 21\n" } } */
  return (x & ~ROR (y, 21)) == 0;
}

int
bics3 (unsigned x, unsigned y)
{
  /* { dg-final { scan-assembler "bics\twzr, w\[0-9\]+, w\[0-9\]+, ror 21\n" } } */
  return (x & (unsigned long)~ROR (y, 21)) == 0;
}

/* { dg-final { scan-assembler-not "cmp" } } */
/* { dg-final { scan-assembler-not "mvn" } } */
/* { dg-final { scan-assembler-not "uxtw" } } */
