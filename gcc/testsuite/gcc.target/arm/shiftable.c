/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm32 } */

/* ARM has shift-and-alu insns.  Depending on the ALU op GCC represents some
   of these as a left shift, others as a multiply.  Check that we match the
    right one.  */

int
plus (int a, int b)
{
  return (a * 64) + b;
}

/* { dg-final { scan-assembler "add.*\[al]sl #6" } } */

int
minus (int a, int b)
{
  return a - (b * 64);
}

/* { dg-final { scan-assembler "sub.*\[al]sl #6" } } */

int
ior (int a, int b)
{
  return (a * 64) | b;
}

/* { dg-final { scan-assembler "orr.*\[al]sl #6" } } */

int
xor (int a, int b)
{
  return (a * 64) ^ b;
}

/* { dg-final { scan-assembler "eor.*\[al]sl #6" } } */

int
and (int a, int b)
{
  return (a * 64) & b;
}

/* { dg-final { scan-assembler "and.*\[al]sl #6" } } */

int
rsb (int a, int b)
{
  return (a * 64) - b;
}

/* { dg-final { scan-assembler "rsb.*\[al]sl #6" } } */

int
mvn (int a, int b)
{
  return ~(a * 64);
}

/* { dg-final { scan-assembler "mvn.*\[al]sl #6" } } */
