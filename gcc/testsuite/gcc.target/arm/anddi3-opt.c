/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned long long
muld (unsigned long long X, unsigned long long Y)
{
  unsigned long long mask = 0xffffffffull;
  return (X & mask) * (Y & mask);
}

/* { dg-final { scan-assembler-not "and\[\\t \]+.+,\[\\t \]*.+,\[\\t \]*.+" } } */
