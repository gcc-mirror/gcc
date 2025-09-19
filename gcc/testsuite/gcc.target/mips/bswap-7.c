/* { dg-options "-march=allegrex" } */

NOMIPS16 unsigned int
foo (unsigned int x)
{
  return __builtin_bswap32 (x);
}

/* { dg-final { scan-assembler "\twsbw\t" } } */
