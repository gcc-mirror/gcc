/* { dg-options "isa_rev>=2" } */

NOMIPS16 unsigned short
foo (unsigned short x)
{
  return __builtin_bswap16 (x);
}

/* { dg-final { scan-assembler "\twsbh\t" } } */
