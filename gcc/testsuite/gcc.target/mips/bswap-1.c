/* { dg-options "isa_rev>=2" } */
/* { dg-skip-if "bswap recognition needs expensive optimizations" { *-*-* } { "-O0" "-O1" } { "" } } */

NOMIPS16 unsigned short
foo (unsigned short x)
{
  return ((x << 8) & 0xff00) | ((x >> 8) & 0xff);
}

/* { dg-final { scan-assembler "\twsbh\t" } } */
