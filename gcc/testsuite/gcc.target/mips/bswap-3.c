/* { dg-options "isa_rev>=2" } */
/* { dg-skip-if "bswap recognition needs expensive optimizations" { *-*-* } { "-O0" "-O1" } { "" } } */

NOMIPS16 unsigned int
foo (unsigned int x)
{
  return (((x << 24) & 0xff000000)
	  | ((x << 8) & 0xff0000)
	  | ((x >> 8) & 0xff00)
	  | ((x >> 24) & 0xff));
}

/* { dg-final { scan-assembler "\twsbh\t" } } */
/* { dg-final { scan-assembler "\tror\t" } } */
