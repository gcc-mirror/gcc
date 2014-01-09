/* { dg-options "isa_rev>=2 -mgp64" } */
/* { dg-skip-if "bswap recognition needs expensive optimizations" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef unsigned long long uint64_t;

NOMIPS16 uint64_t
foo (uint64_t x)
{
  return (((x << 56) & 0xff00000000000000ull)
	  | ((x << 40) & 0xff000000000000ull)
	  | ((x << 24) & 0xff0000000000ull)
	  | ((x << 8) & 0xff00000000ull)
	  | ((x >> 8) & 0xff000000)
	  | ((x >> 24) & 0xff0000)
	  | ((x >> 40) & 0xff00)
	  | ((x >> 56) & 0xff));
}

/* { dg-final { scan-assembler "\tdsbh\t" } } */
/* { dg-final { scan-assembler "\tdshd\t" } } */
