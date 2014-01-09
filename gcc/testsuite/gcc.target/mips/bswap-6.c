/* { dg-options "isa_rev>=2 -mgp64" } */

typedef unsigned long long uint64_t;

NOMIPS16 uint64_t
foo (uint64_t x)
{
  return __builtin_bswap64 (x);
}

/* { dg-final { scan-assembler "\tdsbh\t" } } */
/* { dg-final { scan-assembler "\tdshd\t" } } */
