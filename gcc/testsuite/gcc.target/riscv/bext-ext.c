/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */
typedef unsigned long uint64_t;
typedef unsigned int uint32_t;

uint64_t bext1 (int dst, const uint32_t i)
{
  uint64_t checks = 1U;
  checks &= dst >> i;
  return checks;
}

int bext2 (int dst, int i_denom)
{
  dst = 1 & (dst >> i_denom);
  return dst;
}

const uint32_t bext3 (uint32_t bit_count, uint32_t symbol)
{
  return (symbol >> bit_count) & 1;
}

/* { dg-final { scan-assembler-times "bext\t" 3 } } */
/* { dg-final { scan-assembler-not "sllw\t"} } */
/* { dg-final { scan-assembler-not "srlw\t"} } */
