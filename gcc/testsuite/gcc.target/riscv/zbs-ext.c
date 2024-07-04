/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */
typedef unsigned long uint64_t;
typedef unsigned int uint32_t;

uint64_t bclr (const uint32_t i)
{
  uint64_t checks = 10;
  checks &= ~(1U << i);
  return checks;
}

/* { dg-final { scan-assembler-times "bclr\t" 1 } } */
/* { dg-final { scan-assembler-not "sllw\t"} } */
