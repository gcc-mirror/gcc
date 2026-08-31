/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbs_zicond -mabi=lp64d -mbranch-cost=4" { target rv64} } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-Os" "-Oz" "-Og" "-flto" } } */

/* Elide a short forward branch and generate a czero instead.  */

#include <stdint.h>

uint64_t updateLSB63 (uint64_t val, uint64_t val2, int bits, int n)
{
  if (val & (1ULL << 63))
    val2 ^= n;
  return val2;
}

/* { dg-final { scan-assembler-times {\tczero} 1 } } */
/* { dg-final { scan-assembler-not {\tbge} } } */

