/* { dg-do compile { target lp64 } } */
/* { dg-options "-O3 -fno-vect-cost-model -msve-vector-bits=256 -mautovec-preference=sve-only" } */

int x[8] __attribute__ ((aligned (64)));

void
foo (int *restrict b, unsigned int n)
{
  for (unsigned int i = 1; i < n; ++i)
    x[i] = b[i * 650000000L];
}

/* Peeling for alignment by masking makes lane 7 active; 7 * 650000000
   does not fit in 32 bits.  */
/* { dg-final { scan-assembler-not {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, [su]xtw} } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+\.d\]} 2 } } */
