/* This test requires widening_mul */
/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -dp -EB -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef long long int64_t;
typedef int int128_t __attribute__((mode(TI)));
NOMIPS16 int64_t foo (int64_t x, int64_t y) { return ((int128_t) x * y) >> 64; }
/* { dg-final { scan-assembler "[concat {\tdmult\t\$[45],\$[45][^\n]+smuldi3_highpart_internal\n\tmfhi\t\$2\n}]" } } */
