/* This test requires widening_mul */
/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -dp -EB -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef long long int64_t;
typedef int int128_t __attribute__((mode(TI)));
NOMIPS16 int64_t foo (int64_t x, int64_t y) { return ((int128_t) x * y) >> 64; }
/* ??? A highpart pattern would be a better choice, but we currently
   don't use them.  */
/* { dg-final { scan-assembler "[concat {\tdmult\t\$[45],\$[45][^\n]+mulditi3_r4000\n\tmflo\t\$3\n\tmfhi\t\$2\n}]" } } */
