/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -dp -EB" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef unsigned long long uint64_t;
typedef unsigned int uint128_t __attribute__((mode(TI)));
NOMIPS16 uint64_t foo (uint64_t x, uint64_t y) { return ((uint128_t) x * y) >> 64; }
/* ??? A highpart pattern would be a better choice, but we currently
   don't use them.  */
/* { dg-final { scan-assembler "[concat {\tdmultu\t\$[45],\$[45][^\n]+umulditi3_r4000\n\tmflo\t\$3\n\tmfhi\t\$2\n}]" } } */
