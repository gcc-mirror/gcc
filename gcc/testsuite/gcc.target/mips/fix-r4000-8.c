/* { dg-options "-march=r4000 -mfix-r4000 -O2 -mgp64 -dp -EB" } */
typedef unsigned long long uint64_t;
typedef unsigned int uint128_t __attribute__((mode(TI)));
uint64_t foo (uint64_t x, uint64_t y) { return ((uint128_t) x * y) >> 64; }
/* ??? A highpart pattern would be a better choice, but we currently
   don't use them.  */
/* { dg-final { scan-assembler "[concat {\tdmultu\t\$[45],\$[45][^\n]+umulditi3[^\n]+\n\tmflo\t\$3\n\tmfhi\t\$2\n}]" } } */
