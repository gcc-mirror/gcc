/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -dp -EB -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef unsigned long long uint64_t;
typedef unsigned int uint128_t __attribute__((mode(TI)));
NOMIPS16 uint64_t foo (uint64_t x, uint64_t y) { return ((uint128_t) x * y) >> 64; }
/* { dg-final { scan-assembler "[concat {\tdmultu\t\$[45],\$[45][^\n]+umuldi3_highpart_internal\n\tmfhi\t\$2\n}]" } } */
