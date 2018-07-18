/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -dp" } */
/* { dg-skip-if "naming registers makes this a code quality test" { *-*-* } { "-O0" } { "" } } */
typedef long long int64_t;
typedef unsigned long long uint64_t;
NOMIPS16 int64_t foo (int64_t x, int64_t y) { return x * y; }
NOMIPS16 uint64_t bar (uint64_t x, uint64_t y) { return x * y; }
/* { dg-final { scan-assembler-times "[concat {\tdmult\t\$[45],\$[45][^\n]+muldi3_r4000\n\tmflo\t\$2\n}]" 2 } } */
