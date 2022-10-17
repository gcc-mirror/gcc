/* This test requires widening_mul */
/* { dg-options "-mips1 -mfix-r4000 -dp -EB -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef int int32_t;
typedef long long int64_t;
NOMIPS16 int32_t foo (int32_t x, int32_t y) { return ((int64_t) x * y) >> 32; }
/* { dg-final { scan-assembler "[concat {\tmult\t\$[45],\$[45][^\n]+smulsi3_highpart_internal\n\tmfhi\t\$2\n}]" } } */
