/* { dg-options "-march=r4000 -mfix-r4000 -dp" } */
/* { dg-skip-if "naming registers makes this a code quality test" { *-*-* } { "-O0" } { "" } } */
typedef int int32_t;
typedef int uint32_t;
NOMIPS16 int32_t foo (int32_t x, int32_t y) { return x * y; }
NOMIPS16 uint32_t bar (uint32_t x, uint32_t y) { return x * y; }
/* { dg-final { scan-assembler-times "[concat {\tmult\t\$[45],\$[45][^\n]+mulsi3_r4000[^\n]+\n\tmflo\t\$2\n}]" 2 } } */
