/* { dg-options "-march=r4000 -mfix-r4000 -O2 -dp" } */
typedef int int32_t;
typedef int uint32_t;
int32_t foo (int32_t x, int32_t y) { return x * y; }
uint32_t bar (uint32_t x, uint32_t y) { return x * y; }
/* { dg-final { scan-assembler-times "[concat {\tmult\t\$[45],\$[45][^\n]+mulsi3_r4000[^\n]+\n\tmflo\t\$2\n}]" 2 } } */
