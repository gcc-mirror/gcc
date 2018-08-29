/* This test requires widening_mul */
/* { dg-options "-mips1 -mfix-r4000 -dp -EB -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef int int32_t;
typedef long long int64_t;
NOMIPS16 int32_t foo (int32_t x, int32_t y) { return ((int64_t) x * y) >> 32; }
/* ??? A highpart pattern would be a better choice, but we currently
   don't use them.  */
/* { dg-final { scan-assembler "[concat {\tmult\t\$[45],\$[45][^\n]+mulsidi3_32bit_r4000\n\tmflo\t\$3\n\tmfhi\t\$2\n}]" } } */
