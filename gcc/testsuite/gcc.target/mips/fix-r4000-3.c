/* { dg-options "-mips1 -mfix-r4000 -O2 -dp -EB" } */
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
NOMIPS16 uint32_t foo (uint32_t x, uint32_t y) { return ((uint64_t) x * y) >> 32; }
/* ??? A highpart pattern would be a better choice, but we currently
   don't use them.  */
/* { dg-final { scan-assembler "[concat {\tmultu\t\$[45],\$[45][^\n]+umulsidi3_32bit_r4000[^\n]+\n\tmflo\t\$3\n\tmfhi\t\$2\n}]" } } */
