/* { dg-options "-mips1 -mfix-r4000 -dp -EB -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
NOMIPS16 uint32_t foo (uint32_t x, uint32_t y) { return ((uint64_t) x * y) >> 32; }
/* { dg-final { scan-assembler "[concat {\tmultu\t\$[45],\$[45][^\n]+umulsi3_highpart_internal\n\tmfhi\t\$2\n}]" } } */
