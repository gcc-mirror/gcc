/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -O2 -dp" } */
typedef unsigned long long uint64_t;
NOMIPS16 uint64_t foo (uint64_t x) { return x / 11993; }
/* { dg-final { scan-assembler "[concat {\tdmultu\t\$4,\$[0-9]+[^\n]+umuldi3_highpart[^\n]+\n\tmfhi\t\$[0-9]+\n}]" } } */
