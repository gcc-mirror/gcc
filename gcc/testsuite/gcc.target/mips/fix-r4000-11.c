/* { dg-options "-march=r4000 -mfix-r4000 -mgp64 -O2 -dp" } */
typedef long long int64_t;
NOMIPS16 int64_t foo (int64_t x) { return x / 11993; }
/* { dg-final { scan-assembler "[concat {\tdmult\t\$4,\$[0-9]+[^\n]+smuldi3_highpart[^\n]+\n\tmfhi\t\$[0-9]+\n}]" } } */
