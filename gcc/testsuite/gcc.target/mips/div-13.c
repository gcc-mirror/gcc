/* { dg-options "(-mips16) -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int int32_t;
typedef unsigned int uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;

MIPS16 int32_t f1 (int32_t x, int32_t y) { return x / y + x % y; }
MIPS16 uint32_t f2 (uint32_t x, uint32_t y) { return x / y + x % y; }
MIPS16 int64_t f3 (int64_t x, int64_t y) { return x / y + x % y; }
MIPS16 uint64_t f4 (uint64_t x, uint64_t y) { return x / y + x % y; }

/* { dg-final { scan-assembler-times "\tdiv\t" 1 } } */
/* { dg-final { scan-assembler-times "\tdivu\t" 1 } } */
/* { dg-final { scan-assembler-times "\tddiv\t" 1 } } */
/* { dg-final { scan-assembler-times "\tddivu\t" 1 } } */
