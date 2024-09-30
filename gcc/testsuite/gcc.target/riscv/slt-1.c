/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

#include <stdint.h>

#define COMPARISON(TYPE, OP, OPN, RESULT_TRUE, RESULT_FALSE) \
    TYPE test_##OPN(TYPE x, TYPE y) { \
        return (x OP y) ? RESULT_TRUE : RESULT_FALSE; \
    }

/* Signed comparisons */
COMPARISON(int64_t, >, GT1, 2, 3)
COMPARISON(int64_t, >, GT2, 5, 6)

COMPARISON(int64_t, <, LT1, 2, 3)
COMPARISON(int64_t, <, LT2, 5, 6)

COMPARISON(int64_t, >=, GE1, 3, 2)
COMPARISON(int64_t, >=, GE2, 6, 5)

COMPARISON(int64_t, <=, LE1, 3, 2)
COMPARISON(int64_t, <=, LE2, 6, 5)

/* Unsigned comparisons */
COMPARISON(uint64_t, >, GTU1, 2, 3)
COMPARISON(uint64_t, >, GTU2, 5, 6)

COMPARISON(uint64_t, <, LTU1, 2, 3)
COMPARISON(uint64_t, <, LTU2, 5, 6)

COMPARISON(uint64_t, >=, GEU1, 3, 2)
COMPARISON(uint64_t, >=, GEU2, 6, 5)

COMPARISON(uint64_t, <=, LEU1, 3, 2)
COMPARISON(uint64_t, <=, LEU2, 6, 5)

#define COMPARISON_IMM(TYPE, OP, OPN, RESULT_TRUE, RESULT_FALSE) \
    TYPE testIMM_##OPN(TYPE x) { \
        return (x OP -3) ? RESULT_TRUE : RESULT_FALSE; \
    }

/* Signed comparisons with immediate */
COMPARISON_IMM(int64_t, >, GT1, 3, 2)

COMPARISON_IMM(int64_t, <, LT1, 2, 3)

COMPARISON_IMM(int64_t, >=, GE1, 3, 2)

COMPARISON_IMM(int64_t, <=, LE1, 2, 3)

/* { dg-final { scan-assembler-times "sgt\\t" 4 } } */
/* { dg-final { scan-assembler-times "sgtu\\t" 4 } } */
/* { dg-final { scan-assembler-times "slt\\t" 4 } } */
/* { dg-final { scan-assembler-times "sltu\\t" 4 } } */
/* { dg-final { scan-assembler-times "slti\\t" 4 } } */
/* { dg-final { scan-assembler-times "xori\\ta0,a0,1" 8 } } */
/* { dg-final { scan-assembler-times "xori\\ta0,a0,3" 12 } } */

