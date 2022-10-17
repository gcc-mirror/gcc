/* { dg-do compile } */
/* { dg-options "-O -mstrict-align" } */

#include <arm_neon.h>

int64_t s64[2] __attribute__((aligned(16)));
float64_t f64[2] __attribute__((aligned(16)));

int64x2_t test_s64() { return (int64x2_t) { s64[0], s64[1] }; }
float64x2_t test_f64() { return (float64x2_t) { f64[0], f64[1] }; }

/* { dg-final { scan-assembler-not {\tins\t} } } */
