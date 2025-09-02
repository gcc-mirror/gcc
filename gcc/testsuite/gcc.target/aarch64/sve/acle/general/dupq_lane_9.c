/* { dg-options "-O2 -mbig-endian" } */

#pragma GCC aarch64 "arm_sve.h"

svint32_t f(svint32_t x) { return svdupq_lane (x, 17); }
void g(svint32_t *a, svint32_t *b) { *a = svdupq_lane (*b, 17); }

/* { dg-final { scan-assembler-not {\trevw\t} } } */
