/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_sve.h>

void f(int *p1, int *p2, unsigned long step, unsigned long end, svbool_t pg) {
    for (unsigned long i = 0; i < end; i += step) {
        svst1(pg, p1, svld1_s32(pg, p2));
        p1 += step;
        p2 += step;
    }
}

// Checking that the induction variables are combined into a single variable,
// which is used for all addressing.
// (ie, theres only one scalar add, rather than 3, and the loads and stores use the
// more complex addressing modes)

/* { dg-final { scan-assembler-not {\tld1w\tz[0-9]+\.s, p[0-9]+/z, \[x[0-9]+\]} } } */
/* { dg-final { scan-assembler-not {\tst1w\tz[0-9]+\.s, p[0-9]+, \[x[0-9]+\]} } } */

/* { dg-final { scan-assembler-times {\tadd\tx[0-9]+, x[0-9]+, x[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-9]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]} 1 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-9]+, \[x[0-9]+, x[0-9]+, lsl 2\]} 1 } } */
