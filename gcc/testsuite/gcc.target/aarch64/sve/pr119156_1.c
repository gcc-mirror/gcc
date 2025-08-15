/* { dg-options "-O2" } */

#include <arm_sve.h>

void inner_loop_029(double *restrict input, int64_t *restrict scale,
                           double *restrict output, int64_t size) {
    svbool_t p;
    int64_t i = 0;
    while (p = svwhilelt_b64(i, size), svptest_first(svptrue_b64(), p)) {
        svst1(p, output+i, svld1(p, input+i));
        i += svcntd();
    }
}

/* { dg-final { scan-assembler-not {\tptest\t} } } */
