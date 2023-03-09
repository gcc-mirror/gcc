/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve" } */
#include <arm_sve.h>

void subreg_coalesce5 (
    svbool_t pg, int64_t* base, int n,
    int64_t *in1, int64_t *in2, int64_t*out
)
{
    svint64x2_t result = svld2_s64 (pg, base);

    for (int i = 0; i < n; i += 1) {
        svint64_t v18 = svld1_s64(pg, in1 + i);
        svint64_t v19 = svld1_s64(pg, in2 + i);
        result.__val[0] = svmad_s64_z(pg, v18, v19, result.__val[0]);
        result.__val[1] = svmad_s64_z(pg, v18, v19, result.__val[1]);
    }
    svst2_s64(pg, out, result);
}

/* { dg-final { scan-assembler-not {[ \t]*mov[ \t]*z[0-9]+\.d} } } */
