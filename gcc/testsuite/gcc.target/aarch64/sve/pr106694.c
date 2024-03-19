/* { dg-options "-Ofast" } */

#include "arm_sve.h"

int coalesce (svbool_t pg, int64_t* base, int n, int64_t *in1, int64_t *in2, int64_t*out)
{
  svint64x4_t result = svld4_s64 (pg, base);
  svint64_t v0 = svget4_s64(result, 0);
  svint64_t v1 = svget4_s64(result, 1);
  svint64_t v2 = svget4_s64(result, 2);
  svint64_t v3 = svget4_s64(result, 3);

  for (int i = 0; i < n; i += 1)
    {
        svint64_t v18 = svld1_s64(pg, in1);
        svint64_t v19 = svld1_s64(pg, in2);
        v0 = svmad_s64_z(pg, v0, v18, v19);
        v1 = svmad_s64_z(pg, v1, v18, v19);
        v2 = svmad_s64_z(pg, v2, v18, v19);
        v3 = svmad_s64_z(pg, v3, v18, v19);
    }
  svst1_s64(pg, out+0,v0);
  svst1_s64(pg, out+1,v1);
  svst1_s64(pg, out+2,v2);
  svst1_s64(pg, out+3,v3);
}

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
