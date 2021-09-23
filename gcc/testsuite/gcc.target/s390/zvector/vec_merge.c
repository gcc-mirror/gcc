/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */

/* { dg-final { scan-assembler-times "\tvmrhb\t" 2 } } */
/* { dg-final { scan-assembler-times "\tvmrlb\t" 2 } } */
/* { dg-final { scan-assembler-times "\tvmrhh\t" 2 } } */
/* { dg-final { scan-assembler-times "\tvmrlh\t" 2 } } */
/* { dg-final { scan-assembler-times "\tvmrhf\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvmrlf\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvmrhg\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvmrlg\t" 3 } } */

#include "vec-types.h"
#include <vecintrin.h>

#define GEN_MERGE(VEC_TYPE, HILO)					\
  VEC_TYPE __attribute__((noinline))					\
  merge_##HILO##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {			\
    return vec_merge##HILO (a, b); }

GEN_MERGE(v16qi, l)
GEN_MERGE(v16qi, h)
GEN_MERGE(uv16qi, l)
GEN_MERGE(uv16qi, h)

GEN_MERGE(v8hi, l)
GEN_MERGE(v8hi, h)
GEN_MERGE(uv8hi, l)
GEN_MERGE(uv8hi, h)

GEN_MERGE(v4si, l)
GEN_MERGE(v4si, h)
GEN_MERGE(uv4si, l)
GEN_MERGE(uv4si, h)

GEN_MERGE(v4sf, l)
GEN_MERGE(v4sf, h)

GEN_MERGE(v2di, l)
GEN_MERGE(v2di, h)
GEN_MERGE(uv2di, l)
GEN_MERGE(uv2di, h)

GEN_MERGE(v2df, l)
GEN_MERGE(v2df, h)


#define CHECK_MERGE_LO(VEC_TYPE, SRC1, SRC2)				\
  {									\
    VEC_TYPE v = merge_l_##VEC_TYPE ((SRC1), (SRC2));			\
    int elts = sizeof(v) / sizeof(v[0]);				\
    for (int i = 0; i < elts; i++)					\
      if (v[i] != (i + elts) / 2 + (i % 2) * elts)			\
	__builtin_abort();						\
  }

#define CHECK_MERGE_HI(VEC_TYPE, SRC1, SRC2)				\
  {									\
    VEC_TYPE v = merge_h_##VEC_TYPE ((SRC1), (SRC2));			\
    int elts = sizeof(v) / sizeof(v[0]);				\
    for (int i = 0; i < elts; i++)					\
      if (v[i] != i / 2 + (i % 2) * elts)				\
	__builtin_abort();						\
  }

#define CHECK_MERGE(VEC_TYPE)						\
  {									\
    VEC_TYPE a = GEN_SEQ_VEC (VEC_TYPE, 0);				\
    VEC_TYPE b = GEN_SEQ_VEC (VEC_TYPE, sizeof(VEC_TYPE) / sizeof(a[0])); \
    CHECK_MERGE_LO (VEC_TYPE, a, b);					\
    CHECK_MERGE_HI (VEC_TYPE, a, b);					\
  }

int
main ()
{
  CHECK_MERGE(v16qi);
  CHECK_MERGE(uv16qi);
  CHECK_MERGE(v8hi);
  CHECK_MERGE(uv8hi);
  CHECK_MERGE(v4si);
  CHECK_MERGE(uv4si);
  CHECK_MERGE(v4sf);
  CHECK_MERGE(v2di);
  CHECK_MERGE(uv2di);
  CHECK_MERGE(v2df);
}
