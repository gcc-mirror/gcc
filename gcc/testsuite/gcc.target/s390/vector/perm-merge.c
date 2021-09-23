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

#define GEN_MERGE_2(VEC_TYPE, HILO, A)			\
  VEC_TYPE __attribute__((noinline))			\
  merge_##HILO##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {	\
    return (VEC_TYPE){ a[0+A], b[0+A] }; }

#define GEN_MERGE_4(VEC_TYPE, HILO, A)				\
  VEC_TYPE __attribute__((noinline))				\
  merge_##HILO##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {		\
    return (VEC_TYPE){ a[0+A], b[0+A], a[1+A], b[1+A] }; }

#define GEN_MERGE_8(VEC_TYPE, HILO, A)					\
  VEC_TYPE __attribute__((noinline))					\
  merge_##HILO##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {			\
    return (VEC_TYPE){ a[0+A], b[0+A], a[1+A], b[1+A], a[2+A], b[2+A], a[3+A], b[3+A] }; }

#define GEN_MERGE_16(VEC_TYPE, HILO, A)					\
  VEC_TYPE __attribute__((noinline))					\
  merge_##HILO##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {			\
    return (VEC_TYPE){ a[0+A], b[0+A], a[1+A], b[1+A], a[2+A], b[2+A], a[3+A], b[3+A], \
      a[4+A], b[4+A], a[5+A], b[5+A], a[6+A], b[6+A], a[7+A], b[7+A]}; }


GEN_MERGE_16(v16qi, l, 8)
GEN_MERGE_16(v16qi, h, 0)
GEN_MERGE_16(uv16qi, l, 8)
GEN_MERGE_16(uv16qi, h, 0)

GEN_MERGE_8(v8hi, l, 4)
GEN_MERGE_8(v8hi, h, 0)
GEN_MERGE_8(uv8hi, l, 4)
GEN_MERGE_8(uv8hi, h, 0)

GEN_MERGE_4(v4si, l, 2)
GEN_MERGE_4(v4si, h, 0)
GEN_MERGE_4(uv4si, l, 2)
GEN_MERGE_4(uv4si, h, 0)

GEN_MERGE_4(v4sf, l, 2)
GEN_MERGE_4(v4sf, h, 0)

GEN_MERGE_2(v2di, l, 1)
GEN_MERGE_2(v2di, h, 0)
GEN_MERGE_2(uv2di, l, 1)
GEN_MERGE_2(uv2di, h, 0)

GEN_MERGE_2(v2df, l, 1)
GEN_MERGE_2(v2df, h, 0)


#define CHECK_MERGE_LO(VEC_TYPE, SRC1, SRC2)		\
  {							\
    VEC_TYPE v = merge_l_##VEC_TYPE ((SRC1), (SRC2));	\
    int elts = sizeof(v) / sizeof(v[0]);		\
    for (int i = 0; i < elts; i++)			\
      if (v[i] != (i + elts) / 2 + (i % 2) * elts)	\
	__builtin_abort();				\
  }

#define CHECK_MERGE_HI(VEC_TYPE, SRC1, SRC2)		\
  {							\
    VEC_TYPE v = merge_h_##VEC_TYPE ((SRC1), (SRC2));	\
    int elts = sizeof(v) / sizeof(v[0]);		\
    for (int i = 0; i < elts; i++)			\
      if (v[i] != i / 2 + (i % 2) * elts)		\
	__builtin_abort();				\
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
