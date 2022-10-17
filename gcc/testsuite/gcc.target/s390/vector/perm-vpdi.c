/* { dg-do run { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector --save-temps" } */

/* { dg-final { scan-assembler-times "\tvmrhg\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvmrlg\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvpdi\t" 6 } } */

#include "vec-types.h"
#include <vecintrin.h>

#define GEN_PERMI_BITS(VEC_TYPE, BITS)				\
  VEC_TYPE __attribute__((noinline))				\
  permi_##BITS##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {		\
    return (VEC_TYPE){a[((BITS) & 2) >> 1], b[(BITS) & 1] }; }

#define GEN_PERMI(VEC_TYPE)			\
  GEN_PERMI_BITS(VEC_TYPE, 0);			\
  GEN_PERMI_BITS(VEC_TYPE, 1);			\
  GEN_PERMI_BITS(VEC_TYPE, 2);			\
  GEN_PERMI_BITS(VEC_TYPE, 3);			\

GEN_PERMI(v2di)
GEN_PERMI(uv2di)
GEN_PERMI(v2df)


#define CHECK_PERMI_BITS(VEC_TYPE, BITS)		\
  VEC_TYPE r##BITS = permi_##BITS##_##VEC_TYPE (a, b);	\
  if (r##BITS[0] != ((BITS) & 2) >> 1			\
      || r##BITS[1] != ((BITS) & 1) + 2)		\
    __builtin_abort();

#define CHECK_PERMI(VEC_TYPE)			\
  {						\
    VEC_TYPE a = GEN_SEQ_VEC (VEC_TYPE, 0);	\
    VEC_TYPE b = GEN_SEQ_VEC (VEC_TYPE, 2);	\
    CHECK_PERMI_BITS (VEC_TYPE, 0);		\
    CHECK_PERMI_BITS (VEC_TYPE, 1);		\
    CHECK_PERMI_BITS (VEC_TYPE, 2);		\
    CHECK_PERMI_BITS (VEC_TYPE, 3);		\
  }

int
main ()
{
  CHECK_PERMI (v2di);
  CHECK_PERMI (uv2di);
  CHECK_PERMI (v2df);
}
