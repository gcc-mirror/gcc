/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector --save-temps" } */
/* { dg-do run { target { s390_z13_hw } } } */

/*
 * The vector intrinsic vec_permi(a, b, c) chooses one of the two eight-byte
 * vector elements in each of a and b, depending on the value of c. The valid
 * values for c differ from the encoding for the M4 field in assembly and in the
 * binary instruction.
 *
 * selection | c | encoding in assembly
 * a[0] b[0] | 0 | 0          -> vmrhg
 * a[0] b[1] | 1 | 1
 * a[1] b[0] | 2 | 4
 * a[1] b[1] | 3 | 5          -> vmrlg
 *
 * (i.e., indices a[i] b[j] are encoded for c as (i<<1) | j, yet for the
 * M4 field as (i<<2) | j.
 */

/* { dg-final { scan-assembler-times "\tvmrhg\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvmrlg\t" 3 } } */
/* { dg-final { scan-assembler-times "\tvpdi\t" 6 } } */

#include "vec-types.h"
#include <vecintrin.h>

#define GEN_PERMI_BITS(VEC_TYPE, BITS)			\
  VEC_TYPE __attribute__((noinline))			\
  permi_##BITS##_##VEC_TYPE(VEC_TYPE a, VEC_TYPE b) {	\
    return vec_permi (a, b, (BITS)); }

#define GEN_PERMI(VEC_TYPE)			\
  GEN_PERMI_BITS(VEC_TYPE, 0);			\
  GEN_PERMI_BITS(VEC_TYPE, 1);			\
  GEN_PERMI_BITS(VEC_TYPE, 2);			\
  GEN_PERMI_BITS(VEC_TYPE, 3);

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
