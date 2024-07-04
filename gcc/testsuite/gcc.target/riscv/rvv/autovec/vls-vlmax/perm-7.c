/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include "perm.h"

#define PERMUTE(TYPE, TYPE2, NUNITS)                                           \
  __attribute__ ((noipa)) void permute_##TYPE (TYPE values1, TYPE values2,     \
					       TYPE2 mask, TYPE *out)          \
  {                                                                            \
    TYPE v = __builtin_shuffle (values1, values2, mask);                       \
    *(TYPE *) out = v;                                                         \
  }

#define TEST_ALL(T)                                                            \
  T (vnx2qi, vnx2qi, 2)                                                        \
  T (vnx4qi, vnx4qi, 4)                                                        \
  T (vnx8qi, vnx8qi, 8)                                                        \
  T (vnx16qi, vnx16qi, 16)                                                     \
  T (vnx32qi, vnx32qi, 32)                                                     \
  T (vnx64qi, vnx64qi, 64)                                                     \
  T (vnx128qi, vnx128qi, 128)                                                  \
  T (vnx2hi, vnx2hi, 2)                                                        \
  T (vnx4hi, vnx4hi, 4)                                                        \
  T (vnx8hi, vnx8hi, 8)                                                        \
  T (vnx16hi, vnx16hi, 16)                                                     \
  T (vnx32hi, vnx32hi, 32)                                                     \
  T (vnx64hi, vnx64hi, 64)                                                     \
  T (vnx2si, vnx2si, 2)                                                        \
  T (vnx4si, vnx4si, 4)                                                        \
  T (vnx8si, vnx8si, 8)                                                        \
  T (vnx16si, vnx16si, 16)                                                     \
  T (vnx32si, vnx32si, 32)                                                     \
  T (vnx2di, vnx2di, 2)                                                        \
  T (vnx4di, vnx4di, 4)                                                        \
  T (vnx8di, vnx8di, 8)                                                        \
  T (vnx16di, vnx16di, 16)                                                     \
  T (vnx2sf, vnx2si, 2)                                                        \
  T (vnx4sf, vnx4si, 4)                                                        \
  T (vnx8sf, vnx8si, 8)                                                        \
  T (vnx16sf, vnx16si, 16)                                                     \
  T (vnx32sf, vnx32si, 32)                                                     \
  T (vnx2df, vnx2di, 2)                                                        \
  T (vnx4df, vnx4di, 4)                                                        \
  T (vnx8df, vnx8di, 8)                                                        \
  T (vnx16df, vnx16di, 16)

TEST_ALL (PERMUTE)

/* { dg-final { scan-assembler-times {vrgather\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 31 } } */
