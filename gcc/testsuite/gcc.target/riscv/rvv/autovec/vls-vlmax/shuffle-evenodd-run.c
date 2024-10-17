/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=m8 -std=gnu99" } */

#include "shuffle-evenodd.c"

#define SERIES_2(x, y) (x), (x + 1)
#define SERIES_4(x, y) SERIES_2 (x, y), SERIES_2 (x + 2, y)
#define SERIES_8(x, y) SERIES_4 (x, y), SERIES_4 (x + 4, y)
#define SERIES_16(x, y) SERIES_8 (x, y), SERIES_8 (x + 8, y)
#define SERIES_32(x, y) SERIES_16 (x, y), SERIES_16 (x + 16, y)
#define SERIES_64(x, y) SERIES_32 (x, y), SERIES_32 (x + 32, y)

#define comp(a, b, n)                                                          \
  for (unsigned i = 0; i < n; ++i)                                             \
    if ((a)[i] != (b)[i])                                                      \
      __builtin_abort ();

#define CHECK1(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check1_##TYPE ()                                \
  {                                                                            \
    TYPE v0 = (TYPE){SERIES_##NUNITS (0, NUNITS)};                             \
    TYPE v1 = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                        \
    TYPE ref = (TYPE){MASKE_##NUNITS (0, NUNITS)};                             \
    TYPE res;                                                                  \
    permute1_##TYPE (v0, v1, &res);                                            \
    comp (res, ref, NUNITS);                                                   \
  }

#define CHECK2(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check2_##TYPE ()                       \
  {                                                                            \
    TYPE v0 = (TYPE){SERIES_##NUNITS (0, NUNITS)};                             \
    TYPE v1 = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                        \
    TYPE ref = (TYPE){MASKO_##NUNITS (0, NUNITS)};                             \
    TYPE res;                                                                  \
    permute2_##TYPE (v0, v1, &res);                                            \
    comp (res, ref, NUNITS);                                                   \
  }

#define CHECK_ALL(T)                                                            \
  T (vnx4qi, 4)                                                                \
  T (vnx8qi, 8)                                                                \
  T (vnx16qi, 16)                                                              \
  T (vnx32qi, 32)                                                              \
  T (vnx64qi, 64)                                                              \
  T (vnx4hi, 4)                                                                \
  T (vnx8hi, 8)                                                                \
  T (vnx16hi, 16)                                                              \
  T (vnx32hi, 32)                                                              \
  T (vnx64hi, 64)                                                              \
  T (vnx4si, 4)                                                                \
  T (vnx8si, 8)                                                                \
  T (vnx16si, 16)                                                              \
  T (vnx32si, 32)                                                              \
  T (vnx4di, 4)                                                                \
  T (vnx8di, 8)                                                                \
  T (vnx16di, 16)                                                              \
  T (vnx4sf, 4)                                                                \
  T (vnx8sf, 8)                                                                \
  T (vnx16sf, 16)                                                              \
  T (vnx32sf, 32)                                                              \
  T (vnx4df, 4)                                                                \
  T (vnx8df, 8)                                                                \
  T (vnx16df, 16)

CHECK_ALL (CHECK1)
CHECK_ALL (CHECK2)

int
main ()
{
  check1_vnx4qi ();
  check1_vnx8qi ();
  check1_vnx16qi ();
  check1_vnx32qi ();
  check1_vnx64qi ();
  check1_vnx4hi ();
  check1_vnx8hi ();
  check1_vnx16hi ();
  check1_vnx32hi ();
  check1_vnx64hi ();
  check1_vnx4si ();
  check1_vnx8si ();
  check1_vnx16si ();
  check1_vnx32si ();
  check1_vnx4di ();
  check1_vnx8di ();
  check1_vnx16di ();
  check1_vnx4sf ();
  check1_vnx8sf ();
  check1_vnx16sf ();
  check1_vnx32sf ();
  check1_vnx4df ();
  check1_vnx8df ();
  check1_vnx16df ();
  check2_vnx4qi ();
  check2_vnx8qi ();
  check2_vnx16qi ();
  check2_vnx32qi ();
  check2_vnx64qi ();
  check2_vnx4hi ();
  check2_vnx8hi ();
  check2_vnx16hi ();
  check2_vnx32hi ();
  check2_vnx64hi ();
  check2_vnx4si ();
  check2_vnx8si ();
  check2_vnx16si ();
  check2_vnx32si ();
  check2_vnx4di ();
  check2_vnx8di ();
  check2_vnx16di ();
  check2_vnx4sf ();
  check2_vnx8sf ();
  check2_vnx16sf ();
  check2_vnx32sf ();
  check2_vnx4df ();
  check2_vnx8df ();
  check2_vnx16df ();
}
