/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -std=gnu99 -mrvv-max-lmul=m8 -Wno-overflow" } */

#include "shuffle-slide.c"

#define comp(a, b, n)                                                          \
  for (unsigned i = 0; i < n; ++i)                                             \
    if ((a)[i] != (b)[i])                                                      \
      __builtin_abort ();

#define CHECK1(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check1_##TYPE ()                                \
  {                                                                            \
    TYPE v10_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v11_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref1_##TYPE = (TYPE){MASK1_##NUNITS (0, NUNITS)};                     \
    TYPE res1_##TYPE;                                                          \
    permute1_##TYPE (v10_##TYPE, v11_##TYPE, &res1_##TYPE);                    \
    comp (res1_##TYPE, ref1_##TYPE, NUNITS);                                   \
  }

#define CHECK2(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check2_##TYPE ()                                \
  {                                                                            \
    TYPE v20_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v21_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref2_##TYPE = (TYPE){MASK1D_##NUNITS (0, NUNITS)};                    \
    TYPE res2_##TYPE;                                                          \
    permute2_##TYPE (v20_##TYPE, v21_##TYPE, &res2_##TYPE);                    \
    comp (res2_##TYPE, ref2_##TYPE, NUNITS);                                   \
  }

#define CHECK3(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check3_##TYPE ()                                \
  {                                                                            \
    TYPE v30_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v31_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref3_##TYPE = (TYPE){MASK2U_##NUNITS (0, NUNITS)};                    \
    TYPE res3_##TYPE;                                                          \
    permute3_##TYPE (v30_##TYPE, v31_##TYPE, &res3_##TYPE);                    \
    comp (res3_##TYPE, ref3_##TYPE, NUNITS);                                   \
  }

#define CHECK4(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check4_##TYPE ()                                \
  {                                                                            \
    TYPE v40_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v41_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref4_##TYPE = (TYPE){MASK3U_##NUNITS (0, NUNITS)};                    \
    TYPE res4_##TYPE;                                                          \
    permute4_##TYPE (v40_##TYPE, v41_##TYPE, &res4_##TYPE);                    \
    comp (res4_##TYPE, ref4_##TYPE, NUNITS);                                   \
  }

#define CHECK5(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check5_##TYPE ()                                \
  {                                                                            \
    TYPE v50_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v51_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref5_##TYPE = (TYPE){MASK2D_##NUNITS (0, NUNITS)};                    \
    TYPE res5_##TYPE;                                                          \
    permute5_##TYPE (v50_##TYPE, v51_##TYPE, &res5_##TYPE);                    \
    comp (res5_##TYPE, ref5_##TYPE, NUNITS);                                   \
  }

#define CHECK6(TYPE, NUNITS)                                                   \
  __attribute__ ((noipa)) void check6_##TYPE ()                                \
  {                                                                            \
    TYPE v60_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v61_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref6_##TYPE = (TYPE){MASK3D_##NUNITS (0, NUNITS)};                    \
    TYPE res6_##TYPE;                                                          \
    permute6_##TYPE (v60_##TYPE, v61_##TYPE, &res6_##TYPE);                    \
    comp (res6_##TYPE, ref6_##TYPE, NUNITS);                                   \
  }

#define CHECK_ALL(T)                                                           \
  T (vnx4qi, 4)                                                                \
  T (vnx8qi, 8)                                                                \
  T (vnx16qi, 16)                                                              \
  T (vnx32qi, 32)                                                              \
  T (vnx64qi, 64)                                                              \
  T (vnx128qi, 128)                                                            \
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
CHECK_ALL (CHECK3)
CHECK_ALL (CHECK4)
CHECK_ALL (CHECK5)
CHECK_ALL (CHECK6)

int
main ()
{
  check1_vnx4qi ();
  check1_vnx8qi ();
  check1_vnx16qi ();
  check1_vnx32qi ();
  check1_vnx64qi ();
  check1_vnx128qi ();
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
  check2_vnx128qi ();
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
  check3_vnx4qi ();
  check3_vnx8qi ();
  check3_vnx16qi ();
  check3_vnx32qi ();
  check3_vnx64qi ();
  check3_vnx128qi ();
  check3_vnx4hi ();
  check3_vnx8hi ();
  check3_vnx16hi ();
  check3_vnx32hi ();
  check3_vnx64hi ();
  check3_vnx4si ();
  check3_vnx8si ();
  check3_vnx16si ();
  check3_vnx32si ();
  check3_vnx4di ();
  check3_vnx8di ();
  check3_vnx16di ();
  check3_vnx4sf ();
  check3_vnx8sf ();
  check3_vnx16sf ();
  check3_vnx32sf ();
  check3_vnx4df ();
  check3_vnx8df ();
  check3_vnx16df ();
  check4_vnx4qi ();
  check4_vnx8qi ();
  check4_vnx16qi ();
  check4_vnx32qi ();
  check4_vnx64qi ();
  check4_vnx128qi ();
  check4_vnx4hi ();
  check4_vnx8hi ();
  check4_vnx16hi ();
  check4_vnx32hi ();
  check4_vnx64hi ();
  check4_vnx4si ();
  check4_vnx8si ();
  check4_vnx16si ();
  check4_vnx32si ();
  check4_vnx4di ();
  check4_vnx8di ();
  check4_vnx16di ();
  check4_vnx4sf ();
  check4_vnx8sf ();
  check4_vnx16sf ();
  check4_vnx32sf ();
  check4_vnx4df ();
  check4_vnx8df ();
  check4_vnx16df ();
  check5_vnx4qi ();
  check5_vnx8qi ();
  check5_vnx16qi ();
  check5_vnx32qi ();
  check5_vnx64qi ();
  check5_vnx128qi ();
  check5_vnx4hi ();
  check5_vnx8hi ();
  check5_vnx16hi ();
  check5_vnx32hi ();
  check5_vnx64hi ();
  check5_vnx4si ();
  check5_vnx8si ();
  check5_vnx16si ();
  check5_vnx32si ();
  check5_vnx4di ();
  check5_vnx8di ();
  check5_vnx16di ();
  check5_vnx4sf ();
  check5_vnx8sf ();
  check5_vnx16sf ();
  check5_vnx32sf ();
  check5_vnx4df ();
  check5_vnx8df ();
  check5_vnx16df ();
  check6_vnx4qi ();
  check6_vnx8qi ();
  check6_vnx16qi ();
  check6_vnx32qi ();
  check6_vnx64qi ();
  check6_vnx128qi ();
  check6_vnx4hi ();
  check6_vnx8hi ();
  check6_vnx16hi ();
  check6_vnx32hi ();
  check6_vnx64hi ();
  check6_vnx4si ();
  check6_vnx8si ();
  check6_vnx16si ();
  check6_vnx32si ();
  check6_vnx4di ();
  check6_vnx8di ();
  check6_vnx16di ();
  check6_vnx4sf ();
  check6_vnx8sf ();
  check6_vnx16sf ();
  check6_vnx32sf ();
  check6_vnx4df ();
  check6_vnx8df ();
  check6_vnx16df ();
}
