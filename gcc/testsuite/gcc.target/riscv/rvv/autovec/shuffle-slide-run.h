#define comp(a, b, n)                                                          \
  for (unsigned i = 0; i < n; ++i)                                             \
    if ((a)[i] != (b)[i])                                                      \
      __builtin_abort ();

#define CHECK4(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check4_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK4_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute4_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

#define CHECK8(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check8_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK8_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute8_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

#define CHECK8(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check8_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK8_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute8_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

#define CHECK16(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check16_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK16_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute16_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

#define CHECK32(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check32_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK32_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute32_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

#define CHECK64(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check64_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK64_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute64_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

#define CHECK128(TYPE, NUNITS, A, B, C)                                          \
  __attribute__ ((noipa)) void check128_##A##_##B##_##C##_##TYPE ()               \
  {                                                                            \
    TYPE v0_##TYPE = (TYPE){SERIES_##NUNITS (0, NUNITS)};                     \
    TYPE v1_##TYPE = (TYPE){SERIES_##NUNITS (NUNITS, NUNITS)};                \
    TYPE ref_##TYPE = (TYPE){MASK128_##NUNITS (0, NUNITS, A, B, C)};             \
    TYPE res_##TYPE;                                                          \
    permute128_##A##_##B##_##C##_##TYPE (v0_##TYPE, v1_##TYPE, &res_##TYPE);   \
    comp (res_##TYPE, ref_##TYPE, NUNITS);                                   \
  }

DO_ALL_TEST4(CHECK4)
DO_ALL_TEST8(CHECK8)
DO_ALL_TEST16(CHECK16)
DO_ALL_TEST32(CHECK32)
DO_ALL_TEST64(CHECK64)
DO_ALL_TEST128(CHECK128)

#define CALL_CHECK4(TYPE, NUNITS, A, B, C) check4_##A##_##B##_##C##_##TYPE ();
#define CALL_CHECK8(TYPE, NUNITS, A, B, C) check8_##A##_##B##_##C##_##TYPE ();
#define CALL_CHECK16(TYPE, NUNITS, A, B, C) check16_##A##_##B##_##C##_##TYPE ();
#define CALL_CHECK32(TYPE, NUNITS, A, B, C) check32_##A##_##B##_##C##_##TYPE ();
#define CALL_CHECK64(TYPE, NUNITS, A, B, C) check64_##A##_##B##_##C##_##TYPE ();
#define CALL_CHECK128(TYPE, NUNITS, A, B, C) check128_##A##_##B##_##C##_##TYPE ();

int
main ()
{
  DO_ALL_TEST4(CALL_CHECK4)
  DO_ALL_TEST8(CALL_CHECK8)
  DO_ALL_TEST16(CALL_CHECK16)
  DO_ALL_TEST32(CALL_CHECK32)
  DO_ALL_TEST64(CALL_CHECK64)
  DO_ALL_TEST128(CALL_CHECK128)
}
