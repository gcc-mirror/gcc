/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-assembler-not "\tfmul\t" } } */

#define TEST(TYPE, BUILTIN, CONST, NAME)                       \
  TYPE test_##NAME##_1(TYPE a, int i)                          \
  {                                                            \
    return CONST * __builtin_##BUILTIN(a, i);                  \
  }                                                            \
  TYPE test_##NAME##_2(int i)                                  \
  {                                                            \
    return 45 * __builtin_##BUILTIN(1.0, i);                   \
  }                                                            \
  TYPE test_##NAME##_3(TYPE a, int i)                          \
  {                                                            \
    return a * __builtin_##BUILTIN(1.0, i);                    \
  }                                                            \
  TYPE test_##NAME##_4(int i)                                  \
  {                                                            \
    TYPE a = CONST;                                            \
    return a * __builtin_##BUILTIN(a, i);                      \
  }                                                            \
  TYPE test_##NAME##_5(TYPE a, int i)                          \
  {                                                            \
    TYPE t1 = a;                                               \
    return t1 * __builtin_##BUILTIN(1.0, i);                   \
  }

TEST(double, ldexp, 8.0, double_ldexp)
TEST(float, ldexpf, 8.0f, float_ldexp)
TEST(long double, ldexpl, 8.0L, long_ldexp)