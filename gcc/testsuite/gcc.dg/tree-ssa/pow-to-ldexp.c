/* { dg-do compile } */
/* { dg-additional-options "-Ofast -fdump-tree-optimized" } */
/* { dg-require-effective-target c99_runtime } */

/* { dg-final { scan-tree-dump-times "__builtin_ldexp\ " 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ldexpf\ " 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ldexpl\ " 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_powi" 0 "optimized" } } */

#define TEST(TYPE, BUILTIN, CONST, NAME)                        \
  TYPE test_##NAME##_1(TYPE a, int i)                           \
  {                                                             \
    return a * __builtin_##BUILTIN(CONST, i);                   \
  }                                                             \
  TYPE test_##NAME##_2(int i)                                   \
  {                                                             \
    return __builtin_##BUILTIN(CONST, i);                       \
  }                                                             \
  TYPE test_##NAME##_3(int i)                                   \
  {                                                             \
    return CONST * __builtin_##BUILTIN(CONST, i);               \
  }                                                             \
  TYPE test_##NAME##_4(TYPE a, int i)                           \
  {                                                             \
    TYPE t1 = a;                                                \
    return t1 * __builtin_##BUILTIN(CONST, i);                  \
  }                                                             \
  TYPE test_##NAME##_5(int i)                                   \
  {                                                             \
    TYPE powof2 = 8;                                            \
    return powof2 * __builtin_##BUILTIN(CONST, i);              \
  }                                                             \
  TYPE test_##NAME##_6(int i)                                   \
  {                                                             \
    TYPE powof2 = 32;                                           \
    return __builtin_##BUILTIN(powof2, i);                      \
  }                                                             \
  TYPE test_##NAME##_7(int i)                                   \
  {                                                             \
    return 2 * __builtin_##BUILTIN(CONST, i);                   \
  }

TEST(double, powi, 2.0, double_powi)
TEST(float, powif, 2.0f, float_powif)
TEST(long double, powil, 2.0, long_powil)
