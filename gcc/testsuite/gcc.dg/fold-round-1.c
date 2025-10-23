/* { dg-do compile } */
/* { dg-options "-Ofast" } */

extern void link_error (void);

#define TEST_ROUND(TYPE, FFLOOR, FCEIL)                                       \
  void round_##FFLOOR##_1 (TYPE x)                                            \
  {                                                                           \
    TYPE t1 = 0;                                                              \
    TYPE t2 = __builtin_##FFLOOR (x + 0.5);                                   \
    if ((x - __builtin_##FFLOOR (x)) < (__builtin_##FCEIL (x) - x))           \
      t1 = __builtin_##FFLOOR (x);                                            \
    else                                                                      \
      t1 = __builtin_##FCEIL (x);                                             \
    if (t1 != t2)                                                             \
      link_error ();                                                          \
  }                                                                           \
  void round_##FFLOOR##_2 (TYPE x)                                            \
  {                                                                           \
    TYPE t1 = 0;                                                              \
    TYPE t2 = __builtin_##FFLOOR (x + 0.5);                                   \
    if ((__builtin_##FCEIL (x) - x) > (x - __builtin_##FFLOOR (x)))           \
      t1 = __builtin_##FFLOOR (x);                                            \
    else                                                                      \
      t1 = __builtin_##FCEIL (x);                                             \
    if (t1 != t2)                                                             \
      link_error ();                                                          \
  }                                                                           \
  void round_##FFLOOR##_3 (TYPE x)                                            \
  {                                                                           \
    TYPE t1 = 0;                                                              \
    TYPE t2 = __builtin_##FFLOOR (x + 0.5);                                   \
    if ((__builtin_##FCEIL (x) - x) <= (x - __builtin_##FFLOOR (x)))          \
      t1 = __builtin_##FCEIL (x);                                             \
    else                                                                      \
      t1 = __builtin_##FFLOOR (x);                                            \
    if (t1 != t2)                                                             \
      link_error ();                                                          \
  }                                                                           \
  void round_##FFLOOR##_4 (TYPE x)                                            \
  {                                                                           \
    TYPE t1 = 0;                                                              \
    TYPE t2 = __builtin_##FFLOOR (x + 0.5);                                   \
    if ((x - __builtin_##FFLOOR (x)) >= (__builtin_##FCEIL (x) - x))          \
      t1 = __builtin_##FCEIL (x);                                             \
    else                                                                      \
      t1 = __builtin_##FFLOOR (x);                                            \
    if (t1 != t2)                                                             \
      link_error ();                                                          \
  }

TEST_ROUND (float, floorf, ceilf)
TEST_ROUND (double, floor, ceil)
TEST_ROUND (long double, floorl, ceill)

/* { dg-final { scan-assembler-not "link_error" } } */
