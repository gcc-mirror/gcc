/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mtune=generic-ooo -ffast-math" } */

#define DEF_REDUC_PLUS(TYPE)                                                   \
  TYPE __attribute__ ((noinline, noclone))                                     \
  reduc_plus_##TYPE (TYPE *__restrict a, int n)                                \
  {                                                                            \
    TYPE r = 0;                                                                \
    for (int i = 0; i < n; ++i)                                                \
      r += a[i];                                                               \
    return r;                                                                  \
  }

#define TEST_PLUS(T) T (int) T (float)

TEST_PLUS (DEF_REDUC_PLUS)

/* { dg-final { scan-assembler-not {vsetivli\s+zero,\s*4} } } */
