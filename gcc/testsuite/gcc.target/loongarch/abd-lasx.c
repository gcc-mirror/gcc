/* { dg-do compile } */
/* { dg-options "-O3 -mlasx -fdump-rtl-expand-all" } */

#define ABD(x, y) ((x - y > 0) ? (x - y) : -(x - y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define N 1024

#define FUNC1(T)                                                              \
  void									      \
  sabd1_##T (signed T *restrict a, signed T *restrict b,                      \
             signed T *restrict out)                                          \
  {                                                                           \
    for (int i = 0; i < N; i++)                                               \
      out[i] = ABD (a[i], b[i]);                                              \
  }                                                                           \
                                                                              \
  void									      \
  uabd1_##T (unsigned T *restrict a, unsigned T *restrict b,                  \
             unsigned T *restrict out)                                        \
  {                                                                           \
    for (int i = 0; i < N; i++)                                               \
      out[i] = ABD (a[i], b[i]);                                              \
  }

#define FUNC2(T)                                                              \
  void									      \
  sabd2_##T (signed T *restrict a, signed T *restrict b,                      \
             signed T *restrict out)                                          \
  {                                                                           \
    for (int i = 0; i < N; i++)                                               \
      out[i] = MAX (a[i], b[i]) - MIN (a[i], b[i]);                           \
  }                                                                           \
                                                                              \
  void									      \
  uabd2_##T (unsigned T *restrict a, unsigned T *restrict b,                  \
             unsigned T *restrict out)                                        \
  {                                                                           \
    for (int i = 0; i < N; i++)                                               \
      out[i] = MAX (a[i], b[i]) - MIN (a[i], b[i]);                           \
  }

/* Verify if the expand pass fits standard pattern name.  */
FUNC1 (char)
FUNC1 (short)
FUNC1 (int)
FUNC1 (long)

/* Verify if the combiner works well.  */
FUNC2 (char)
FUNC2 (short)
FUNC2 (int)
FUNC2 (long)
/* { dg-final { scan-rtl-dump "Function sabd1_char.*ABD.*Function uabd1_char" "expand" } } */
/* { dg-final { scan-rtl-dump "Function uabd1_char.*ABD.*Function sabd1_short" "expand" } } */
/* { dg-final { scan-rtl-dump "Function sabd1_short.*ABD.*Function uabd1_short" "expand" } } */
/* { dg-final { scan-rtl-dump "Function uabd1_short.*ABD.*Function sabd1_int" "expand" } } */
/* { dg-final { scan-rtl-dump "Function sabd1_int.*ABD.*Function uabd1_int" "expand" } } */
/* { dg-final { scan-rtl-dump "Function sabd1_long.*ABD.*Function uabd1_long" "expand" } } */
/* { dg-final { scan-assembler-times "sabd2_char:.*\txvabsd\\.b.*-sabd2_char" 1 } } */
/* { dg-final { scan-assembler-times "uabd2_char:.*\txvabsd\\.bu.*-uabd2_char" 1 } } */
/* { dg-final { scan-assembler-times "sabd2_short:.*\txvabsd\\.h.*-sabd2_short" 1 } } */
/* { dg-final { scan-assembler-times "uabd2_short:.*\txvabsd\\.hu.*-uabd2_short" 1 } } */
/* { dg-final { scan-assembler-times "sabd2_int:.*\txvabsd\\.w.*-sabd2_int" 1 } } */
/* { dg-final { scan-assembler-times "uabd2_int:.*\txvabsd\\.wu.*-uabd2_int" 1 } } */
/* { dg-final { scan-assembler-times "sabd2_long:.*\txvabsd\\.d.*-sabd2_long" 1 } } */
/* { dg-final { scan-assembler-times "uabd2_long:.*\txvabsd\\.du.*-uabd2_long" 1 } } */
