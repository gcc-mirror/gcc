#ifndef HAVE_DEFINED_AVG_H
#define HAVE_DEFINED_AVG_H

#include <stdint.h>

#define DEF_AVG_0(NT, WT, NAME)                                 \
__attribute__((noinline))                                       \
void                                                            \
test_##NAME##_##WT##_##NT##_0(NT * restrict a, NT * restrict b, \
			      NT * restrict out, int n)         \
{                                                               \
  for (int i = 0; i < n; i++) {                                 \
    out[i] = (NT)(((WT)a[i] + (WT)b[i]) >> 1);                  \
  }                                                             \
}
#define DEF_AVG_0_WRAP(NT, WT, NAME) DEF_AVG_0(NT, WT, NAME)

#define RUN_AVG_0(NT, WT, NAME, a, b, out, n) \
  test_##NAME##_##WT##_##NT##_0(a, b, out, n)
#define RUN_AVG_0_WRAP(NT, WT, NAME, a, b, out, n) \
  RUN_AVG_0(NT, WT, NAME, a, b, out, n)

#define DEF_AVG_1(NT, WT, NAME)                                 \
__attribute__((noinline))                                       \
void                                                            \
test_##NAME##_##WT##_##NT##_1(NT * restrict a, NT * restrict b, \
			      NT * restrict out, int n)         \
{                                                               \
  for (int i = 0; i < n; i++) {                                 \
    out[i] = (NT)(((WT)a[i] + (WT)b[i] + 1) >> 1);              \
  }                                                             \
}
#define DEF_AVG_1_WRAP(NT, WT, NAME) DEF_AVG_1(NT, WT, NAME)

#define RUN_AVG_1(NT, WT, NAME, a, b, out, n) \
  test_##NAME##_##WT##_##NT##_1(a, b, out, n)
#define RUN_AVG_1_WRAP(NT, WT, NAME, a, b, out, n) \
  RUN_AVG_1(NT, WT, NAME, a, b, out, n)

#endif
