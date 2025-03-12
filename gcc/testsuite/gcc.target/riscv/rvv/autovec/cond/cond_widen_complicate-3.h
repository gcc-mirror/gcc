#ifndef COND_WIDEN_COMPLICATE_3_H
#define COND_WIDEN_COMPLICATE_3_H

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2)                                                \
  __attribute__ ((noipa)) void vwadd_##TYPE1##_##TYPE2 (                       \
    TYPE1 *__restrict dst, TYPE1 *__restrict dst2, TYPE1 *__restrict dst3,     \
    TYPE1 *__restrict dst4, TYPE2 *__restrict a, TYPE2 *__restrict b,          \
    TYPE2 *__restrict a2, TYPE2 *__restrict b2, int *__restrict pred, int n)   \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	dst[i] = pred[i] ? (TYPE1) a[i] * (TYPE1) b[i] : dst[i];               \
	dst2[i] = pred[i] ? (TYPE1) a2[i] * (TYPE1) b[i] : dst2[i];            \
	dst3[i] = pred[i] ? (TYPE1) a2[i] * (TYPE1) a[i] : dst3[i];            \
	dst4[i] = pred[i] ? (TYPE1) a[i] * (TYPE1) b2[i] : dst4[i];            \
      }                                                                        \
  }

#endif
