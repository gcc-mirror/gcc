/* { dg-options "-O2 --param=aarch64-stp-policy=aligned -mcpu=generic" } */

#include <stdlib.h>
#include <stdint.h>

typedef int v4si __attribute__ ((vector_size (16)));

#define STP_TEST_ALIGNED(TYPE) \
TYPE *stp_aligned_##TYPE(char* ptr, TYPE x){ \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    arr[0] = x; \
    arr[1] = x; \
    return arr; \
}

#define STP_TEST_ADJUST_ALIGNED(TYPE) \
TYPE *stp_aligned_adjust_##TYPE(char* ptr, TYPE x){ \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    arr[100] = x; \
    arr[101] = x; \
    arr[102] = x; \
    arr[103] = x; \
    return arr; \
}

STP_TEST_ALIGNED(int32_t);
STP_TEST_ALIGNED(int64_t);
STP_TEST_ALIGNED(v4si);
STP_TEST_ADJUST_ALIGNED(int32_t);
STP_TEST_ADJUST_ALIGNED(int64_t);

/* { dg-final { scan-assembler-times "stp\tw\[0-9\]+, w\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "stp\tx\[0-9\]+, x\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "stp\tq\[0-9\]+, q\[0-9\]" 1 } } */

