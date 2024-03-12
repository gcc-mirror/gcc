/* { dg-options "-O2 --param=aarch64-stp-policy=never -mcpu=generic" } */

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

#define STP_TEST_UNALIGNED(TYPE) \
TYPE *stp_unaligned_##TYPE(char* ptr, TYPE x){ \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    TYPE *a = arr+1; \
    a[0] = x; \
    a[1] = x; \
    return a; \
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

#define STP_TEST_ADJUST_UNALIGNED(TYPE) \
TYPE *stp_unaligned_adjust_##TYPE(char* ptr, TYPE x){ \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    TYPE *a = arr+1; \
    a[100] = x; \
    a[101] = x; \
    a[102] = x; \
    a[103] = x; \
    return a; \
}

STP_TEST_ALIGNED(int32_t);
STP_TEST_ALIGNED(int64_t);
STP_TEST_ALIGNED(v4si);
STP_TEST_UNALIGNED(int32_t);
STP_TEST_UNALIGNED(int64_t);
STP_TEST_UNALIGNED(v4si);
STP_TEST_ADJUST_ALIGNED(int32_t);
STP_TEST_ADJUST_ALIGNED(int64_t);
STP_TEST_ADJUST_UNALIGNED(int32_t);
STP_TEST_ADJUST_UNALIGNED(int64_t);

/* { dg-final { scan-assembler-times "stp\tw\[0-9\]+, w\[0-9\]" 0 } } */
/* { dg-final { scan-assembler-times "stp\tx\[0-9\]+, x\[0-9\]" 0 } } */
/* { dg-final { scan-assembler-times "stp\tq\[0-9\]+, q\[0-9\]" 0 } } */

