/* { dg-options "-O2 --param=aarch64-ldp-policy=always -mcpu=generic" } */

#include <stdlib.h>
#include <stdint.h>

typedef int v4si __attribute__ ((vector_size (16)));

#define LDP_TEST_ALIGNED(TYPE) \
TYPE ldp_aligned_##TYPE(char* ptr){ \
    TYPE a_0, a_1; \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    a_0 = arr[0]; \
    a_1 = arr[1]; \
    return a_0 + a_1; \
}

#define LDP_TEST_UNALIGNED(TYPE) \
TYPE ldp_unaligned_##TYPE(char* ptr){ \
    TYPE a_0, a_1; \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    TYPE *a = arr+1; \
    a_0 = a[0]; \
    a_1 = a[1]; \
    return a_0 + a_1; \
}

LDP_TEST_ALIGNED(int32_t);
LDP_TEST_ALIGNED(int64_t);
LDP_TEST_ALIGNED(v4si);
LDP_TEST_UNALIGNED(int32_t);
LDP_TEST_UNALIGNED(int64_t);
LDP_TEST_UNALIGNED(v4si);

/* { dg-final { scan-assembler-times "ldp\tw\[0-9\]+, w\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "ldp\tx\[0-9\]+, x\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "ldp\tq\[0-9\]+, q\[0-9\]" 2 } } */

