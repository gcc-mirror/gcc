/* { dg-options "-O2 --param=aarch64-ldp-policy=aligned -mcpu=generic" } */

#include <stdlib.h>
#include <stdint.h>

typedef int v4si __attribute__ ((vector_size (16)));

#define LDP_TEST_UNALIGNED(TYPE) \
TYPE ldp_unaligned_##TYPE(char* ptr){ \
    TYPE a_0, a_1; \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    TYPE *a = arr+1; \
    a_0 = a[0]; \
    a_1 = a[1]; \
    return a_0 + a_1; \
}

#define LDP_TEST_ADJUST_UNALIGNED(TYPE) \
TYPE ldp_unaligned_adjust_##TYPE(char* ptr){ \
    TYPE a_0, a_1, a_2, a_3, a_4; \
    TYPE *arr = (TYPE*) ((uintptr_t)ptr & ~(2 * 8 * _Alignof(TYPE) - 1)); \
    TYPE *a = arr+1; \
    a_0 = a[100]; \
    a_1 = a[101]; \
    a_2 = a[102]; \
    a_3 = a[103]; \
    a_4 = a[110]; \
    return a_0 + a_1 + a_2 + a_3 + a_4; \
}

LDP_TEST_UNALIGNED(int32_t);
LDP_TEST_UNALIGNED(int64_t);
LDP_TEST_UNALIGNED(v4si);
LDP_TEST_ADJUST_UNALIGNED(int32_t);
LDP_TEST_ADJUST_UNALIGNED(int64_t);

/* { dg-final { scan-assembler-times "ldp\tw\[0-9\]+, w\[0-9\]" 0 } } */
/* { dg-final { scan-assembler-times "ldp\tx\[0-9\]+, x\[0-9\]" 0 } } */
/* { dg-final { scan-assembler-times "ldp\tq\[0-9\]+, q\[0-9\]" 0 } } */

