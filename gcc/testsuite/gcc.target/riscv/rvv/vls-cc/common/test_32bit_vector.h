// Test: Two 32-bit vectors (very small vectors)

#include "../vls-cc-common.h"

int16x2_t VLS_CC(ABI_VLEN) test_32bit_vector(int16x2_t vec1, int16x2_t vec2) {
    // Two 32-bit vectors:
    // - Each always uses 1 vector register regardless of ABI_VLEN
    // - Uses even smaller portion when ABI_VLEN > 32
    int16x2_t result;
    result[0] = vec1[0] + vec2[0];
    result[1] = vec1[1] + vec2[1];
    return result;
}