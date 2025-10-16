// Test: Two 64-bit vectors (always fit in one register each)

#include "../vls-cc-common.h"

int32x2_t VLS_CC(ABI_VLEN) test_64bit_vector(int32x2_t vec1, int32x2_t vec2) {
    // Two 64-bit vectors:
    // - Each always uses 1 vector register regardless of ABI_VLEN
    // - Only uses portion of the register when ABI_VLEN > 64
    int32x2_t result;
    result[0] = vec1[0] + vec2[0];
    result[1] = vec1[1] + vec2[1];
    return result;
}