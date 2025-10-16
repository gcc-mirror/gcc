// Test: Two vectors passed in registers (<= ABI_VLEN bits each)
// int32x4_t (128 bits) should use 1 register per vector when ABI_VLEN >= 128

#include "../vls-cc-common.h"

int32x4_t VLS_CC(ABI_VLEN) test_single_register(int32x4_t vec1, int32x4_t vec2) {
    // Add vectors element-wise
    int32x4_t result;
    result = vec1 + vec2;
    return result;
}