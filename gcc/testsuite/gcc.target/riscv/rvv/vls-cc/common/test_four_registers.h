// Test: Two vectors passed in eight registers (<= 4 * ABI_VLEN bits each)
// int32x16_t (512 bits) should use 4 registers each when ABI_VLEN = 128

#include "../vls-cc-common.h"

int32x16_t VLS_CC(ABI_VLEN) test_four_registers(int32x16_t vec1, int32x16_t vec2) {
    // Add vectors element-wise
    int32x16_t result;
    result = vec1 + vec2;
    return result;
}