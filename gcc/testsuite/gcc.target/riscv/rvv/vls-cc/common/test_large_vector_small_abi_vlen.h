// Test: Two large vectors that might be passed by reference with small ABI_VLEN

#include "../vls-cc-common.h"

int32x16_t VLS_CC(ABI_VLEN) test_large_vector_small_abi_vlen(int32x16_t vec1, int32x16_t vec2) {
    // Two 512-bit vectors:
    // - ABI_VLEN=128: each uses 4 registers (8 total, fits in available registers)
    // - ABI_VLEN=64:  each needs 8 registers (16 total, exceeds available, passed by reference)
    // - ABI_VLEN=32:  each needs 16 registers (32 total, far exceeds available, passed by reference)
    int32x16_t result;
    result = vec1 + vec2;
    return result;
}