// Test: Two 256-bit vectors behavior under different ABI_VLEN

#include "../vls-cc-common.h"

int32x8_t VLS_CC(ABI_VLEN) test_256bit_vector(int32x8_t vec1, int32x8_t vec2) {
    // Two 256-bit vectors:
    // - ABI_VLEN=256: each uses 1 vector register (2 total)
    // - ABI_VLEN=128: each uses 2 vector registers (4 total)
    // - ABI_VLEN=64:  each uses 4 vector registers (8 total)
    // - ABI_VLEN=32:  each uses 8 vector registers (16 total)
    int32x8_t result;
    result = vec1 + vec2;
    return result;
}