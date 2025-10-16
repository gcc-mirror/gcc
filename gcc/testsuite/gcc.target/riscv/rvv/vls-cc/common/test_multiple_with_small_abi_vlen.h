// Test: Multiple vectors that may exceed register count with small ABI_VLEN

#include "../vls-cc-common.h"

int32x4_t VLS_CC(ABI_VLEN) test_multiple_with_small_abi_vlen(int32x4_t v1, int32x4_t v2, 
                                           int32x4_t v3, int32x4_t v4) {
    // With ABI_VLEN=32, each 128-bit vector needs 4 registers
    // 4 vectors  *  4 registers = 16 registers needed
    // Will exceed available vector registers, some passed by reference
    int32x4_t result = {v1[0] + v2[0], v1[1] + v3[1], v1[2] + v4[2], v1[3] + v2[3]};
    return result;
}