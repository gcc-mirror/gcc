// Test: Multiple vector arguments consuming available registers

#include "../vls-cc-common.h"

int32x4_t VLS_CC(ABI_VLEN) test_multiple_vectors(int32x4_t v1, int32x4_t v2, int32x4_t v3) {
    // Each vector uses 1 register, total 3 registers
    int32x4_t result = {v1[0] + v2[0] + v3[0], 
                        v1[1] + v2[1] + v3[1],
                        v1[2] + v2[2] + v3[2], 
                        v1[3] + v2[3] + v3[3]};
    return result;
}