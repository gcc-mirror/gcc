// Test: Vector register exhaustion - should fall back to reference passing

#include "../vls-cc-common.h"

int32x4_t VLS_CC(ABI_VLEN) test_register_exhaustion(
    int32x4_t v1, int32x4_t v2, int32x4_t v3, int32x4_t v4,
    int32x4_t v5, int32x4_t v6, int32x4_t v7, int32x4_t v8,
    int32x4_t v9, int32x4_t v10, int32x4_t v11, int32x4_t v12,
    int32x4_t v13, int32x4_t v14, int32x4_t v15, int32x4_t v16,
    int32x4_t v17) {
    // First 16 vectors (v1-v16) should use vector registers v8-v23
    // 17th vector (v17) should be passed by reference due to register exhaustion
    int32x4_t result = {v1[0] + v17[0], v2[1] + v17[1], v3[2] + v17[2], v4[3] + v17[3]};
    return result;
}