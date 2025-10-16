// Test: Large number of mixed arguments to test register exhaustion

#include "../vls-cc-common.h"

int VLS_CC(ABI_VLEN) test_register_exhaustion_mixed(
    // Integer arguments (use a0-a7)
    int i1, int i2, int i3, int i4, int i5, int i6, int i7, int i8, int i9,
    // FP arguments (use fa0-fa7) 
    float f1, float f2, float f3, float f4, float f5, float f6, float f7, float f8, float f9,
    // Vector arguments (use v8-v23, 16 registers available)
    int32x4_t v1, int32x4_t v2, int32x4_t v3, int32x4_t v4,
    int32x4_t v5, int32x4_t v6, int32x4_t v7, int32x4_t v8,
    int32x4_t v9, int32x4_t v10, int32x4_t v11, int32x4_t v12,
    int32x4_t v13, int32x4_t v14, int32x4_t v15, int32x4_t v16,
    int32x4_t v17
) {
    // i9, f9 should be passed via stack due to integer/FP register exhaustion
    // v1-v16 should use vector registers v8-v23
    // v17 should be passed via stack/memory due to vector register exhaustion
    return i1 + i9 + (int)f1 + (int)f9 + v1[0] + v17[0];
}