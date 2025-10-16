// Test: All three types mixed (int, float, vector)

#include "../vls-cc-common.h"

double VLS_CC(ABI_VLEN) test_all_mixed(int i, float f, int32x4_t vec1, double d, float32x4_t vec2, int j) {
    // int i -> a0 (integer register)
    // float f -> fa0 (FP register)  
    // int32x4_t vec1 -> v8 (vector register)
    // double d -> fa1 (FP register)
    // float32x4_t vec2 -> v9 (vector register)
    // int j -> a1 (integer register)
    return i + f + vec1[0] + d + vec2[0] + j;
}