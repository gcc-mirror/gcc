// Test: Mixed floating-point scalars and vectors

#include "../vls-cc-common.h"

float VLS_CC(ABI_VLEN) test_mixed_float_vector(float f1, float32x4_t vec, double d1, float32x4_t vec2) {
    // Float scalars should use FP registers (fa0, fa1, ...)
    // Vectors should use vector registers (v8, v9, ...)
    // Different register classes should not interfere
    return f1 + (float)d1 + vec[0] + vec2[0];
}