// Test: Mixed integer scalars and vectors

#include "../vls-cc-common.h"

int VLS_CC(ABI_VLEN) test_mixed_int_vector(int a, int32x4_t vec, int b, int32x4_t vec2) {
    // Scalars should use integer registers (a0, a1, a2, ...)
    // Vectors should use vector registers (v8, v9, ...)  
    // Should not interfere with each other's register allocation
    return a + b + vec[0] + vec2[0];
}