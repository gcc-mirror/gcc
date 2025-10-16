// Test: Floating-point vectors vs integer vectors

#include "../vls-cc-common.h"

float VLS_CC(ABI_VLEN) test_fp_vs_int_vectors(int32x4_t int_vec, float32x4_t float_vec, 
                            double64x2_t double_vec) {
    // All vectors should use vector registers, regardless of element type
    // Element type shouldn't affect register allocation for fixed-length vectors
    return int_vec[0] + float_vec[0] + (float)double_vec[0];
}