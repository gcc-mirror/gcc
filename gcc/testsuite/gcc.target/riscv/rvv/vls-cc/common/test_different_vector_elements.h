// Test: Vector types with different element types in same call

#include "../vls-cc-common.h"

int VLS_CC(ABI_VLEN) test_different_vector_elements(int8x16_t byte_vec, int16x8_t short_vec, 
                                  int32x4_t int_vec, int64x2_t long_vec) {
    // All are 128-bit vectors but with different element types
    // Should all use same vector register allocation rules
    return byte_vec[0] + short_vec[0] + int_vec[0] + (int)long_vec[0];
}