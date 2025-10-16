// Test: Struct with two vectors of different widths but same total size
// int32x4_t (128 bits, 4 x 32-bit) and int16x8_t (128 bits, 8 x 16-bit) - both should be flattened

#include "../vls-cc-common.h"

typedef struct {
    int32x4_t wide_vec;   // 128-bit vector with 4 x 32-bit elements
    int16x8_t narrow_vec; // 128-bit vector with 8 x 16-bit elements
} different_width_vectors_struct_t;

different_width_vectors_struct_t VLS_CC(ABI_VLEN) test_different_width_vectors_struct(different_width_vectors_struct_t s) {
    // Should be flattened and passed as two separate vector arguments
    // wide_vec should use first vector register, narrow_vec should use second vector register
    different_width_vectors_struct_t result;
    
    // Process wide vector: add 100 to each 32-bit element
    for (int i = 0; i < 4; i++) {
        result.wide_vec[i] = s.wide_vec[i] + 100;
    }
    
    // Process narrow vector: add 10 to each 16-bit element
    for (int i = 0; i < 8; i++) {
        result.narrow_vec[i] = s.narrow_vec[i] + 10;
    }
    
    return result;
}