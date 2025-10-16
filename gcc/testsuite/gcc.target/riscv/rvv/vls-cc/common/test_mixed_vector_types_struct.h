// Test: Struct with two vectors of same width but different types
// int32x4_t (128 bits) and float32x4_t (128 bits) - both should be flattened

#include "../vls-cc-common.h"

typedef struct {
    int32x4_t int_vec;    // 128-bit integer vector
    float32x4_t float_vec; // 128-bit float vector
} mixed_vector_types_struct_t;

mixed_vector_types_struct_t VLS_CC(ABI_VLEN) test_mixed_vector_types_struct(mixed_vector_types_struct_t s) {
    // Should be flattened and passed as two separate vector arguments
    // int_vec should use first vector register, float_vec should use second vector register
    mixed_vector_types_struct_t result;
    
    // Combine the vectors: add 10 to int elements, multiply float elements by 2.0
    for (int i = 0; i < 4; i++) {
        result.int_vec[i] = s.int_vec[i] + 10;
        result.float_vec[i] = s.float_vec[i] * 2.0f;
    }
    
    return result;
}