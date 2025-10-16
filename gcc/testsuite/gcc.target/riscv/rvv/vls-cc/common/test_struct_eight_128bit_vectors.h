// Test: Struct with eight 128-bit vectors
// Each int32x4_t is 128 bits (4 x 32-bit elements)
// Total: 8 vectors * 128 bits = 1024 bits
// With ABI_VLEN=128: 8 vectors * 1 register each = 8 registers (fits in 16 available)

#include "../vls-cc-common.h"

typedef struct {
    int32x4_t vec1;  // 128-bit vector
    int32x4_t vec2;  // 128-bit vector
    int32x4_t vec3;  // 128-bit vector
    int32x4_t vec4;  // 128-bit vector
    int32x4_t vec5;  // 128-bit vector
    int32x4_t vec6;  // 128-bit vector
    int32x4_t vec7;  // 128-bit vector
    int32x4_t vec8;  // 128-bit vector
} eight_128bit_vectors_struct_t;

eight_128bit_vectors_struct_t VLS_CC(ABI_VLEN) test_struct_eight_128bit_vectors(eight_128bit_vectors_struct_t s) {
    // Should be flattened and passed as separate vector arguments
    // With ABI_VLEN=128: vec1 uses v8, vec2 uses v9, ..., vec8 uses v15
    // Total uses 8 out of 16 available vector argument registers
    eight_128bit_vectors_struct_t result;
    
    // Process each 128-bit vector: multiply by (index + 1)
    for (int i = 0; i < 4; i++) {
        result.vec1[i] = s.vec1[i] * 1;
        result.vec2[i] = s.vec2[i] * 2;
        result.vec3[i] = s.vec3[i] * 3;
        result.vec4[i] = s.vec4[i] * 4;
        result.vec5[i] = s.vec5[i] * 5;
        result.vec6[i] = s.vec6[i] * 6;
        result.vec7[i] = s.vec7[i] * 7;
        result.vec8[i] = s.vec8[i] * 8;
    }
    
    return result;
}