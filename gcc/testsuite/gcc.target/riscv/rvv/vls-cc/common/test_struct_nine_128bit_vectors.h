// Test: Struct with nine 128-bit vectors
// Each int32x4_t is 128 bits (4 x 32-bit elements)
// Total: 9 vectors * 128 bits = 1152 bits
// With ABI_VLEN=128: 9 vectors * 1 register each = 9 registers (fits in 16 available)

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
    int32x4_t vec9;  // 128-bit vector
} nine_128bit_vectors_struct_t;

nine_128bit_vectors_struct_t VLS_CC(ABI_VLEN) test_struct_nine_128bit_vectors(nine_128bit_vectors_struct_t s) {
    // Should be flattened and passed as separate vector arguments
    // With ABI_VLEN=128: vec1 uses v8, vec2 uses v9, ..., vec9 uses v16
    // Total uses 9 out of 16 available vector argument registers
    nine_128bit_vectors_struct_t result;
    
    // Process each 128-bit vector: add vector index * 100 to each element
    for (int i = 0; i < 4; i++) {
        result.vec1[i] = s.vec1[i] + 100;
        result.vec2[i] = s.vec2[i] + 200;
        result.vec3[i] = s.vec3[i] + 300;
        result.vec4[i] = s.vec4[i] + 400;
        result.vec5[i] = s.vec5[i] + 500;
        result.vec6[i] = s.vec6[i] + 600;
        result.vec7[i] = s.vec7[i] + 700;
        result.vec8[i] = s.vec8[i] + 800;
        result.vec9[i] = s.vec9[i] + 900;
    }
    
    return result;
}