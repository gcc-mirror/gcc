// Test: Struct with five 256-bit vectors
// Each int32x8_t is 256 bits (8 x 32-bit elements)
// Total: 5 vectors * 256 bits = 1280 bits
// With ABI_VLEN=128: 5 vectors * 2 registers each = 10 registers (fits in 16 available)

#include "../vls-cc-common.h"

typedef struct {
    int32x8_t vec1;  // 256-bit vector
    int32x8_t vec2;  // 256-bit vector
    int32x8_t vec3;  // 256-bit vector
    int32x8_t vec4;  // 256-bit vector
    int32x8_t vec5;  // 256-bit vector
} five_256bit_vectors_struct_t;

five_256bit_vectors_struct_t VLS_CC(ABI_VLEN) test_struct_five_256bit_vectors(five_256bit_vectors_struct_t s) {
    // Should be flattened and passed as separate vector arguments
    // With ABI_VLEN=128: vec1 uses v8+v9, vec2 uses v10+v11, vec3 uses v12+v13, 
    //                    vec4 uses v14+v15, vec5 uses v16+v17
    // Total uses 10 out of 16 available vector argument registers
    five_256bit_vectors_struct_t result;
    
    // Process each 256-bit vector: add different values to each vector
    for (int i = 0; i < 8; i++) {
        result.vec1[i] = s.vec1[i] + 1000;
        result.vec2[i] = s.vec2[i] + 2000;
        result.vec3[i] = s.vec3[i] + 3000;
        result.vec4[i] = s.vec4[i] + 4000;
        result.vec5[i] = s.vec5[i] + 5000;
    }
    
    return result;
}