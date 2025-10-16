// Test: Struct with four 256-bit vectors
// Each int32x8_t is 256 bits (8 x 32-bit elements)
// Total: 4 vectors * 256 bits = 1024 bits
// With ABI_VLEN=128: 4 vectors * 2 registers each = 8 registers (fits in 16 available)

#include "../vls-cc-common.h"

typedef struct {
    int32x8_t vec1;  // 256-bit vector
    int32x8_t vec2;  // 256-bit vector
    int32x8_t vec3;  // 256-bit vector
    int32x8_t vec4;  // 256-bit vector
} four_256bit_vectors_struct_t;

four_256bit_vectors_struct_t VLS_CC(ABI_VLEN) test_struct_four_256bit_vectors(four_256bit_vectors_struct_t s) {
    // Should be flattened and passed as separate vector arguments
    // With ABI_VLEN=128: vec1 uses v8+v9, vec2 uses v10+v11, vec3 uses v12+v13, vec4 uses v14+v15
    // With ABI_VLEN=256: vec1 uses v8, vec2 uses v9, vec3 uses v10, vec4 uses v11
    four_256bit_vectors_struct_t result;
    
    // Process each 256-bit vector: add 100 to first element, add 200 to second element, etc.
    for (int i = 0; i < 8; i++) {
        result.vec1[i] = s.vec1[i] + 100;
        result.vec2[i] = s.vec2[i] + 200;
        result.vec3[i] = s.vec3[i] + 300;
        result.vec4[i] = s.vec4[i] + 400;
    }
    
    return result;
}