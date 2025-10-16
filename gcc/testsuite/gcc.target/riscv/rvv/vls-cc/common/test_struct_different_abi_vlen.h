// Test: Struct behavior under different ABI_VLEN

#include "../vls-cc-common.h"

typedef struct {
    int32x4_t vec1;
    int32x4_t vec2;
} two_medium_vectors_t;

two_medium_vectors_t VLS_CC(ABI_VLEN) test_struct_different_abi_vlen(two_medium_vectors_t s) {
    // Struct with 2 * 128-bit vectors (256 bits total):
    // - ABI_VLEN=256: fits in single "vector tuple" concept, uses 2 registers
    // - ABI_VLEN=128: each vector uses 1 register, total 2 registers  
    // - ABI_VLEN=64:  each vector uses 2 registers, total 4 registers
    // - ABI_VLEN=32:  each vector uses 4 registers, total 8 registers
    return s;
}