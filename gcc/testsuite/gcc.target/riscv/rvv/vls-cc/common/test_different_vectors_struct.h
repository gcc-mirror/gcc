// Test: Struct with different sized vectors - uses hardware FP calling convention

#include "../vls-cc-common.h"

struct_different_vectors_t VLS_CC(ABI_VLEN) test_different_vectors_struct(struct_different_vectors_t s) {
    // Contains int32x2_t (64 bits) and int32x4_t (128 bits) - different lengths
    // Should use hardware floating-point calling convention, not vector registers
    return s;
}