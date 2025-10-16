// Test: Struct with two same-length vectors - should be passed as vector tuple

#include "../vls-cc-common.h"

struct_two_same_vectors_t VLS_CC(ABI_VLEN) test_same_vectors_struct(struct_two_same_vectors_t s) {
    // Should be passed in vector registers like a vector tuple
    // Both vectors are int32x4_t (128 bits each), total 256 bits
    // Should use 2 vector registers when ABI_VLEN = 128
    return s;
}