// Test: Struct with vector array - should be passed as vector tuple

#include "../vls-cc-common.h"

struct_vector_array_t VLS_CC(ABI_VLEN) test_vector_array_struct(struct_vector_array_t s) {
    // Array of 2 int32x4_t vectors, should be passed as vector tuple
    // Total size: 256 bits, should use 2 vector registers
    return s;
}