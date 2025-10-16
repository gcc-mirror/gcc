// Test: Struct with single vector - should be flattened

#include "../vls-cc-common.h"

struct_single_vector_t VLS_CC(ABI_VLEN) test_single_vector_struct(struct_single_vector_t s) {
    // Should be flattened and passed as single vector argument
    return s;
}