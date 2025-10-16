// Test: Simple union with vector - should use integer calling convention

#include "../vls-cc-common.h"

union_vector_t VLS_CC(ABI_VLEN) test_simple_union(union_vector_t u) {
    // Union with fixed-length vector should always use integer calling convention
    // Should NOT use vector registers, even though it contains a vector
    return u;
}