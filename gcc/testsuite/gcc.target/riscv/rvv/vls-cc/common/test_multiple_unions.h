// Test: Multiple union arguments

#include "../vls-cc-common.h"

union_vector_t VLS_CC(ABI_VLEN) test_multiple_unions(union_vector_t u1, union_vector_t u2, union_vector_t u3) {
    // All should use integer calling convention
    // Should consume integer registers, not vector registers
    union_vector_t result;
    result.scalars[0] = u1.scalars[0] + u2.scalars[0] + u3.scalars[0];
    result.scalars[1] = u1.scalars[1] + u2.scalars[1] + u3.scalars[1];
    result.scalars[2] = u1.scalars[2] + u2.scalars[2] + u3.scalars[2];
    result.scalars[3] = u1.scalars[3] + u2.scalars[3] + u3.scalars[3];
    return result;
}