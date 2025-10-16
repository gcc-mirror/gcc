// Test: Mixed scalar and vector arguments

#include "../vls-cc-common.h"

int VLS_CC(ABI_VLEN) test_mixed_args(int scalar1, int32x4_t vec1, float scalar2, int32x4_t vec2) {
    // scalars use integer/float registers, vectors use vector registers
    return scalar1 + (int)scalar2 + vec1[0] + vec2[0];
}