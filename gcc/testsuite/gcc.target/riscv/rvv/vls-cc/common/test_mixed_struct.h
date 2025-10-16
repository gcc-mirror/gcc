// Test: Struct with mixed vector and scalar - uses hardware FP calling convention

#include "../vls-cc-common.h"

struct_mixed_t VLS_CC(ABI_VLEN) test_mixed_struct(struct_mixed_t s) {
    // Contains vector and scalar - should use hardware FP calling convention
    return s;
}