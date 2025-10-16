// Test: Function pointers with mixed signatures

#include "../vls-cc-common.h"

typedef int (*mixed_func_ptr_t)(int, int32x4_t, float) VLS_CC(ABI_VLEN);

// Helper function to be called via function pointer
int VLS_CC(ABI_VLEN) helper_mixed_function(int i, int32x4_t v, float f) {
    return i + v[0] + (int)f;
}

int VLS_CC(ABI_VLEN) test_call_mixed_function(mixed_func_ptr_t func, int i, int32x4_t v, float f) {
    return func(i, v, f);
}