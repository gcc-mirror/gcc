// Test: Compare struct vs union behavior

#include "../vls-cc-common.h"

// Struct equivalent to union (should use vector calling convention)
typedef struct {
    int32x4_t vec;
} equivalent_struct_t;

equivalent_struct_t VLS_CC(ABI_VLEN) test_equivalent_struct(equivalent_struct_t s) {
    // This struct should use vector calling convention (flattened)
    // Compare behavior with the union version
    return s;
}