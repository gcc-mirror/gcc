// Test: Vector and scalar struct members (advanced mixed struct)

#include "../vls-cc-common.h"

typedef struct {
    int scalar;
    int32x4_t vector;
    float fp_scalar;
} mixed_struct_advanced_t;

mixed_struct_advanced_t VLS_CC(ABI_VLEN) test_mixed_struct_advanced(mixed_struct_advanced_t s) {
    // Should use hardware FP calling convention due to mixed types
    // Vector part will be treated according to standard struct rules
    return s;
}