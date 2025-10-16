// Test: Function demonstrating register pressure with different ABI_VLEN

#include "../vls-cc-common.h"

void VLS_CC(ABI_VLEN) test_register_pressure_scenarios(int32x2_t small1, int32x2_t small2,
                                     int32x4_t medium1, int32x4_t medium2,
                                     int32x8_t large1) {
    // Different register consumption based on ABI_VLEN:
    // ABI_VLEN=128: 2 * 1 + 2 * 1 + 1 * 2 = 6 registers
    // ABI_VLEN=64:  2 * 1 + 2 * 2 + 1 * 4 = 10 registers (exceeds 8, some by reference)
    // ABI_VLEN=32:  2 * 2 + 2 * 4 + 1 * 8 = 20 registers (many by reference)
    
    // Just use the parameters to avoid warnings
    (void)small1; (void)small2; (void)medium1; (void)medium2; (void)large1;
}