// Test: Vector passed in two registers (<= 2 * ABI_VLEN bits)
// int32x8_t (256 bits) should use 2 registers when ABI_VLEN = 128

#include "../vls-cc-common.h"

int32x8_t VLS_CC(ABI_VLEN) test_two_registers(int32x8_t vec, int32x8_t vec2) {
    return vec + vec2;
}