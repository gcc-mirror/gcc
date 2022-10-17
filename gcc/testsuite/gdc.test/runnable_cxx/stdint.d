// EXTRA_CPP_SOURCES: stdint.cpp

module stdint_test;

import core.stdc.stdint;

extern(C++):

int testCppI8Mangle (int8_t,  uint8_t,  int_least8_t,  uint_least8_t,  int_fast8_t,  uint_fast8_t);
int testCppI16Mangle(int16_t, uint16_t, int_least16_t, uint_least16_t, int_fast16_t, uint_fast16_t);
int testCppI32Mangle(int32_t, uint32_t, int_least32_t, uint_least32_t, int_fast32_t, uint_fast32_t);
int testCppI64Mangle(int64_t, uint64_t, int_least64_t, uint_least64_t, int_fast64_t, uint_fast64_t);
int testCppIntPtrMangle(intptr_t, uintptr_t);
int testCppIntMaxMangle(intmax_t, uintmax_t);

void main()
{
    assert(testCppI8Mangle (1, 2, 3, 4, 5, 6) == 1);
    assert(testCppI16Mangle(1, 2, 3, 4, 5, 6) == 2);
    assert(testCppI32Mangle(1, 2, 3, 4, 5, 6) == 3);
    assert(testCppI64Mangle(1, 2, 3, 4, 5, 6) == 4);
    assert(testCppIntPtrMangle(1, 2) == 5);
    assert(testCppIntMaxMangle(1, 2) == 6);
}
