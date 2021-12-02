#include <stdint.h>

int testCppI8Mangle (int8_t,  uint8_t,  int_least8_t,  uint_least8_t,  int_fast8_t,  uint_fast8_t)  { return 1; }
int testCppI16Mangle(int16_t, uint16_t, int_least16_t, uint_least16_t, int_fast16_t, uint_fast16_t) { return 2; }
int testCppI32Mangle(int32_t, uint32_t, int_least32_t, uint_least32_t, int_fast32_t, uint_fast32_t) { return 3; }
int testCppI64Mangle(int64_t, uint64_t, int_least64_t, uint_least64_t, int_fast64_t, uint_fast64_t) { return 4; }
int testCppIntPtrMangle(intptr_t, uintptr_t) { return 5; }
int testCppIntMaxMangle(intmax_t, uintmax_t) { return 6; }
