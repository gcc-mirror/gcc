// { dg-do compile { target c++20 } }
// { dg-options { -gdwarf -dA } }

// Test that all three use DW_ATE_UTF.
// { dg-final { scan-assembler-times {DW_AT_encoding \(0x10\)} 3 } }

char8_t c8;
char16_t c16;
char32_t c32;
