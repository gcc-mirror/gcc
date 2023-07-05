// { dg-do compile { target c++20 } }
// { dg-options { -gdwarf-4 -dA } }

// Test that all three use DW_ATE_UTF.
// This test uses -gdwarf-4 since in DWARF5 optimize_implicit_const
// would optimize the output from:
//   .byte   0x10    # DW_AT_encoding
// into:
//                   # DW_AT_encoding (0x10)
// { dg-final { scan-assembler-times "0x10\[ \t]\[^\n\r]* DW_AT_encoding" 3 } }

char8_t c8;
char16_t c16;
char32_t c32;
