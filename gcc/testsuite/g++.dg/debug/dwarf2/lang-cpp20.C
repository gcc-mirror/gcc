// { dg-do compile }
// { dg-options "-O -std=c++20 -gdwarf-5 -dA" }
// { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } }
// For -gdwarf-6 hopefully DW_LANG_C_plus_plus_20
// DW_LANG_C_plus_plus_14 = 0x0021
// { dg-final { scan-assembler "0x21\[^\n\r]* DW_AT_language" } } */

int version;
