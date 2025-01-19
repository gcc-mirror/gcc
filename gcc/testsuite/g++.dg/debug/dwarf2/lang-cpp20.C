// { dg-do compile }
// { dg-options "-O -std=c++20 -gdwarf-5 -dA -gno-strict-dwarf" }
// { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } }
// DW_LANG_C_plus_plus_14 = 0x0021
// DW_LNAME_C_plus_plus = 0x0004 202002
// { dg-final { scan-assembler "0x21\[^\n\r]* DW_AT_language" } } */
// { dg-final { scan-assembler "0x4\[^\n\r]* DW_AT_language_name" } } */
// { dg-final { scan-assembler "0x31512\[^\n\r]* DW_AT_language_version" } } */

int version;
