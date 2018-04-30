// { dg-do compile }
// { dg-options "-O -std=c++11 -gdwarf-5 -dA" }
// DW_LANG_C_plus_plus_11 = 0x001a
// { dg-final { scan-assembler "0x1a\[^\n\r]* DW_AT_language" } } */

int version;
