// { dg-do compile }
// { dg-options "-O -std=c++98 -gdwarf-2 -dA" }
// DW_LANG_C_plus_plus = 0x0004
// { dg-final { scan-assembler "0x4\[^\n\r]* DW_AT_language" } } */

int version;
