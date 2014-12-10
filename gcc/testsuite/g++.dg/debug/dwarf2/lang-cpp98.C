// { dg-do compile }
// { dg-options "-O -std=c++98 -g -dA" }
// DW_LANG_C_plus_plus = 0x0004
// { dg-final { scan-assembler "0x4.*DW_AT_language" } } */

int version;
