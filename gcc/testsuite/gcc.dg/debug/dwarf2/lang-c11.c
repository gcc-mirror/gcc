// { dg-do compile }
// { dg-options "-O -std=c11 -gdwarf-5 -dA" }
// DW_LANG_C11 = 0x001d
// { dg-final { scan-assembler "0x1d.*DW_AT_language" } } */

int version;
