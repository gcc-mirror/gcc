// { dg-do compile }
// { dg-options "-O -std=c99 -gdwarf-3 -dA" }
// DW_LANG_C99 = 0x000c
// { dg-final { scan-assembler "0xc.*DW_AT_language" } } */

int version;
