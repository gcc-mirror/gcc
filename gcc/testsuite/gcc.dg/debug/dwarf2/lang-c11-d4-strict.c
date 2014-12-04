// { dg-do compile }
// { dg-options "-O -std=c11 -gdwarf-4 -gstrict-dwarf -dA" }
// We cannot produce DW_LANG_C11 = 0x001d because strict-dwarf.
// So expect DW_LANG_C99 = 0x000c
// { dg-final { scan-assembler "0xc.*DW_AT_language" } } */

int version;
