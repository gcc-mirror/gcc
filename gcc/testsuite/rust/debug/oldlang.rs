fn main () {
// { dg-do compile }
// { dg-options "-gstrict-dwarf -gdwarf-3 -dA" }
// Strict DWARF < 5 uses DW_LANG_C = 0x0002
// { dg-final { scan-assembler "0x2\[ \t]\[^\n\r]* DW_AT_language" } } */
}
