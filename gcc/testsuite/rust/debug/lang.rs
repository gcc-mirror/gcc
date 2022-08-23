fn main () {
// { dg-do compile }
// { dg-options "-gdwarf-5 -dA" }
// DW_LANG_Rust is 0x1c
// { dg-final { scan-assembler "0x1c\[ \t]\[^\n\r]* DW_AT_language" } } */
}
