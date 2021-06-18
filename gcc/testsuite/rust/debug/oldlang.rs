fn main () {
// { dg-do compile }
// { dg-options "-gstrict-dwarf -gdwarf-3 -dA" }
// DW_LANG_Rust_old is 0x9000
// { dg-final { scan-assembler "0x9000\[^\n\r]* DW_AT_language" } } */
}
