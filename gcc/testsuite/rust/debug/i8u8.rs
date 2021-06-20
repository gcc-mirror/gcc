// i8 and u8 types should not have the DWARF 'char' encoding.
fn main () {
    let x : i8 = 5;
    let y : u8 = 7;
// { dg-do compile }
// Use -w to avoid warnings about the unused variables
// { dg-options "-w -g -dA" }
// DW_ATE_signed_char = 6
// { dg-final { scan-assembler-not "0x6\[ \t]\[^\n\r]* DW_AT_encoding" } } */
// DW_ATE_unsigned_char = 8
// { dg-final { scan-assembler-not "0x8\[ \t]\[^\n\r]* DW_AT_encoding" } } */
}
