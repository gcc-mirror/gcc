// 'char' should use DW_ATE_UTF
fn main () {
    let c = 'x';
// { dg-do compile }
// Use -w to avoid warnings about the unused variables
// DW_ATE_UTF entered in DWARF 4.
// { dg-options "-w -gdwarf-4 -dA" }
// DW_ATE_UTF = 0x10
// { dg-final { scan-assembler "0x10\[^\n\r]* DW_AT_encoding" } } */
}
