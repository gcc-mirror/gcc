// { dg-do compile }
// { dg-skip-if "see https://github.com/Rust-GCC/gccrs/pull/1632" { *-*-darwin* } }
// { dg-options "-w -gdwarf-4 -dA" }
// 'char' should use DW_ATE_UTF
fn main() {
    let c = 'x';
    // Use -w to avoid warnings about the unused variables
    // DW_ATE_UTF entered in DWARF 4.
    // DW_ATE_UTF = 0x10
    // { dg-final { scan-assembler "0x10\[ \t]\[^\n\r]* DW_AT_encoding" } } */
}
