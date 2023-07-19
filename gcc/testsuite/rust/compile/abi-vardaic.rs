// https://doc.rust-lang.org/error_codes/E0045.html
#![allow(unused)]
fn main() {
    extern "Rust" {
        fn foo(x: u8, ...); // { dg-error "C-variadic function must have C or cdecl calling convention" }
    }
}