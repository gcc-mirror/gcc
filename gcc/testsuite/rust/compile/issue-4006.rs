#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

pub fn main() {
    asm!(
        "xor eax, eax"
        "xor eax, eax");
    // { dg-error "expected token .;." "" { target *-*-* } .-1 }
}
