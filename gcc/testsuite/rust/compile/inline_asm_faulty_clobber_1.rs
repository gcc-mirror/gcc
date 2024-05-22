#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    asm!("nop", clobber_abi);  // { dg-error "expected `\\(`, found end of macro arguments" }
}