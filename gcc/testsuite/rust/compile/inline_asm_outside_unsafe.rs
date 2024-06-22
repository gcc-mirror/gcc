#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    asm!("nop"); // { dg-error "use of inline assembly is unsafe and requires unsafe function or block" }
}

