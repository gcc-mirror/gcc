#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    unsafe {
        asm!(i_am_a_dummy); // { dg-error "asm template must be a string literal" }
    }
}