#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    unsafe {
        asm!("nop", clobber_abi+);  // { dg-error "expected '\\(', found '\\+'" }
    }
}