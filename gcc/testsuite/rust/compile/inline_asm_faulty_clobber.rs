#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    unsafe {
        asm!("nop", clobber_abi());  // { dg-error "at least one abi must be provided as an argument to 'clobber_abi'" }
    }
}