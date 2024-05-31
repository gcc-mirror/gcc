#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    unsafe {
        asm!("nop", options(nomem, nomem)); // { dg-error "the 'nomem' option was already provided" }
        asm!("nop", options(noreturn, noreturn)); // { dg-error "the 'noreturn' option was already provided" }
    }
}