#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    unsafe {
        asm!("nop", options(nomem, nomem)); // { dg-error "the 'nomem' option was already provided" }
        asm!("nop", options(noreturn, noreturn)); // { dg-error "the 'noreturn' option was already provided" }
        asm!("nop", options(xxx)); // { dg-error "expected one of '\\)', 'att_syntax', 'may_unwind', 'nomem', 'noreturn', 'nostack', 'preserves_flags', 'pure', 'raw', or 'readonly', found 'xxx'" }
        asm!("nop", options(+)); // { dg-error "expected one of '\\)', 'att_syntax', 'may_unwind', 'nomem', 'noreturn', 'nostack', 'preserves_flags', 'pure', 'raw', or 'readonly', found '\\+'" }
        asm!("nop", options+); // { dg-error "expected '\\(', found '\\+'" }
    }
}