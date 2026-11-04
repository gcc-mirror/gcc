/* { dg-do run { target x86_64*-*-* } } */
/* { dg-output "9\r*\n" } */
#![feature(no_core)]
#![no_core]
#![feature(rustc_attrs)]

// Rust-GCC/gccrs#5057: an input listed before an output got the output's
// operand number, so this moved x into y

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    let y: i32 = 9;
    let mut x: i32 = 0;
    unsafe {
        asm!("mov {}, {}", in(reg) y, out(reg) x);
        printf("%d\n\0" as *const str as *const i8, x);
    }
    0
}
