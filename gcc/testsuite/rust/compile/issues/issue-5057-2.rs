#![feature(no_core)]
#![no_core]
#![feature(rustc_attrs)]

// Rust-GCC/gccrs#5057: a placeholder past the last operand is an error,
// not something to hand gcc

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

fn main() {
    let y: i32 = 9;
    let mut x: i32 = 0;
    unsafe {
        asm!("mov {2}, {}", in(reg) y, out(reg) x); // { dg-error "invalid reference to argument at index 2" }
    }
}
