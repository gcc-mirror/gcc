/* { dg-do run { target x86_64*-*-* } } */
/* { dg-output "5\r*\n9\r*\n" }*/

#![feature(rustc_attrs)]
#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    let mut x: i32 = 0;
    let mut _y: i32 = 9; // Mark it as _y since it is only used as input operand, not printing

    unsafe {
        asm!(
            "mov $5, {}",
            out(reg) x
        );
        printf("%d\n\0" as *const str as *const i8, x);
    };

    unsafe {
        asm!(
            "mov {}, {}",
            in(reg) _y,
            out(reg) x,
        );
        printf("%d\n\0" as *const str as *const i8, x);
    }
    0
}
