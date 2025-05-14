/* { dg-do run { target arm*-*-* } } */
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
    let mut _x: i32 = 0;
    let mut _y: i32 = 9;

    unsafe {
        asm!(
            "mov {}, 5",
            out(reg) _x
        );
        printf("%d\n\0" as *const str as *const i8, _x);
    };

    unsafe {
        asm!(
            "mov {}, {}",
            in(reg) _y,
            out(reg) _x
        );
        printf("%d\n\0" as *const str as *const i8, _x);
    }

    0
}
