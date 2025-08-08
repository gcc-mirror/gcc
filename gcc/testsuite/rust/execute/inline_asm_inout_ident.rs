/* { dg-do run { target x86_64*-*-* } } */
/* { dg-output "Value is: 5\r*\n" } */
#![feature(rustc_attrs)]

extern "C" {
    fn printf(s: *const i8, ...);
}

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

fn main() -> i32 {
    let x: i32;
    // `inout` can also move values to different places
    unsafe {
        asm!("inc {}", inout(reg) 4u64=>x);
    }
    unsafe {
        printf("Value is: %i\n\0" as *const str as *const i8, x);
    }
    0
}
