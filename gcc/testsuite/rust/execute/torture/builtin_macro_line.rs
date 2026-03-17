// { dg-output "22\r*\n25\r*\n" }
#![feature(no_core)]
#![no_core]
#![feature(rustc_attrs)]

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print(s: u32) {
    unsafe {
        printf("%u\n\0" as *const str as *const i8, s);
    }
}

#[rustc_builtin_macro]
macro_rules! line {
    () => {{}};
}

fn main() -> i32 {
    let a = line!();
    print(a);

    let b = line!();
    print(b);

    0
}
