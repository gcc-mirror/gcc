// { dg-output "20\r*\n23\r*\n" }
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
