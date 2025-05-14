// { dg-output "hello, include!\r*\nhello, include!\r*\nhello, include!\r*\n" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_str {
    () => {{}};
}

macro_rules! my_file {
    () => {"include.txt"};
}

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print(s: &str) {
    unsafe {
        printf(
            "%s" as *const str as *const i8,
            s as *const str as *const i8,
        );
    }
}

fn main() -> i32 {
    // include_str! (and include_bytes!) allow for an optional trailing comma.
    let my_str = include_str!("include.txt",);
    print(my_str);
    let my_str = include_str!(my_file!());
    print(my_str);
    let my_str = include_str!(my_file!(),);
    print(my_str);

    0
}
