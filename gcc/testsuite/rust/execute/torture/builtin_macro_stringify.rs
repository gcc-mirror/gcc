// { dg-output "a! ()" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! stringify {
    () => {};
}

macro_rules! a {
    () => {
        " foo"
    };
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
    let a = stringify!(a!());

    print(a);

    0
}
