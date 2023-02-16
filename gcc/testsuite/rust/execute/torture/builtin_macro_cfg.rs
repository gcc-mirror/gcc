// { dg-additional-options "-w -frust-cfg=A" }
// { dg-output "A\r*\n" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! cfg {
    () => {{}};
}

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print(s: &str) {
    unsafe {
        printf(
            "%s\n\0" as *const str as *const i8,
            s as *const str as *const i8,
        );
    }
}

fn main() -> i32 {
    let cfg = cfg!(A);
    if cfg {
        print("A\0");
    }
    let cfg = cfg!(B);
    if cfg {
        print("B\0");
    }

    0
}
