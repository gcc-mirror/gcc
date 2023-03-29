// { dg-additional-options "-w -frust-cfg=A" }
// { dg-output "A\n" }
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
            "%s\n" as *const str as *const i8,
            s as *const str as *const i8,
        );
    }
}

fn main() -> i32 {
    let cfg = cfg!(A);
    if cfg {
        print("A");
    }
    let cfg = cfg!(B);
    if cfg {
        print("B");
    }

    0
}
