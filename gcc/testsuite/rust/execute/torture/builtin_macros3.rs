// { dg-output "14\r*\n42\r*\n" }
#[rustc_builtin_macro]
macro_rules! column {
    () => {{}};
}

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print(s: u32) {
    unsafe {
        printf("%u\n\0" as *const str as *const i8, s);
    }
}

fn main() -> i32 {
    let c0 = column!();

    print(c0);

    let c1 = /* --------------------- */ column!();

    print(c1);

    0
}
