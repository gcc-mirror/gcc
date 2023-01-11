// { dg-output "1\r*\n" }
#[rustc_builtin_macro]
macro_rules! concat {
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
    let mut x = concat!("x");
    x = concat!("y");
    if x == "y" {
        print(1);
    }

    0
}
