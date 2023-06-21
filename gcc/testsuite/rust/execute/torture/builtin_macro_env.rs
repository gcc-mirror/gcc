// { dg-output "VALUE\r*\nVALUE\r*\n" }
// { dg-set-compiler-env-var ENV_MACRO_TEST "VALUE" }
#[rustc_builtin_macro]
macro_rules! env {
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
    let val0 = env!("ENV_MACRO_TEST");

    print(val0);

    let val1 = env!("ENV_MACRO_TEST",);

    print(val1);

    0
}
