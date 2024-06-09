// { dg-output "Named variadic" }

extern "C" {
    fn printf(fmt: *const i8, variadic: ...);
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
    print("Named variadic");

    0
}
