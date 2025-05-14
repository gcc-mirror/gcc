// { dg-output "gcc\n\nrs\n" }

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn main() -> i32 {
    let a = "gcc

rs\0";

    unsafe { printf("%s\n\0" as *const str as *const i8, a as *const str as *const i8); }

    0
}
