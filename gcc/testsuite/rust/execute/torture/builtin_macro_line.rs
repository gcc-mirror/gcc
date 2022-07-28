// { dg-output "17\n20\n" }
extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print(s: u32) {
    unsafe {
        printf("%u\n\0" as *const str as *const i8, s);
    }
}

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
