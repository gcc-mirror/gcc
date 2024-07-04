// { dg-output "123\r*\n" }
#[lang = "sized"]
pub trait Sized {}

trait A {
    fn get_int(&self) -> i32;
}

impl A for i32 {
    fn get_int(&self) -> i32 {
        *self
    }
}

fn get_dyn_a(x: &i32) -> &dyn A {
    x
}

extern "C" {
    fn printf(s: *const i8, ...) -> i32;
}

fn main() -> i32 {
    let x = 123;
    let y = get_dyn_a(&x);
    let value = y.get_int();
    let fmt_string = "%d\n\0" as *const str as *const i8;
    unsafe {
        printf(fmt_string, value);
    }
    return 0;
}
