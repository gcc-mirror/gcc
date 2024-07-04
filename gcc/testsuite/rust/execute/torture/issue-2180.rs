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
    x as &dyn A
}

fn clobber_stack() {
    let _z: [usize; 8] = [1, 2, 3, 4, 5, 6, 7, 8];
}

extern "C" {
    fn printf(s: *const i8, ...) -> i32;
}

fn main() -> i32 {
    let x = 123;
    let y = get_dyn_a(&x);
    clobber_stack();
    let value = y.get_int();
    let fmt_string = "%d\n\0" as *const str as *const i8;
    unsafe {
        printf(fmt_string, value);
    }
    return 0;
}
