/* { dg-output "Value is: 42\r*\n" } */
#![feature(rustc_attrs)]

extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

#[rustc_builtin_macro]
macro_rules! llvm_asm {
    () => {};
}

pub fn black_box<T>(mut dummy: T) -> T {
    unsafe {
        llvm_asm!("" : : "r"(&mut dummy) : "memory" : "volatile");
    }

    dummy
}

fn main() {
    let dummy: i32 = 42;
    let result = black_box(dummy);
    unsafe {
        printf("Value is: %i\n\0" as *const str as *const i8, result);
    }
}
