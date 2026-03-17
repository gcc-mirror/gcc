// { dg-output "minus two!\r*\nelse\r*\n" }
#![feature(no_core)]
#![no_core]

extern "C" {
    fn printf(s: *const i8, ...);
}

fn foo(x: i32) {
    match x {
        -2 => {
            let a = "minus two!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
        _ => {
            let a = "else\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn main() -> i32 {
    foo(-2);
    foo(2);
    0
}
