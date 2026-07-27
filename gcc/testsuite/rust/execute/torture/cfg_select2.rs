// { dg-additional-options "-frust-compat-version=1.90 -frust-cfg=A=\"foo\"" }
// { dg-output "pass\r*\n" }
#![feature(no_core)]
#![no_core]

extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    cfg_select! {
        A = "foo" => {
            unsafe {
                let a = "pass\n\0";
                printf(a as *const str as *const i8);
            }
        }
        _ => {
            unsafe {
                let a = "fail\n\0";
                printf(a as *const str as *const i8);
            }
        }
    }
    return 0;
}
