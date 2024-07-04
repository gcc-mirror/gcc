/* { dg-output "S::f\r*\nT1::f\r*\nT2::f\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

struct S;

impl S {
    fn f() {
        unsafe {
            let a = "S::f\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
    }
}

trait T1 {
    fn f() {
        unsafe {
            let a = "T1::f\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
    }
}
impl T1 for S {}

trait T2 {
    fn f() {
        unsafe {
            let a = "T2::f\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
    }
}
impl T2 for S {}

fn main() -> i32 {
    S::f();
    <S as T1>::f();
    <S as T2>::f();

    0
}
