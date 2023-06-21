// { dg-additional-options "-w -frust-cfg=A" }
// { dg-output "test1\r*\ntest2\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

struct Foo(i32);
impl Foo {
    #[cfg(A)]
    fn test(&self) {
        unsafe {
            let a = "test1\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
    }

    #[cfg(not(B))]
    fn test2(&self) {
        unsafe {
            let a = "test2\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
    }
}

fn main() -> i32 {
    let a = Foo(123);
    a.test();
    a.test2();

    0
}
