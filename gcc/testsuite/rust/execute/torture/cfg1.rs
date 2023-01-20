// { dg-additional-options "-w -frust-cfg=A" }
// { dg-output "test1\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

#[cfg(A)]
fn test() {
    unsafe {
        let a = "test1\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c);
    }
}

#[cfg(B)]
fn test() {
    unsafe {
        let a = "test2\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c);
    }
}

fn main() -> i32 {
    test();
    0
}
