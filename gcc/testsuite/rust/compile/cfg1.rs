// { dg-additional-options "-w" }
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

fn main() {
    test();
    // { dg-error "Cannot find path .test. in this scope" "" { target *-*-* } .-1 }
}
