#[lang = "sized"]
pub trait Sized {}

fn test<T>(a: T) -> T {
    a
}

fn main() {
    let a: i32 = test(123);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b: i32 = test(456);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
