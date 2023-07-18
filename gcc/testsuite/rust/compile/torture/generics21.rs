#[lang = "sized"]
pub trait Sized {}

fn callee<T>(t: &T) -> i32 {
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    32
}

fn caller(t: i32) -> i32 {
    callee(&t)
}

fn main() {
    let a;
    a = caller(123);
}
