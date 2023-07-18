#[lang = "sized"]
pub trait Sized {}

struct Foo(i32);

trait A {
    const A: i32 = 123;
    fn B();
    fn C(&self);
}

trait B: A {
    fn test(&self);
}

pub fn main() {
    let a;
    a = Foo(123);

    let b: &dyn B = &a;
    // { dg-error "trait bound is not object safe" "" { target *-*-* } .-1 }
}
