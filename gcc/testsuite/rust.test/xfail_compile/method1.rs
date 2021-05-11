// { dg-excess-errors "Noisy error and debug" }
struct Foo(i32);

impl Foo {
    fn test() {}
}

pub fn main() {
    let a;
    a = Foo(123);

    a.test()
    // { dg-error "associated function is not a method" "" { target *-*-* } .-1 }
}
