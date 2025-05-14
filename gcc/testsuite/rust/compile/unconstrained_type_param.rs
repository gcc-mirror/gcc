#[lang = "sized"]
pub trait Sized {}

struct Foo<T>(T, bool);

impl<X, Y> Foo<X> {
    // { dg-error "unconstrained type parameter" "" { target *-*-* } .-1 }
    fn test() -> Y {
        123
    }
}

fn main() {
    let a = Foo::test();
    // { dg-error "expected" "" { target *-*-* } .-1 }
}
