struct Foo<T>(T, bool);

impl<X, Y> Foo<X> {
    // { dg-error "unconstrained type parameter" "" { target *-*-* } .-1 }
    fn test() -> Y {
        123
    }
}

fn main() {
    let a = Foo::test();
    // { dg-error "Failed to resolve expression of function call" "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
}
