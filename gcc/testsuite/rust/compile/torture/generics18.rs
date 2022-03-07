struct Foo<T>(T);

impl<X> Foo<X> {
    fn new(a: X) -> Self {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        Self(a)
    }

    fn test(self) -> X {
        self.0
    }
}

fn main() {
    let a;
    a = Foo(123);

    let b = a.test();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
