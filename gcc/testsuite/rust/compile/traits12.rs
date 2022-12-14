trait A<T> {
    type Output;

    fn test(self, a: &T) -> &Self::Output;
}

struct Foo<T> {
    start: T,
    end: T,
}

impl<T> A for Foo<usize> {
    // { dg-error "generic item takes at least 1 type arguments but 0 were supplied" "" { target *-*-* } .-1 }
    // { dg-error "unconstrained type parameter" "" { target *-*-* } .-2 }
    type Output = T;

    fn test(self, a: &T) -> &Self::Output {
        a
    }
}
