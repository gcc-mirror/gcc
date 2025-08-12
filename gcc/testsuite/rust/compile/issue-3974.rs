impl<'a, F> RunUntil<'a, F> {
    // { dg-error "could not resolve type path" "" { target *-*-* } .-1 }
    fn project<'pin>() -> Projection<'pin, 'a, F> {
        // { dg-error "could not resolve type path" "" { target *-*-* } .-1 }
        Self!()
        // { dg-error "could not resolve macro invocation" "" { target *-*-* } .-1 }
    }
}
