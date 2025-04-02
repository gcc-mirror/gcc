pub trait TypeFn {}

impl TypeFn for Output<{ 42 }> {
    // { dg-error "could not resolve type path .Output. .E0412." "" { target *-*-* } .-1 }
    type Output = ();
}
