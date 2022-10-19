pub trait A: B {}
// { dg-error "trait cycle detected" "" { target *-*-* } .-1 }

pub trait B: A {}
// { dg-error "trait cycle detected" "" { target *-*-* } .-1 }
