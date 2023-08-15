#[lang = "sized"]
pub trait Sized {}

pub trait A: B {}
// { dg-error "cycle detected when computing the super predicates of .A." "" { target *-*-* } .-1 }

pub trait B: A {}
// { dg-error "cycle detected when computing the super predicates of .B." "" { target *-*-* } .-1 }
