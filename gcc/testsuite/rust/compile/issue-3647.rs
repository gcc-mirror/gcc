#![allow(dead_code)]
type A = fn();

type B = for<'static> fn();
// { dg-error "invalid lifetime parameter name: .static. .E0262." "" { target *-*-* } .-1 }

pub fn main() {}
