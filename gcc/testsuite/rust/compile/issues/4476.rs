#![feature(no_core)]
#![no_core]

struct Foo {}
struct S {}

fn main() {
    Foo { #[cfg(feature = -1)] .. } = S {};
    // { dg-error "attributes are not allowed before .* in a struct expression" "" { target *-*-* } .-1 }
}
